(ns dj.dispatch.treefn
"
(let [args {:x 1, :y 2}
      fms {:add (fm [:x :y]
                    (+ x y))}
      tf (treefm fms :add)]
  (tf args))
"
  (:require [clojure.set :as cs]
            [dj.algorithms.dag :as dag]))

(defmacro fm
  "adds dependency metadata to fn, uses map destructuring
  assume you provide the vector in {:keys [a b ...]}
  "
  [bindings & body]
  `(with-meta (fn [{:keys ~bindings}]
                ~@body)
     {::dependencies '~(mapv keyword bindings)}))

(defn treefm
  "shakes fm-map with root-key and produces a treefn but with fm instead of fn, which uses map destructuring

  Internally, maps are being passed around to the functions"
  [fn-map root-key]
  (let [available-keys (set (keys fn-map))
        ;; set of keys
        shaken-keys ((fn collect [all-dependents temp-key]
                       (let [the-fn (temp-key fn-map)
                             dependents (-> the-fn
                                            meta
                                            :dj.dispatch.treefn/dependencies
                                            set)]
                         (if (empty? dependents)
                           (conj all-dependents
                                 temp-key)
                           (reduce collect
                                   (cs/union dependents all-dependents)
                                   dependents))))
                     #{root-key}
                     root-key)
        ;; set of keys
        input-key-set (cs/difference shaken-keys available-keys)
        ;; map of keys->fms that will be called
        shaken-map (select-keys fn-map shaken-keys) ; select-keys automatically disregards keys 
        ;; map of keys->keys (also for analysis, contains topological information)
        shaken-dag-with-inputs (reduce-kv (fn [ret k the-fn]
                                            (let [dependents (-> the-fn
                                                                 meta
                                                                 :dj.dispatch.treefn/dependencies)]
                                              (assoc ret
                                                k
                                                (set dependents))))
                                          {}
                                          shaken-map)
        ;; map of keys->keys (remove input-keys so we call only functions)
        shaken-dag (reduce-kv (fn [ret k dependents]
                                (assoc ret
                                  k
                                  (cs/difference dependents
                                                 input-key-set)))
                              {}
                              shaken-dag-with-inputs)
        ;; vector of keys of fms we will call
        sorted-keys (dag/topological-sort shaken-dag)]
    (with-meta (fn [partial-val-map]
                 (let [provided-input-key-set (set (keys partial-val-map))]
                   (when-not (empty? (cs/difference input-key-set provided-input-key-set))
                     (throw (ex-info "provided input keys does not cover required inputs"
                                     {:required-input-key-set input-key-set
                                      :provided-input-key-set provided-input-key-set
                                      :missing-input-key-set (cs/difference input-key-set provided-input-key-set)}))))
                 ;; would like to use transients but 'contains?' bug CLJ-700
                 (let [timings (atom {})]
                   (with-meta
                     (reduce (fn [val-map the-fn-key]
                               (let [start (System/nanoTime)]
                                 ;; we can reuse previously computed values
                                 (if (contains? val-map the-fn-key)
                                   val-map
                                   (let [the-fn (shaken-map the-fn-key)
                                         ret (assoc val-map
                                                    the-fn-key
                                                    (try
                                                      (the-fn val-map)
                                                      (catch Exception e
                                                        (throw (ex-info "treefn node error"
                                                                        {:val-map val-map
                                                                         :the-fn the-fn
                                                                         :shaken-map shaken-map
                                                                         :the-fn-key the-fn-key}
                                                                        e)))))]
                                     (swap! timings assoc the-fn-key
                                            (/ (double (- (System/nanoTime)
                                                          start))
                                               1000000.0))
                                     ret))))
                             partial-val-map
                             sorted-keys)
                     {::timings @timings})))
      {::dag shaken-dag-with-inputs
       ::sorted-keys sorted-keys
       ::input-key-set input-key-set
       ::fn-map fn-map})))

(defprotocol Idismantle
  (dismantle [this] "destroy or close resource"))

;; Default no-op
(extend-type java.lang.Object
  Idismantle
  (dismantle [this] nil))

;; might as well auto do this for anything that can close
;; (note: AutoCloseable is the parent of Closeable so only need to
;; implement parent)
(extend-type java.lang.AutoCloseable
  Idismantle
  (dismantle [this]
    (.close this)))

(extend-type nil
  Idismantle
  (dismantle [this] nil))

(defn dismantlefm
  "dismantle map should include functions (fms) that dismantle the
object passed in, also for bookkeeping, any entries in the map may
also be used to determine what will be removed from the map after
dismantling (handled externally).

If value-specific dismantle function is not provided, will also call
  type specific Idismantle

On dismantle exception, immediately gives up, this is a tradeoff in
  that you can try to manually handle the case, extract debugging
  information, and then if so choose, continue the dismantle process
  by reinvoking with remaining objects

dismantle-if, the returned fn, when called returns nil

Might want to match the semantics of Stuart Sierra's component, where
  construction or destruction returns the data again
  "
  [dismantle-map tree-fn]
  (let [dismantle-order (-> tree-fn
                            meta
                            ::sorted-keys
                            reverse)]
    (with-meta (fn dismantle-if
                 ([val-map]
                  (dismantle-if val-map (fn [x] true)))
                 ([val-map apply?]
                  (doseq [the-fn-key dismantle-order]
                    (when (and (apply? the-fn-key)
                               (contains? val-map the-fn-key))
                      (if-let [the-fn (dismantle-map the-fn-key)]
                        (try
                          (the-fn val-map)
                          (catch Exception e
                            (throw (ex-info "dismantle-if node error"
                                            val-map
                                            e))))
                        (try
                          (dismantle (val-map the-fn-key))
                          (catch Exception e
                            (throw (ex-info "dismantle-if node error"
                                            val-map
                                            e)))))))))
      {::dismantle-map dismantle-map})))

(defn touched-children [tree-fn touched-nodes]
  (dag/touched-children (-> tree-fn
                            meta
                            ::dag
                            dag/reverse-dag)
                        touched-nodes))

(defn invalidate-entries
  "removes keys/vals of val-map that have been children of the recursively touched"
  [val-map treefn touched-nodes]
  (apply dissoc val-map (dag/touched-children (-> treefn
                                                  meta
                                                  ::dag
                                                  dag/reverse-dag) touched-nodes)))

(defn dependencies->rev-edges
  "
converts dependency graph into a graphviz friendly edge vector representing flow of data
"
  [direct-dependency-map]
  (->> (reduce-kv (fn add [ret k v]
                    (if (empty? v)
                      ret
                      (let [dep (first v)]
                        (recur (conj ret [k dep])
                               k
                               (disj v dep)))))
                  []
                  direct-dependency-map)
       (mapv (comp vec rseq))
       set))

(defn graphviz-data [treefn]
  (let [meta-data (meta treefn)
        edges (-> meta-data
                  ::dag
                  dependencies->rev-edges)
        input (-> meta-data
                  ::input-key-set)]
    {:type :digraph
     :name :tree_fn_flow
     :attributes {:graph {:size "10"}
                  :node (reduce (fn [ret node]
                                  (assoc ret
                                         node
                                         {:color :red
                                          :penwidth "3"}))
                                {}
                                input)}
     :edges edges}))
