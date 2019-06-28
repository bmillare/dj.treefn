(ns dj.dispatch.treefn.point
  "
* Motivation
- treefn's are flexible, easy to manipulate, composable, and
  reusable, however, are cumbersome to create/call for common tasks
- there is a need to make it easier to create/call treefn's that
  are commonly used
- The \"point\" model attempts to solve this problem by providing a
  reference \"point\" that is easy to use/call

* Model
- Points are simply an encapsulation of an output key
  (:target), treefn map (:treefns), and default input map
  (:inputs)
- Any existing point can be modified to provide the exact same
  functionality of another point
"
  (:refer-clojure :exclude [compile merge])
  (:require [dj.dispatch.treefn :as tf]))


(defn merge
  "merge's the contents of two points, with precedence for the
  later (left-to-right)"
  [point1 & points]
  (reduce (fn [ret m]
            (let [{:keys [target treefns inputs]} m]
              (-> (if target
                    (assoc ret
                           :target target)
                    ret)
                  (assoc :treefns
                         (clojure.core/merge (:treefns ret) treefns)
                         :inputs
                         (clojure.core/merge (:inputs ret) inputs)))))
          point1
          points))

(defn one-shot
  "create and call a treefn from a point and return the target

  A point is a map containing keys :target, :treefns,
  and :inputs"
  ([point]
   (let [{:keys [target treefns inputs]} point
         the-tf (tf/treefm treefns target)]
     (get (the-tf inputs)
          target)))
  ([point1 & points]
   (one-shot (apply merge point1 points))))

(defn compile
  "creates a fn that wraps a treefn from a point, and merges its
  current input with the point's input map, returns the target

  A point is a map containing keys :target, :treefns,
  and :inputs"
  ([point]
   (let [{:keys [target treefns inputs]} point
         the-tf (tf/treefm treefns target)]
     (fn [external-inputs]
       (get (the-tf (clojure.core/merge inputs external-inputs))
            target))))
  ([point1 & points]
   (compile (apply merge point1 points))))

(defn debug
  "create and call a treefn from a point and returns the full output
  map

  A point is a map containing keys :target, :treefns,
  and :inputs"
  ([point]
   (let [{:keys [target treefns inputs]} point
         the-tf (tf/treefm treefns target)]
     (the-tf inputs)))
  ([point1 & points]
   (debug (apply merge point1 points))))
