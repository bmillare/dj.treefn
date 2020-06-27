# dj.treefn
A more composable version of "let"

## basic example

```clojure
(require '[dj.dispatch.treefn :as tf])
(let [args {:name "bob"
            :born-year 2001
            :current-year 2020}
      fms {:age (tf/fm [:born-year :current-year]
                       (- current-year born-year))
           :message (tf/fm [:name :age]
                           (str "bob is " age " years old"))}
      the-tf (tf/treefm fms :message)]
  (the-tf args))
;; =>
{:name "bob", :born-year 2001, :current-year 2020, :age 19, :message "bob is 19 years old"}
```
