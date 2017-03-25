(ns corale.core-test
  (:require [corale.core])
  (:require-macros [corale.core]))

(corale.core/exclude-core)
(corale.core/require-corale)
(corale.core/require-corale-macros)

(identical? 1 2)
(identical? "e" "e")
(let [e #js [3]]
  (identical? e e))
(identical? #js {:e "e" :f 4} #js {:e "e" :f 4})

(nil? 3)

(array? #js [1 2])
(array? nil)
(array? #js {:e "e"})

(number? 3)
(number? nil)
(number? "e")

(not true)

(some? #js [])
(some? nil)

(object? #js {:e "e"})
(let [a #js [1]]
  (aset a 3 4)
  (object? a))

(string? "rr")
(string? nil)

(char? "er")
(char? "e")
(char? nil)

(type #js [2])
(type nil)
(type #js {:e "e"})

(system-time)

(defn ^array test-comp
  ([size]
     (js/Array. size))
  ([type size]
     (test-comp size))
  ([type size & more-sizes]
   (let [dims more-sizes
          dimarray (test-comp size)]
      (dotimes [i (alength dimarray)]
        (aset dimarray i (apply test-comp nil dims)))
      dimarray)))

(make-array 2)
(make-array nil 2)
(make-array nil 2 2)
(make-array nil 3 2 2)
(make-array nil 3 2 2 2)

#_(arr #js {:e "e"})

