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
  (aset a "r" 4)
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

(make-array 2)
(make-array nil 2)
(make-array nil 2 2)
(make-array nil 3 2 2)
(make-array nil 3 2 2 2)
(let [a #js [1 2]]
  (identical? a (aclone a)))

(array)
(array nil)
(array "e" "r" "g" 4 5)

;; emits a warning, the cljs.core version too
#_(aget (array) 2)
(let [a #js []]
  (aset a 3 4)
  a)

(alength #js [])
(alength #js [1 2 3])

(into-array #js {:e "e" :2 3})

(js-invoke "ed" "split" "")

;; (instance?)

#_(arr #js {:e "e"})

;;;;

#_(defn ff
    ([rr]
     (.log js/console rr))
    ([a [b c] {:strs [aa bb]} & rr]
       (.log js/console a b c)
       (.log js/console aa bb)
       (.log js/console rr)
       nil))

  #_(ff 3 4 5)
  #_(ff "a" #js ["fc" 3] #js {:aa "e" :bb 4} "r" 44)

(comment

  (defn ff
    ([& rr]
     (.log js/console rr))
    #_([a [b c] {:strs [aa bb]} & rr]
       (.log js/console a b c)
       (.log js/console aa bb)
       (.log js/console rr)
       nil))

  #_(ff 3 4 5)
  #_(ff "a" #js ["fc" 3] #js {:aa "e" :bb 4} "r" 44))




