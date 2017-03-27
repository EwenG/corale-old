(ns corale.core
  (:require [goog.object :as o]
            [goog.array :as garray]
            [corale.primitives])
  (:require-macros [corale.core])
  (:import [goog.string StringBuffer]))

(corale.core/require-corale-macros)
(corale.core/exclude-core)

(defn truth_
  "Internal - do not use!"
  [x]
  (corale.core/truth_ x))

(declare instance? Keyword)

(defn ^boolean identical?
  "Tests if 2 arguments are the same object"
  [x y]
  (corale.core/identical? x y))

(defn- pr-opts []
  #js {:flush-on-newline *flush-on-newline*
       :readably *print-readably*
       :meta *print-meta*
       :dup *print-dup*
       :print-length *print-length*})

(defn ^boolean nil?
  "Returns true if x is nil, false otherwise."
  [x]
  (corale.core/coercive-= x nil))

(defn ^boolean array?
  "Returns true if x is a JavaScript array."
  [x]
  (if (identical? *target* "nodejs")
    (.isArray js/Array x)
    (instance? js/Array x)))

(defn ^boolean number?
  "Returns true if x is a JavaScript number."
  [x]
  (corale.core/number? x))

(defn ^boolean not
  "Returns true if x is logical false, false otherwise."
  [x]
  (cond
    (nil? x) true
    (false? x) true
    :else false))

(defn ^boolean some?
  "Returns true if x is not nil, false otherwise."
  [x] (not (nil? x)))

(defn ^boolean object?
  "Returns true if x's constructor is Object"
  [x]
  (corale.core/if-not (nil? x)
    (identical? (.-constructor x) js/Object)
    false))

(defn ^boolean string?
  "Returns true if x is a JavaScript string."
  [x]
  (goog/isString x))

(defn ^boolean char?
  "Returns true if x is a JavaScript string of length one."
  [x]
  (and (string? x) (== 1 (.-length x))))

(defn ^boolean any?
  "Returns true if given any argument."
  [x] true)

(defn type
  "Return x's constructor."
  [x]
  (when-not (nil? x)
    (.-constructor x)))

(defn missing-protocol [proto obj]
  (let [ty (type obj)
        ty (if (and ty (.-cljs$lang$type ty))
             (.-cljs$lang$ctorStr ty)
             (goog/typeOf obj))]
    (js/Error.
     (.join (array "No protocol method " proto
                   " defined for type " ty ": " obj) ""))))

(defn type->str [ty]
  (if-let [s (.-cljs$lang$ctorStr ty)]
    s
    (str ty)))

(defn system-time
  "Returns highest resolution time offered by host in milliseconds."
  []
  (cond
    (and (exists? js/performance)
         (not (nil? (. js/performance -now))))
    (.now js/performance)

    (and (exists? js/process)
         (not (nil? (. js/process -hrtime))))
    (let [t (.hrtime js/process)]
      (/ (+ (* (aget t 0) 1e9) (aget t 1)) 1e6))

    :else (.getTime (js/Date.))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arrays ;;;;;;;;;;;;;;;;

(declare apply)

(defn ^array make-array
  "Construct a JavaScript array of the specified dimensions. Accepts ignored
  type argument for compatibility with Clojure. Note that there is no efficient
  way to allocate multi-dimensional arrays in JavaScript; as such, this function
  will run in polynomial time when called with 3 or more arguments."
  ([size]
     (js/Array. size))
  ([type size]
     (make-array size))
  ([type size & more-sizes]
    (let [dims more-sizes
          dimarray (make-array size)]
      (dotimes [i (alength dimarray)]
        (aset dimarray i (apply make-array nil dims)))
      dimarray)))

(defn aclone
  "Returns a javascript array, cloned from the passed in array"
  [arr]
  (let [len (alength arr)
        new-arr (make-array len)]
    (dotimes [i len]
      (aset new-arr i (aget arr i)))
    new-arr))

(defn ^array array
  "Creates a new javascript array.
  @param {...*} var_args" ;;array is a special case, don't emulate this doc string
  [var-args]            ;; [& items]
  (let [a (js/Array. (alength (corale.core/js-arguments)))]
    (loop [i 0]
      (if (< i (alength a))
        (do
          (aset a i (aget (corale.core/js-arguments) i))
          (recur (inc i)))
        a))))

(defn aget
  "Returns the value at the index."
  ([array i]
     (corale.core/aget array i))
  ([array i & idxs]
   (apply aget (aget array i) idxs)))

(defn aset
  "Sets the value at the index."
  ([array i val]
    (corale.core/aset array i val))
  ([array idx idx2 & idxv]
   (apply aset (aget array idx) idx2 idxv)))

(defn ^number alength
  "Returns the length of the array. Works on arrays of all types."
  [array]
  (corale.core/alength array))

(declare reduce)

(defn ^array into-array
  "Returns an array with components set to the values in aseq. Optional type
  argument accepted for compatibility with Clojure."
  ([aseq]
   (into-array nil aseq))
  ([type aseq]
   (reduce (fn [a x] (.push a x) a) (array) aseq)))

(defn js-invoke
  "Invoke JavaScript object method via string. Needed when the
  string is not a valid unquoted property name."
  [obj s & args]
  (.apply (aget obj s) obj args))

;;;;;;;;;;;;;;;;;;;;;;;;;;; core protocols ;;;;;;;;;;;;;

(defprotocol IFn
  "Protocol for adding the ability to invoke an object as a function.
  For example, a vector can also be used to look up a value:
  ([1 2 3 4] 1) => 2"
  (-invoke
    [this]
    [this a]
    [this a b]
    [this a b c]
    [this a b c d]
    [this a b c d e]
    [this a b c d e f]
    [this a b c d e f g]
    [this a b c d e f g h]
    [this a b c d e f g h i]
    [this a b c d e f g h i j]
    [this a b c d e f g h i j k]
    [this a b c d e f g h i j k l]
    [this a b c d e f g h i j k l m]
    [this a b c d e f g h i j k l m n]
    [this a b c d e f g h i j k l m n o]
    [this a b c d e f g h i j k l m n o p]
    [this a b c d e f g h i j k l m n o p q]
    [this a b c d e f g h i j k l m n o p q r]
    [this a b c d e f g h i j k l m n o p q r s]
    [this a b c d e f g h i j k l m n o p q r s t]
    [this a b c d e f g h i j k l m n o p q r s t rest]))

(defprotocol ICounted
  "Protocol for adding the ability to count a collection."
  (^number -count [coll]
   "Calculates the count of coll. Used by corale.core/count."))

(defprotocol IEmptyableCollection
  "Protocol for creating an empty collection."
  (-empty [coll]
    "Returns an empty collection of the same category as coll. Used
     by corale.core/empty."))

(defprotocol ICollection
  "Protocol for adding to a collection."
  (^clj -conj [coll o]
   "Returns a new collection of coll with o added to it. The new item
     should be added to the most efficient place, e.g.
     (conj [1 2 3 4] 5) => [1 2 3 4 5]
     (conj '(2 3 4 5) 1) => '(1 2 3 4 5)"))

(defprotocol IIndexed
  "Protocol for collections to provide indexed-based access to their items."
  (-nth [coll n] [coll n not-found]
    "Returns the value at the index n in the collection coll.
     Returns not-found if index n is out of bounds and not-found is supplied."))

(defprotocol ILookup
  "Protocol for looking up a value in a data structure."
  (-lookup [o k] [o k not-found]
    "Use k to look up a value in o. If not-found is supplied and k is not
     a valid value that can be used for look up, not-found is returned."))

(defprotocol IAssociative
  "Protocol for adding associativity to collections."
  (^boolean -contains-key? [coll k]
   "Returns true if k is a key in coll.")
  #_(-entry-at [coll k])
  (^clj -assoc [coll k v]
   "Returns a new collection of coll with a mapping from key k to
     value v added to it."))

(defprotocol IMap
  "Protocol for adding mapping functionality to collections."
  #_(-assoc-ex [coll k v])
  (^clj -dissoc [coll k]
   "Returns a new collection of coll without the mapping for key k."))

(defprotocol ISet
  "Protocol for adding set functionality to a collection."
  (^clj -disjoin [coll v]
   "Returns a new collection of coll that does not contain v."))

(defprotocol IStack
  "Protocol for collections to provide access to their items as stacks. The top
  of the stack should be accessed in the most efficient way for the different
  data structures."
  (-peek [coll]
    "Returns the item from the top of the stack. Is used by corale.core/peek.")
  (^clj -pop [coll]
   "Returns a new stack without the item on top of the stack. Is used
     by corale.core/pop."))

(defprotocol IDeref
  "Protocol for adding dereference functionality to a reference."
  (-deref [o]
    "Returns the value of the reference o."))

(defprotocol IReduce
  "Protocol for seq types that can reduce themselves.
  Called by corale..core/reduce."
  (-reduce [coll f] [coll f start]
    "f should be a function of 2 arguments. If start is not supplied,
     returns the result of applying f to the first 2 items in coll, then
     applying f to that result and the 3rd item, etc."))

(defprotocol IKVReduce
  "Protocol for associative types that can reduce themselves
  via a function of key and val. Called by corale.core/reduce-kv."
  (-kv-reduce [coll f init]
    "Reduces an associative collection and returns the result. f should be
     a function that takes three arguments."))

(defprotocol IEquiv
  "Protocol for adding value comparison functionality to a type."
  (^boolean -equiv [o other]
   "Returns true if o and other are equal, false otherwise."))

(defprotocol IHash
  "Protocol for adding hashing functionality to a type."
  (-hash [o]
    "Returns the hash code of o."))

(defprotocol ISequential
  "Marker interface indicating a persistent collection of sequential items")

(defprotocol IRecord
  "Marker interface indicating a record object")

(defprotocol ISorted
  "Protocol for a collection which can represent their items
  in a sorted manner. "
  (^clj -sorted-seq [coll ascending?]
   "Returns a sorted seq from coll in either ascending or descending order.")
  (^clj -sorted-seq-from [coll k ascending?]
   "Returns a sorted seq from coll in either ascending or descending order.
     If ascending is true, the result should contain all items which are > or >=
     than k. If ascending is false, the result should contain all items which
     are < or <= than k, e.g.
     (-sorted-seq-from (sorted-set 1 2 3 4 5) 3 true) => (3 4 5)
     (-sorted-seq-from (sorted-set 1 2 3 4 5) 3 false) => (3 2 1)")
  (-entry-key [coll entry]
    "Returns the key for entry.")
  (-comparator [coll]
    "Returns the comparator for coll."))

(defprotocol IPrintWithWriter
  "The old IPrintable protocol's implementation consisted of building a giant
   list of strings to concatenate.  This involved lots of concat calls,
   intermediate vectors, and lazy-seqs, and was very slow in some older JS
   engines.  IPrintWithWriter implements printing via the IWriter protocol, so it
   be implemented efficiently in terms of e.g. a StringBuffer append."
  (-pr-writer [o writer opts]))

(defprotocol IComparable
  "Protocol for values that can be compared."
  (^number -compare [x y]
   "Returns a negative number, zero, or a positive number when x is logically
     'less than', 'equal to', or 'greater than' y."))

(defprotocol INamed
  "Protocol for adding a name."
  (^string -name [x]
   "Returns the name String of x.")
  (^string -namespace [x]
   "Returns the namespace String of x."))


(defprotocol IArrayable
  "Protocol for adding the ability to a type to be transformed into an array."
  (-arr [o]
    "Returns a array of o, or nil if o is empty."))

;; Printing support

(defn pr-str*
  "Support so that collections can implement toString without
   loading all the printing machinery."
  [^not-native obj]
  (let [sb (StringBuffer.)
        writer (StringBufferWriter. sb)]
    (-pr-writer obj writer (pr-opts))
    (-flush writer)
    (str sb)))

;;;;;;;;;;;;;;;;;;; Murmur3 ;;;;;;;;;;;;;;;

;;http://hg.openjdk.java.net/jdk7u/jdk7u6/jdk/file/8c2c5d63a17e/src/share/classes/java/lang/Integer.java
(defn ^number int-rotate-left [x n]
  (bit-or
    (bit-shift-left x n)
    (unsigned-bit-shift-right x (- n))))

;; http://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/imul
(if (and (exists? Math/imul)
         (not (zero? (Math/imul 0xffffffff 5))))
  (defn ^number imul [a b] (Math/imul a b))
  (defn ^number imul [a b]
    (let [ah (bit-and (unsigned-bit-shift-right a 16) 0xffff)
          al (bit-and a 0xffff)
          bh (bit-and (unsigned-bit-shift-right b 16) 0xffff)
          bl (bit-and b 0xffff)]
      (bit-or
        (+ (* al bl)
           (unsigned-bit-shift-right
             (bit-shift-left (+ (* ah bl) (* al bh)) 16) 0)) 0))))

;; http://smhasher.googlecode.com/svn/trunk/MurmurHash3.cpp
(def m3-seed 0)
(def m3-C1 (int 0xcc9e2d51))
(def m3-C2 (int 0x1b873593))

(defn ^number m3-mix-K1 [k1]
  (-> (int k1) (imul m3-C1) (int-rotate-left 15) (imul m3-C2)))

(defn ^number m3-mix-H1 [h1 k1]
  (int (-> (int h1) (bit-xor (int k1)) (int-rotate-left 13) (imul 5) (+ (int 0xe6546b64)))))

(defn ^number m3-fmix [h1 len]
  (as-> (int h1) h1
    (bit-xor h1 len)
    (bit-xor h1 (unsigned-bit-shift-right h1 16))
    (imul h1 (int 0x85ebca6b))
    (bit-xor h1 (unsigned-bit-shift-right h1 13))
    (imul h1 (int 0xc2b2ae35))
    (bit-xor h1 (unsigned-bit-shift-right h1 16))))

(defn ^number m3-hash-int [in]
  (if (zero? in)
    in
    (let [k1 (m3-mix-K1 in)
          h1 (m3-mix-H1 m3-seed k1)]
      (m3-fmix h1 4))))

(defn ^number m3-hash-unencoded-chars [in]
  (let [h1 (loop [i 1 h1 m3-seed]
             (if (< i (alength in))
               (recur (+ i 2)
                 (m3-mix-H1 h1
                   (m3-mix-K1
                     (bit-or (.charCodeAt in (dec i))
                       (bit-shift-left (.charCodeAt in i) 16)))))
               h1))
        h1 (if (== (bit-and (alength in) 1) 1)
             (bit-xor h1 (m3-mix-K1 (.charCodeAt in (dec (alength in)))))
             h1)]
    (m3-fmix h1 (imul 2 (alength in)))))

;;;;;;;;;;;;;;;;;;; symbols ;;;;;;;;;;;;;;;

(declare Symbol = compare)

;; Simple caching of string hashcode
(def string-hash-cache (js-obj))
(def string-hash-cache-count 0)

;;http://hg.openjdk.java.net/jdk7u/jdk7u6/jdk/file/8c2c5d63a17e/src/share/classes/java/lang/String.java
(defn hash-string* [s]
  (corale.core/if-not (nil? s)
    (let [len (alength s)]
      (if (pos? len)
        (loop [i 0 hash 0]
          (if (< i len)
            (recur (inc i) (+ (imul 31 hash) (.charCodeAt s i)))
            hash))
        0))
    0))

(defn add-to-string-hash-cache [k]
  (let [h (hash-string* k)]
    (aset string-hash-cache k h)
    (set! string-hash-cache-count (inc string-hash-cache-count))
    h))

(defn hash-string [k]
  (when (> string-hash-cache-count 255)
    (set! string-hash-cache (js-obj))
    (set! string-hash-cache-count 0))
  (if (nil? k)
    0
    (let [h (aget string-hash-cache k)]
      (if (number? h)
        h
        (add-to-string-hash-cache k)))))

(defn hash
  "Returns the hash code of its argument. Note this is the hash code
   consistent with =."
  [o]
  (cond
    (corale.core/implements? IHash o)
    (-hash ^not-native o)

    (number? o)
    (if (js/isFinite o)
      (js-mod (Math/floor o) 2147483647)
      (case o
        Infinity
        2146435072
        -Infinity
        -1048576
        2146959360))

    ;; note: mirrors Clojure's behavior on the JVM, where the hashCode is
    ;; 1231 for true and 1237 for false
    ;; http://docs.oracle.com/javase/7/docs/api/java/lang/Boolean.html#hashCode%28%29
    (true? o) 1231

    (false? o) 1237

    (string? o)
    (m3-hash-int (hash-string o))

    (instance? js/Date o)
    (.valueOf o)

    (nil? o) 0

    :else
    (-hash o)))

(defn hash-combine [seed hash]
  ; a la boost
  (bit-xor seed
    (+ hash 0x9e3779b9
      (bit-shift-left seed 6)
      (bit-shift-right seed 2))))

(defn ^boolean instance?
  "Evaluates x and tests if it is an instance of the type
  c. Returns true or false"
  [c x]
  (corale.core/instance? c x))

(defn ^boolean symbol?
  "Return true if x is a Symbol"
  [x]
  (instance? Symbol x))

(defn- hash-symbol [sym]
  (hash-combine
    (m3-hash-unencoded-chars (.-name sym))
    (hash-string (.-ns sym))))

(defn- compare-symbols [a b]
  (cond
   (identical? (.-str a) (.-str b)) 0
   (and (not (.-ns a)) (.-ns b)) -1
   (.-ns a) (corale.core/if-not (.-ns b)
              1
              (let [nsc (garray/defaultCompare (.-ns a) (.-ns b))]
                (if (== 0 nsc)
                  (garray/defaultCompare (.-name a) (.-name b))
                  nsc)))
   :default (garray/defaultCompare (.-name a) (.-name b))))

(declare get)

(deftype Symbol [ns name str ^:mutable _hash _meta]
  Object
  (toString [_] str)
  (equiv [this other] (-equiv this other))

  IEquiv
  (-equiv [_ other]
    (if (instance? Symbol other)
      (identical? str (.-str other))
      false))

  IFn
  (-invoke [sym coll]
    (get coll sym))
  (-invoke [sym coll not-found]
    (get coll sym not-found))

  IHash
  (-hash [sym]
    (corale.core/caching-hash sym hash-symbol _hash))

  INamed
  (-name [_] name)
  (-namespace [_] ns)

  IPrintWithWriter
  (-pr-writer [o writer _] (-write writer str))

  ;; printing at the REPL calls cljs.core function
  cljs.core/IPrintWithWriter
  (cljs.core/-pr-writer [o writer _] (-write writer str)))

(defn symbol
  "Returns a Symbol with the given namespace and name."
  ([name]
   (if (symbol? name)
     name
     (let [idx (.indexOf name "/")]
       (if (< idx 1)
         (symbol nil name)
         (symbol (.substring name 0 idx)
                 (.substring name (inc idx) (. name -length)))))))
  ([ns name]
   (let [sym-str (corale.core/if-not (nil? ns)
                   (str ns "/" name)
                   name)]
     (Symbol. ns name sym-str nil nil))))

;;;;;;;;;;;;;;;;;;; fundamentals ;;;;;;;;;;;;;;;

(defn arr
  "Returns an array of collection. If the collection is
  empty, returns nil.  (arr nil) returns nil. arr also works on
  Strings."
  [coll]
  (when-not (nil? coll)
    (cond
      (corale.core/implements? IArrayable coll)
      (-arr ^not-native coll)

      (array? coll)
      (when-not (zero? (alength coll))
        coll)

      (object? coll)
      (let [arr #js []
            builder (fn [v k o] (.push arr #js [k v]))]
        (o/forEach coll builder)
        (if (zero? (alength arr)) nil arr))

      (string? coll)
      (.split coll "")

      :else (throw (js/Error. (str coll " is not IArrayable"))))))

(defn first
  "Returns the first item in the collection. If coll is nil, returns nil."
  [coll]
  (when-not (nil? coll)
    (get (arr coll) 0)))

(declare = count)

(defn ^boolean =
  "Equality. Returns true if x equals y, false if not. Compares
  numbers and collections in a type-independent manner."
  ([x] true)
  ([x y]
   (cond (nil? x) (nil? y)
         (identical? x y) true
         (array? x) (and (array? y)
                         (when (array? y)
                           (let [xl (alength x)]
                             (if (not (== xl (alength y)))
                               false
                               (loop [i 0]
                                 (if (< i xl)
                                   (if (= (aget x i) (aget y i))
                                     (recur (inc i))
                                     false)
                                   true))))))
         (object? x) (and (object? y)
                          (when (object? y)
                            (corale.primitives/compareObj = x y)))
         :else ^boolean (-equiv x y)))
  ([x y & more]
   (let [l (alength more)]
     (loop [x x
            y y
            more more
            i 0]
       (if (= x y)
         (if (== i l)
           true
           (recur y (aget more i) more (inc i)))
         false)))))

;;;;;;;;;;;;;;;;;;; Murmur3 Helpers ;;;;;;;;;;;;;;;;

(defn ^number mix-collection-hash
  "Mix final collection hash for ordered or unordered collections.
   hash-basis is the combined collection hash, count is the number
   of elements included in the basis. Note this is the hash code
   consistent with =, different from .hashCode.
   See http://clojure.org/data_structures#hash for full algorithms."
  [hash-basis count]
  (let [h1 m3-seed
        k1 (m3-mix-K1 hash-basis)
        h1 (m3-mix-H1 h1 k1)]
    (m3-fmix h1 count)))

#_(defn ^number hash-ordered-coll
  "Returns the hash code, consistent with =, for an external ordered
   collection implementing Iterable.
   See http://clojure.org/data_structures#hash for full algorithms."
  [coll]
  (loop [n 0 hash-code 1 coll (seq coll)]
    (corale.core/if-not (nil? coll)
      (recur (inc n) (bit-or (+ (imul 31 hash-code) (hash (first coll))) 0)
        (next coll))
      (mix-collection-hash hash-code n))))

(def ^:private empty-ordered-hash
  (mix-collection-hash 1 0))

#_(defn ^number hash-unordered-coll
  "Returns the hash code, consistent with =, for an external unordered
   collection implementing Iterable. For maps, the iterator should
   return map entries whose hash is computed as
     (hash-ordered-coll [k v]).
   See http://clojure.org/data_structures#hash for full algorithms."
  [coll]
  (loop [n 0 hash-code 0 coll (seq coll)]
    (corale.core/if-not (nil? coll)
      (recur (inc n) (bit-or (+ hash-code (hash (first coll))) 0) (next coll))
      (mix-collection-hash hash-code n))))

(def ^:private empty-unordered-hash
  (mix-collection-hash 0 0))

;;;;;;;;;;;;;;;;;;; protocols on primitives ;;;;;;;;

(declare js-obj)

(extend-type nil
  ICounted
  (-count [_] 0))

;; TODO: we should remove this and handle date equality checking
;; by some other means, probably by adding a new primitive type
;; case to the hash table lookup - David

(extend-type js/Date
  IEquiv
  (-equiv [o other]
    (and (instance? js/Date other)
         (== (.valueOf o) (.valueOf other))))

  IComparable
  (-compare [this other]
    (if (instance? js/Date other)
      (garray/defaultCompare (.valueOf this) (.valueOf other))
      (throw (js/Error. (str "Cannot compare " this " to " other))))))

(defprotocol Inst
  (inst-ms* [inst]))

(extend-protocol Inst
  js/Date
  (inst-ms* [inst] (.getTime inst)))

(defn inst-ms
  "Return the number of milliseconds since January 1, 1970, 00:00:00 GMT"
  [inst]
  (inst-ms* inst))

(defn ^boolean inst?
  "Return true if x satisfies Inst"
  [x]
  (corale.core/satisfies? Inst x))

(extend-type number
  IEquiv
  (-equiv [x o] (identical? x o)))

(extend-type default
  IHash
  (-hash [o]
    (goog/getUid o)))

;;this is primitive because & emits call to array-seq
(defn inc
  "Returns a number one greater than num."
  [x] (corale.core/+ x 1))

(declare deref)

(deftype Reduced [val]
  IDeref
  (-deref [o] val))

(defn reduced
  "Wraps x in a way such that a reduce will terminate with the value x"
  [x]
  (Reduced. x))

(defn ^boolean reduced?
  "Returns true if x is the result of a call to reduced"
  [r]
  (instance? Reduced r))

(defn ensure-reduced
  "If x is already reduced?, returns it, else returns (reduced x)"
  [x]
  (if (reduced? x) x (reduced x)))

(defn unreduced
  "If x is reduced?, returns (deref x), else returns x"
  [x]
  (if (reduced? x) (deref x) x))

;; generic to all refs
;; (but currently hard-coded to atom!)
(defn deref
  "Also reader macro: @var/@atom/@delay. Returns the
   most-recently-committed value of ref. When applied to a var
   or atom, returns its current state. When applied to a delay, forces
   it if not already forced. See also - realized?."
  [o]
  (-deref o))

(defn- array-reduce
  ([arr f]
     (let [cnt (alength arr)]
       (if (zero? (alength arr))
         (f)
         (loop [val (aget arr 0), n 1]
           (if (< n cnt)
             (let [nval (f val (aget arr n))]
               (if (reduced? nval)
                 (deref nval)
                 (recur nval (inc n))))
             val)))))
  ([arr f val]
     (let [cnt (alength arr)]
       (loop [val val, n 0]
         (if (< n cnt)
           (let [nval (f val (aget arr n))]
             (if (reduced? nval)
               (deref nval)
               (recur nval (inc n))))
           val))))
  ([arr f val idx]
     (let [cnt (alength arr)]
       (loop [val val, n idx]
         (if (< n cnt)
           (let [nval (f val (aget arr n))]
             (if (reduced? nval)
               (deref nval)
               (recur nval (inc n))))
           val)))))

(declare cons drop count nth)

(defn ^boolean counted?
  "Returns true if coll implements count in constant time"
  [x] (corale.core/satisfies? ICounted x))

(defn ^boolean indexed?
  "Returns true if coll implements nth in constant time"
  [x] (corale.core/satisfies? IIndexed x))

(defn second
  [coll]
  (get (arr coll) 1))

(defn ffirst
  "Same as (first (first x))"
  [coll]
  (first (first coll)))

(defn last
  "Return the last item in coll"
  [s]
  (when-let [a (arr s)]
    (get a (dec (.-length a)))))

(extend-type default
  IEquiv
  (-equiv [x o] (identical? x o)))

(defn conj
  "conj[oin]. Returns a new collection with the xs
  'added'. (conj nil item) returns #js [item].  The 'addition' may
  happen at different 'places' depending on the concrete type."
  ([] (array))
  ([coll] coll)
  ([coll x]
   (cond (nil? coll)
         (array x)
         (array? coll)
         (let [copy (aclone coll)]
           (.push copy x)
           copy)
         (object? coll)
         (let [copy (o/clone coll)]
           (o/set copy (nth x 0) (nth x 1))
           copy)
         :else (-conj coll x)))
  ([coll x & xs]
   (cond (nil? coll)
         (apply array x xs)
         (array? coll)
         (let [copy (aclone coll)]
           (.push copy x)
           (doseq [x xs]
             (.push copy x))
           copy)
         (object? coll)
         (let [copy (o/clone coll)]
           (o/set copy (nth x 0) (nth x 1))
           (doseq [x xs]
             (o/set copy (nth x 0) (nth x 1)))
           copy)
         :else (let [l (alength xs)]
                 (loop [coll (-conj coll x)
                        i 0]
                   (if (< i l)
                     (recur (-conj coll (aget xs i)) (inc i))
                     coll))))))

(defn empty
  "Returns an empty collection of the same category as coll, or nil"
  [coll]
  (cond (nil? coll) nil
        (array? coll) (array)
        (object? coll) (js-obj)
        :else (-empty coll)))

(defn count
  "Returns the number of items in the collection. (count nil) returns
  0.  Also works on strings, arrays, and Maps"
  [coll]
  (corale.core/if-not (nil? coll)
    (cond
      (corale.core/implements? ICounted coll)
      (-count ^not-native coll)

      (array? coll)
      (alength coll)
    
      (string? coll)
      (alength coll)

      (corale.core/implements? IArrayable coll)
      (alength (-arr coll))

      :else (-count coll))
    0))

(defn nth
  "Returns the value at the index. get returns nil if index out of
  bounds, nth throws an exception unless not-found is supplied.  nth
  also works for strings, arrays and regex Matchers."
  ([coll n]
    (cond
      (not (number? n))
      (throw (js/Error. "Index argument to nth must be a number"))

      (nil? coll)
      coll

      (corale.core/implements? IIndexed coll)
      (-nth ^not-native coll n)

      (array? coll)
      (if (and (>= n 0) (< n (.-length coll)))
        (aget coll n)
        (throw (js/Error. "Index out of bounds")))

      (string? coll)
      (if (and (>= n 0) (< n (.-length coll)))
        (.charAt coll n)
        (throw (js/Error. "Index out of bounds")))

      (native-satisfies? IIndexed coll)
      (-nth coll n)

      :else
      (throw (js/Error. (str "nth not supported on this type "
                          (type->str (type coll)))))))
  ([coll n not-found]
   (cond
     (not (number? n))
     (throw (js/Error. "Index argument to nth must be a number."))

     (nil? coll)
     not-found

     (corale.core/implements? IIndexed coll)
     (-nth ^not-native coll n not-found)

     (array? coll)
     (if (and (>= n 0) (< n (.-length coll)))
       (aget coll n)
       not-found)

     (string? coll)
     (if (and (>= n 0) (< n (.-length coll)))
       (.charAt coll n)
       not-found)

     (native-satisfies? IIndexed coll)
     (-nth coll n)

     :else
     (throw (js/Error. (str "nth not supported on this type "
                            (type->str (type coll))))))))

(defn get
  "Returns the value mapped to key, not-found or nil if key not present."
  ([o k]
   (when-not (nil? o)
      (cond
        (corale.core/implements? ILookup o)
        (-lookup ^not-native o k)

        (array? o)
        (when (and (some? k) (< k (.-length o)))
          (aget o (int k)))

        (object? o)
        (if (nil? k)
          (o/get o "")
          (o/get o k))

        (string? o)
        (when (and (some? k) (< k (.-length o)))
          (.charAt o (int k)))

        (native-satisfies? ILookup o)
        (-lookup o k)

        :else nil)))
  ([o k not-found]
    (corale.core/if-not (nil? o)
      (cond
        (corale.core/implements? ILookup o)
        (-lookup ^not-native o k not-found)

        (array? o)
        (if (and (some? k) (>= k 0) (< k (.-length o)))
          (aget o (int k))
          not-found)

        (object? o)
        (if (nil? k)
          (o/get o "" not-found)
          (o/get o k not-found))

        (string? o)
        (if (and (some? k) (>= k 0) (< k (.-length o)))
          (.charAt o (int k))
          not-found)

        (native-satisfies? ILookup o)
        (-lookup o k not-found)

        :else not-found)
      not-found)))

(defn assoc
  "assoc[iate]. When applied to a map, returns a new map of the
   same (hashed/sorted) type, that contains the mapping of key(s) to
   val(s). When applied to an array, returns a new array that
   contains val at index."
  ([coll k v]
   (cond (nil? coll) (js-obj k v)
         (array? coll)
         (let [copy (aclone coll)]
           (aset copy k v)
           copy)
         (object? coll)
         (let [copy (o/clone coll)]
           (o/set copy k v)
           copy)
         :else (-assoc coll k v)))
  ([coll k v & kvs]
   (cond (nil? coll) (apply js-obj k v kvs)
         (array? coll)
         (let [copy (aclone coll)
               l (alength kvs)]
           (aset copy k v)
           (loop [i 0]
             (when (< i l)
               (aset copy (aget kvs i) (aget kvs (inc i)))
               (recur (+ i 2))))
           copy)
         (object? coll)
         (let [copy (o/clone coll)
               l (alength kvs)]
           (o/set copy k v)
           (loop [i 0]
             (when (< i l)
               (o/set copy (aget kvs i) (aget kvs (inc i)))
               (recur (+ i 2))))
           copy)
         :else (let [l (alength kvs)]
                 (loop [coll (-assoc coll k v)
                        i 0]
                   (if (< i l)
                     (recur (-assoc coll (aget kvs i) (aget kvs (inc i))) (+ i 2))
                     coll))))))

(defn dissoc
  "dissoc[iate]. Returns a new map of the same (hashed/sorted) type,
  that does not contain a mapping for key(s)."
  ([coll] coll)
  ([coll k]
   (cond (nil? coll) nil
         (object? coll)
         (let [copy (o/clone coll)]
           (o/remove copy k)
           copy)
         :else (-dissoc coll k)))
  ([coll k & ks]
   (cond (nil? coll) nil
         (object? coll)
         (let [copy (o/clone coll)]
           (o/remove copy k)
           (doseq [k ks]
             (o/remove copy k))
           copy)
         :else (let [copy (o/clone coll)]
                 (-dissoc copy k)
                 (doseq [k ks]
                   (-dissoc copy k))
                 copy))))

(defn ^boolean fn?
  "Return true if f is a JavaScript function or satisfies the Fn protocol."
  [f]
  (or ^boolean (goog/isFunction f) (corale.core/satisfies? Fn f)))

(defn peek
  "For a queue, same as first, for an array, same as last. 
  If the collection is empty, returns nil."
  [coll]
  (cond (nil? coll) nil
        (array? coll) (last coll)
        :else (-peek coll)))

(defn pop
  "For a queue, returns a new queue without the first
  item, for an array, returns a new array without the last item."
  [coll]
  (cond (nil? coll) nil
        (array? coll) (let [copy (aclone coll)]
                        (.pop copy)
                        copy)
        :else (-pop coll)))

(defn disj
  "disj[oin]. Returns a new set of the same (hashed/sorted) type, that
  does not contain key(s)."
  ([coll] coll)
  ([coll k]
   (when-not (nil? coll)
     (-disjoin coll k)))
  ([coll k & ks]
   (when-not (nil? coll)
     (let [l (alength ks)]
       (loop [coll (-disjoin coll k)
              i 0]
         (if (< i l)
           (recur (-disjoin coll (aget ks i)) (inc i))
           coll))))))

(defn ^boolean empty?
  "Returns true if coll has no items."
  [coll] (cond (nil? coll) true
               (object? coll) (o/isEmpty coll)
               :else (= 0 (alength (arr coll)))))

(defn ^boolean coll?
  "Returns true if x satisfies ICollection"
  [x]
  (if (nil? x)
    false
    (corale.core/satisfies? ICollection x)))

(defn ^boolean set?
  "Returns true if x satisfies ISet"
  [x]
  (if (nil? x)
    false
    (corale.core/satisfies? ISet x)))

(defn ^boolean associative?
 "Returns true if coll implements Associative"
  [x] (corale.core/satisfies? IAssociative x))

(defn ^boolean sequential?
  "Returns true if coll satisfies ISequential"
  [x] (corale.core/satisfies? ISequential x))

(defn ^boolean sorted?
  "Returns true if coll satisfies ISorted"
  [x] (corale.core/satisfies? ISorted x))

(defn ^boolean reduceable?
  "Returns true if coll satisfies IReduce"
  [x] (corale.core/satisfies? IReduce x))

(defn ^boolean map?
  "Return true if x satisfies IMap"
  [x]
  (if (nil? x)
    false
    (corale.core/satisfies? IMap x)))

(defn ^boolean record?
  "Return true if x satisfies IRecord"
  [x]
  (corale.core/satisfies? IRecord x))

;;;;;;;;;;;;;;;;;;;; js primitives ;;;;;;;;;;;;
(defn js-obj
  "Create JavaSript object from an even number arguments representing
  interleaved keys and values."
  ([]
   (corale.core/js-obj))
  ([& keyvals]
   (apply o/create keyvals)))

(defn js-keys
  "Return the JavaScript keys for an object."
  [obj]
  (let [keys (array)]
    (o/forEach obj (fn [val key obj] (.push keys key)))
    keys))

(defn js-delete
  "Delete a property from a JavaScript object."
  [obj key]
  (corale.core/js-delete obj key))

;;;;;;;;;;;;;;;; preds ;;;;;;;;;;;;;;;;;;

(def ^:private lookup-sentinel (js-obj))

(defn ^boolean false?
  "Returns true if x is the value false, false otherwise."
  [x] (corale.core/false? x))

(defn ^boolean true?
  "Returns true if x is the value true, false otherwise."
  [x] (corale.core/true? x))

(defn ^boolean boolean?
  "Return true if x is a Boolean"
  [x] (or (corale.core/true? x) (corale.core/false? x)))

(defn ^boolean undefined?
  "Returns true if x identical to the JavaScript undefined value."
  [x]
  (corale.core/undefined? x))

(defn ^boolean arrayable?
  "Return true if the arr function is supported for s"
  [s]
  (or
   (corale.core/satisfies? IArrayable s)
   (array? s)
   (object? s)
   (string? s)))

(defn ^boolean boolean
  "Coerce to boolean"
  [x]
  (cond
    (nil? x) false
    (false? x) false
    :else true))

(defn ^boolean ifn?
  "Returns true if f returns true for fn? or satisfies IFn."
  [f]
  (or (fn? f) (corale.core/satisfies? IFn f)))

(defn ^boolean integer?
  "Returns true if n is a JavaScript number with no decimal part."
  [n]
  (and (number? n)
       (not ^boolean (js/isNaN n))
       (not (identical? n js/Infinity))
       (== (js/parseFloat n) (js/parseInt n 10))))

(defn ^boolean int?
  "Return true if x satisfies integer? or is an instance of goog.math.Integer
   or goog.math.Long."
  [x]
  (or (integer? x)
      (instance? goog.math.Integer x)
      (instance? goog.math.Long x)))

(defn ^boolean pos-int?
  "Return true if x satisfies int? and is positive."
  [x]
  (cond
    (integer? x) (pos? x)

    (instance? goog.math.Integer x)
    (and (not (.isNegative x))
         (not (.isZero x)))

    (instance? goog.math.Long x)
    (and (not (.isNegative x))
         (not (.isZero x)))

    :else false))

(defn ^boolean neg-int?
  "Return true if x satisfies int? and is positive."
  [x]
  (cond
    (integer? x) (neg? x)

    (instance? goog.math.Integer x)
    (.isNegative x)

    (instance? goog.math.Long x)
    (.isNegative x)

    :else false))

(defn ^boolean nat-int?
  "Return true if x satisfies int? and is a natural integer value."
  [x]
  (cond
    (integer? x)
    (or (not (neg? x)) (zero? x))

    (instance? goog.math.Integer x)
    (or (not (.isNegative x)) (.isZero x))

    (instance? goog.math.Long x)
    (or (not (.isNegative x)) (.isZero x))

    :else false))

(defn ^boolean float?
  "Returns true for JavaScript numbers, false otherwise."
  [x]
  (number? x))

(defn ^boolean double?
  "Returns true for JavaScript numbers, false otherwise."
  [x]
  (number? x))

(defn ^boolean infinite?
  "Returns true for Infinity and -Infinity values."
  [x]
  (or (identical? x js/Number.POSITIVE_INFINITY)
      (identical? x js/Number.NEGATIVE_INFINITY)))

(defn ^boolean contains?
  "Returns true if key is present in the given collection, otherwise
  returns false.  Note that for numerically indexed collections like
  arrays, this tests if the numeric key is within the
  range of indexes. 'contains?' operates constant or logarithmic time;
  it will not perform a linear search for a value.  See also 'some'."
  [coll v]
  (if (identical? (get coll v lookup-sentinel) lookup-sentinel)
    false
    true))

(defn find
  "Returns the map entry for key, or nil if key not present."
  [coll k]
  (when (and (not (nil? coll))
             (or (associative? coll)
                 (object? coll))
             (contains? coll k))
    (array k (get coll k))))

(declare set)

(defn ^boolean distinct?
  "Returns true if no two of the arguments are ="
  ([x] true)
  ([x y] (not (= x y)))
  ([x y & more]
   (if (not (= x y))
     (let [l (alength more)]
       (loop [s (apply set x y) i 0]
         (if (< i l)
           (if (contains? s (aget more i))
             false
             (recur (conj s x) (inc i)))
           true)))
     false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Seq fns ;;;;;;;;;;;;;;;;

(declare compare-indexed)

(defn ^number compare
  "Comparator. Returns a negative number, zero, or a positive number
  when x is logically 'less than', 'equal to', or 'greater than'
  y. Uses IComparable if available and google.array.defaultCompare for objects
  of the same type and special-cases nil to be less than any other object."
  [x y]
  (cond
    (identical? x y) 0

    (nil? x) -1

    (nil? y) 1

    (number? x) (if (number? y)
                  (garray/defaultCompare x y)
                  (throw (js/Error. (str "Cannot compare " x " to " y))))

    (array? x) (compare-indexed x y)

    (corale.core/satisfies? IComparable x)
    (-compare x y)

    :else
    (if (and (or (string? x) (array? x) (true? x) (false? x))
             (identical? (type x) (type y)))
      (garray/defaultCompare x y)
      (throw (js/Error. (str "Cannot compare " x " to " y))))))

(defn ^:private compare-indexed
  "Compare indexed collection."
  ([xs ys]
     (let [xl (count xs)
           yl (count ys)]
       (cond
        (< xl yl) -1
        (> xl yl) 1
        (== xl 0) 0
        :else (compare-indexed xs ys xl 0))))
  ([xs ys len n]
     (let [d (compare (nth xs n) (nth ys n))]
       (if (and (zero? d) (< (+ n 1) len))
         (recur xs ys len (inc n))
         d))))

(defn ^:private fn->comparator
  "Given a fn that might be boolean valued or a comparator,
   return a fn that is a comparator."
  [f]
  (if (= f compare)
    compare
    (fn [x y]
      (let [r (f x y)]
        (if (number? r)
          r
          (if r
            -1
            (if (f y x) 1 0)))))))

(defn sort
  "Returns a sorted sequence of the items in coll. Comp can be
   boolean-valued comparison function, or a -/0/+ valued comparator.
   Comp defaults to compare."
  ([coll]
   (sort compare coll))
  ([comp coll]
   (if (array? coll)
     (let [copy (aclone coll)]
       (garray/stableSort copy (fn->comparator comp))
       copy)
     (if-let [a (arr coll)]
       ;; matching Clojure's stable sort, though docs don't promise it
       (do (garray/stableSort a (fn->comparator comp)) a)
       (array)))))

(defn sort-by
  "Returns a sorted sequence of the items in coll, where the sort
   order is determined by comparing (keyfn item).  Comp can be
   boolean-valued comparison funcion, or a -/0/+ valued comparator.
   Comp defaults to compare."
  ([keyfn coll]
   (sort-by keyfn compare coll))
  ([keyfn comp coll]
   (sort (fn [x y] ((fn->comparator comp) (keyfn x) (keyfn y))) coll)))

(defn shuffle
  "Return a random permutation of coll"
  [coll]
  (if (nil? coll)
    (array)
    (if (array? coll)
      (let [copy (aclone coll)]
        (garray/shuffle copy)
        copy)
      (let [a (arr coll)]
        (garray/shuffle a)
        a))))

(defn reduce
  "f should be a function of 2 arguments. If val is not supplied,
  returns the result of applying f to the first 2 items in coll, then
  applying f to that result and the 3rd item, etc. If coll contains no
  items, f must accept no arguments as well, and reduce returns the
  result of calling f with no arguments.  If coll has only 1 item, it
  is returned and f is not called.  If val is supplied, returns the
  result of applying f to val and the first item in coll, then
  applying f to that result and the 2nd item, etc. If coll contains no
  items, returns val and f is not called."
  ([f coll]
     (cond
       (corale.core/implements? IReduce coll)
       (-reduce ^not-native coll f)

       (nil? coll)
       (array-reduce (array) f)

       (array? coll)
       (array-reduce coll f)

       (string? coll)
       (array-reduce coll f)

       (native-satisfies? IReduce coll)
       (-reduce coll f)

       :else
       (array-reduce (arr coll) f)))
  ([f val coll]
     (cond
       (corale.core/implements? IReduce coll)
       (-reduce ^not-native coll f val)

       (nil? coll)
       (array-reduce (array) f val)

       (array? coll)
       (array-reduce coll f val)
      
       (string? coll)
       (array-reduce coll f val)

       (native-satisfies? IReduce coll)
       (-reduce coll f val)

       :else
       (array-reduce (arr coll) f val))))

(defn- array-kv-reduce [f init coll]
  (let [l (alength coll)]
    (loop [i 0 init init]
      (if (< i l)
        (let [init (f init i (aget coll i))]
          (if (reduced? init)
            (deref init)
            (recur (inc i) init)))
        init))))

(defn- object-kv-reduce [f init coll]
  (let [coll-keys (o/getKeys coll)
        l (alength coll-keys)]
    (loop [i 0 init init]
      (if (< i l)
        (let [k (aget coll-keys i)
              init (f init k (get coll k))]
          (if (reduced? init)
            (deref init)
            (recur (inc i) init)))
        init))))

(defn reduce-kv
  "Reduces an associative collection. f should be a function of 3
  arguments. Returns the result of applying f to init, the first key
  and the first value in coll, then applying f to that result and the
  2nd key and value, etc. If coll contains no entries, returns init
  and f is not called. Note that reduce-kv is supported on arrays,
  where the keys will be the ordinals."
  ([f init coll]
   (cond (nil? coll) init
         (array? coll) (array-kv-reduce f init coll)
         (object? coll) (object-kv-reduce f init coll)
         :else (-kv-reduce coll f init))))

(defn identity
  "Returns its argument."
  [x] x)

(defn completing
  "Takes a reducing function f of 2 args and returns a fn suitable for
  transduce by adding an arity-1 signature that calls cf (default -
  identity) on the result argument."
  ([f] (completing f identity))
  ([f cf]
    (fn
      ([] (f))
      ([x] (cf x))
      ([x y] (f x y)))))

(defn transduce
  "reduce with a transformation of f (xf). If init is not
  supplied, (f) will be called to produce it. f should be a reducing
  step function that accepts both 1 and 2 arguments, if it accepts
  only 2 you can add the arity-1 with 'completing'. Returns the result
  of applying (the transformed) xf to init and the first item in coll,
  then applying xf to that result and the 2nd item, etc. If coll
  contains no items, returns init and f is not called. Note that
  certain transforms may inject or skip items."
  ([xform f coll] (transduce xform f (f) coll))
  ([xform f init coll]
     (let [f (xform f)
           ret (reduce f init coll)]
       (f ret))))

;;; Math - variadic forms will not work until the following implemented:
;;; first, next, reduce

(defn ^number +
  "Returns the sum of nums. (+) returns 0."
  ([] 0)
  ([x] x)
  ([x y] (corale.core/+ x y))
  ([x y & more]
    (reduce + (corale.core/+ x y) more)))

(defn ^number -
  "If no ys are supplied, returns the negation of x, else subtracts
  the ys from x and returns the result."
  ([x] (corale.core/- x))
  ([x y] (corale.core/- x y))
  ([x y & more] (reduce - (corale.core/- x y) more)))

(defn ^number *
  "Returns the product of nums. (*) returns 1."
  ([] 1)
  ([x] x)
  ([x y] (corale.core/* x y))
  ([x y & more] (reduce * (corale.core/* x y) more)))

(declare divide)

(defn ^number /
  "If no denominators are supplied, returns 1/numerator,
  else returns numerator divided by all of the denominators."
  ([x] (corale.core// 1 x))
  ([x y] (corale.core/divide x y)) ;; FIXME: waiting on cljs.core//
  ([x y & more] (reduce / (/ x y) more)))

(defn ^boolean <
  "Returns non-nil if nums are in monotonically increasing order,
  otherwise false."
  ([x] true)
  ([x y] (corale.core/< x y))
  ([x y & more]
   (if (corale.core/< x y)
     (let [l (alength more)]
       (loop [y y
              i 0]
         (if (< i l)
           (if (corale.core/< y (aget more i))
             (recur (aget more i) (inc i))
             false)
           true)))
     false)))

(defn ^boolean <=
  "Returns non-nil if nums are in monotonically non-decreasing order,
  otherwise false."
  ([x] true)
  ([x y] (corale.core/<= x y))
  ([x y & more]
   (if (corale.core/<= x y)
     (let [l (alength more)]
       (loop [y y
              i 0]
         (if (< i l)
           (if (corale.core/<= y (aget more i))
             (recur (aget more i) (inc i))
             false)
           true)))
     false)))

(defn ^boolean >
  "Returns non-nil if nums are in monotonically decreasing order,
  otherwise false."
  ([x] true)
  ([x y] (corale.core/> x y))
  ([x y & more]
   (if (corale.core/> x y)
     (let [l (alength more)]
       (loop [y y
              i 0]
         (if (< i l)
           (if (corale.core/> y (aget more i))
             (recur (aget more i) (inc i))
             false)
           true)))
     false)))

(defn ^boolean >=
  "Returns non-nil if nums are in monotonically non-increasing order,
  otherwise false."
  ([x] true)
  ([x y] (corale.core/>= x y))
  ([x y & more]
   (if (corale.core/>= x y)
     (let [l (alength more)]
       (loop [y y
              i 0]
         (if (< i l)
           (if (corale.core/>= y (aget more i))
             (recur (aget more i) (inc i))
             false)
           true)))
     false)))

(defn dec
  "Returns a number one less than num."
  [x] (- x 1))

(defn ^number max
  "Returns the greatest of the nums."
  ([x] x)
  ([x y] (corale.core/max x y))
  ([x y & more]
   (reduce max (corale.core/max x y) more)))

(defn ^number min
  "Returns the least of the nums."
  ([x] x)
  ([x y] (corale.core/min x y))
  ([x y & more]
   (reduce min (corale.core/min x y) more)))

(defn ^number byte [x] x)

(defn char
  "Coerce to char"
  [x]
  (cond
    (number? x) (.fromCharCode js/String x)
    (and (string? x) (== (.-length x) 1)) x
    :else (throw (js/Error. "Argument to char must be a character or number"))))

(defn ^number short [x] x)
(defn ^number float [x] x)
(defn ^number double [x] x)

(defn ^number unchecked-byte [x] x)
(defn ^number unchecked-char [x] x)
(defn ^number unchecked-short [x] x)
(defn ^number unchecked-float [x] x)
(defn ^number unchecked-double [x] x)

(defn ^number unchecked-add
  "Returns the sum of nums. (+) returns 0."
  ([] 0)
  ([x] x)
  ([x y] (corale.core/unchecked-add x y))
  ([x y & more] (reduce unchecked-add (corale.core/unchecked-add x y) more)))

(defn ^number unchecked-add-int
  "Returns the sum of nums. (+) returns 0."
  ([] 0)
  ([x] x)
  ([x y] (corale.core/unchecked-add-int x y))
  ([x y & more] (reduce unchecked-add-int (corale.core/unchecked-add-int x y) more)))

(defn unchecked-dec
  "Returns a number one less than x, an int."
  [x]
  (corale.core/unchecked-dec x))

(defn unchecked-dec-int
  "Returns a number one less than x, an int."
  [x]
  (corale.core/unchecked-dec-int x))

(defn ^number unchecked-divide-int
  "If no denominators are supplied, returns 1/numerator,
  else returns numerator divided by all of the denominators."
  ([x] (unchecked-divide-int 1 x))
  ([x y] (corale.core/divide x y)) ;; FIXME: waiting on cljs.core//
  ([x y & more] (reduce unchecked-divide-int (unchecked-divide-int x y) more)))

(defn unchecked-inc [x]
  (corale.core/unchecked-inc x))

(defn unchecked-inc-int [x]
  (corale.core/unchecked-inc-int x))

(defn ^number unchecked-multiply
  "Returns the product of nums. (*) returns 1."
  ([] 1)
  ([x] x)
  ([x y] (corale.core/unchecked-multiply x y))
  ([x y & more] (reduce unchecked-multiply (corale.core/unchecked-multiply x y) more)))

(defn ^number unchecked-multiply-int
  "Returns the product of nums. (*) returns 1."
  ([] 1)
  ([x] x)
  ([x y] (corale.core/unchecked-multiply-int x y))
  ([x y & more] (reduce unchecked-multiply-int (corale.core/unchecked-multiply-int x y) more)))

(defn unchecked-negate [x]
  (corale.core/unchecked-negate x))

(defn unchecked-negate-int [x]
  (corale.core/unchecked-negate-int x))

(declare mod)

(defn unchecked-remainder-int [x n]
  (corale.core/unchecked-remainder-int x n))

(defn ^number unchecked-subtract
  "If no ys are supplied, returns the negation of x, else subtracts
  the ys from x and returns the result."
  ([x] (corale.core/unchecked-subtract x))
  ([x y] (corale.core/unchecked-subtract x y))
  ([x y & more] (reduce unchecked-subtract (corale.core/unchecked-subtract x y) more)))

(defn ^number unchecked-subtract-int
  "If no ys are supplied, returns the negation of x, else subtracts
  the ys from x and returns the result."
  ([x] (corale.core/unchecked-subtract-int x))
  ([x y] (corale.core/unchecked-subtract-int x y))
  ([x y & more] (reduce unchecked-subtract-int (corale.core/unchecked-subtract-int x y) more)))

(defn- ^number fix [q]
  (if (>= q 0)
    (Math/floor q)
    (Math/ceil q)))

(defn int
  "Coerce to int by stripping decimal places."
  [x]
  (bit-or x 0))

(defn unchecked-int
  "Coerce to int by stripping decimal places."
  [x]
  (fix x))

(defn long
  "Coerce to long by stripping decimal places. Identical to `int'."
  [x]
  (fix x))

(defn unchecked-long
  "Coerce to long by stripping decimal places. Identical to `int'."
  [x]
  (fix x))

(defn booleans [x] x)
(defn bytes [x] x)
(defn chars [x] x)
(defn shorts [x] x)
(defn ints [x] x)
(defn floats [x] x)
(defn doubles [x] x)
(defn longs [x] x)

(defn js-mod
  "Modulus of num and div with original javascript behavior. i.e. bug for negative numbers"
  [n d]
  (corale.core/js-mod n d))

(defn mod
  "Modulus of num and div. Truncates toward negative infinity."
  [n d]
  (js-mod (+ (js-mod n d) d) d))

(defn quot
  "quot[ient] of dividing numerator by denominator."
  [n d]
  (let [rem (js-mod n d)]
    (fix (/ (- n rem) d))))

(defn rem
  "remainder of dividing numerator by denominator."
  [n d]
  (let [q (quot n d)]
    (- n (* d q))))

(defn bit-xor
  "Bitwise exclusive or"
  ([x y] (corale.core/bit-xor x y))
  ([x y & more]
     (reduce bit-xor (corale.core/bit-xor x y) more)))

(defn bit-and
  "Bitwise and"
  ([x y] (corale.core/bit-and x y))
  ([x y & more]
     (reduce bit-and (corale.core/bit-and x y) more)))

(defn bit-or
  "Bitwise or"
  ([x y] (corale.core/bit-or x y))
  ([x y & more]
     (reduce bit-or (corale.core/bit-or x y) more)))

(defn bit-and-not
  "Bitwise and with complement"
  ([x y] (corale.core/bit-and-not x y))
  ([x y & more]
     (reduce bit-and-not (corale.core/bit-and-not x y) more)))

(defn bit-clear
  "Clear bit at index n"
  [x n]
  (corale.core/bit-clear x n))

(defn bit-flip
  "Flip bit at index n"
  [x n]
  (corale.core/bit-flip x n))

(defn bit-not
  "Bitwise complement"
  [x] (corale.core/bit-not x))

(defn bit-set
  "Set bit at index n"
  [x n]
  (corale.core/bit-set x n))

(defn ^boolean bit-test
  "Test bit at index n"
  [x n]
  (corale.core/bit-test x n))

(defn bit-shift-left
  "Bitwise shift left"
  [x n] (corale.core/bit-shift-left x n))

(defn bit-shift-right
  "Bitwise shift right"
  [x n] (corale.core/bit-shift-right x n))

(defn bit-shift-right-zero-fill
  "DEPRECATED: Bitwise shift right with zero fill"
  [x n] (corale.core/bit-shift-right-zero-fill x n))

(defn unsigned-bit-shift-right
  "Bitwise shift right with zero fill"
  [x n] (corale.core/unsigned-bit-shift-right x n))

(defn bit-count
  "Counts the number of bits set in n"
  [v]
  (let [v (- v (bit-and (bit-shift-right v 1) 0x55555555))
        v (+ (bit-and v 0x33333333) (bit-and (bit-shift-right v 2) 0x33333333))]
    (bit-shift-right (* (bit-and (+ v (bit-shift-right v 4)) 0xF0F0F0F) 0x1010101) 24)))

(defn ^boolean ==
  "Returns non-nil if nums all have the equivalent
  value, otherwise false. Behavior on non nums is
  undefined."
  ([x] true)
  ([x y] (-equiv x y))
  ([x y & more]
   (let [l (alength more)]
     (loop [x x
            y y
            more more
            i 0]
       (if (== x y)
         (if (== i l)
           true
           (recur y (aget more i) more (inc i)))
         false)))))

(defn ^boolean pos?
  "Returns true if num is greater than zero, else false"
  [x] (corale.core/pos? x))

(defn ^boolean zero?
  "Returns true if num is zero, else false"
  [x]
  (corale.core/zero? x))

(defn ^boolean neg?
  "Returns true if num is less than zero, else false"
  [x] (corale.core/neg? x))


;;;;;;;;;;;;;;;;;;;;;;;;;; basics ;;;;;;;;;;;;;;;;;;

(defn str
  "With no args, returns the empty string. With one arg x, returns
  x.toString().  (str nil) returns the empty string. With more than
  one arg, returns the concatenation of the str values of the args."
  ([] "")
  ([x] (if (nil? x)
         ""
         (.join #js [x] "")))
  ([x & ys]
   (let [l (alength ys)]
     (loop [sb (StringBuffer. (str x))
            i 0]
       (if (< i l)
         (recur (. sb  (append (str (aget ys i)))) (inc i))
         (.toString sb))))))

(defn subs
  "Returns the substring of s beginning at start inclusive, and ending
  at end (defaults to length of string), exclusive."
  ([s start] (.substring s start))
  ([s start end] (.substring s start end)))

(declare name)

(defn reverse
  "Returns an array of the items in coll in reverse order."
  [coll]
  (cond (nil? coll) (array)
        (array? coll) (.reverse (aclone coll))
        :else (if-let [a (arr coll)]
                (.reverse a)
                (array))))

(defn cons
  "Returns a new array where x is the first element and coll is the rest."
  [x coll]
  (if (nil? coll) (array x)
      (let [a (if (array? coll) (aclone coll) (arr coll))]
        (.unshift a x)
        a)))

(defn hash-keyword [k]
  (int (+ (hash-symbol k) 0x9e3779b9)))

(defn- compare-keywords [a b]
  (cond
    (identical? (.-fqn a) (.-fqn b)) 0
    (and (not (.-ns a)) (.-ns b)) -1
    (.-ns a) (corale.core/if-not (.-ns b)
               1
               (let [nsc (garray/defaultCompare (.-ns a) (.-ns b))]
                 (if (== 0 nsc)
                   (garray/defaultCompare (.-name a) (.-name b))
                   nsc)))
    :default (garray/defaultCompare (.-name a) (.-name b))))

(deftype Keyword [ns name fqn ^:mutable _hash]
  Object
  (toString [_] (str ":" fqn))
  (equiv [this other]
    (-equiv this other))
  
  IEquiv
  (-equiv [_ other]
    (if (instance? Keyword other)
      (identical? fqn (.-fqn other))
      false))
  IFn
  (-invoke [kw coll]
    (get coll kw))
  (-invoke [kw coll not-found]
    (get coll kw not-found))

  IHash
  (-hash [this]
    (corale.core/caching-hash this hash-keyword _hash))

  INamed
  (-name [_] name)
  (-namespace [_] ns)

  IPrintWithWriter
  (-pr-writer [o writer _] (-write writer (str ":" fqn)))

  ;; printing at the REPL calls cljs.core function
  cljs.core/IPrintWithWriter
  (cljs.core/-pr-writer [o writer _] (-write writer (str ":" fqn))))

(defn ^boolean keyword?
  "Return true if x is a Keyword"
  [x]
  (instance? Keyword x))

(defn ^boolean keyword-identical?
  "Efficient test to determine that two keywords are identical."
  [x y]
  (if (identical? x y)
    true
    (if (and (keyword? x) (keyword? y))
      (identical? (.-fqn x) (.-fqn y))
      false)))

(defn ^boolean symbol-identical?
  "Efficient test to determine that two symbols are identical."
  [x y]
  (if (identical? x y)
    true
    (if (and (symbol? x) (symbol? y))
      (identical? (.-str x) (.-str y))
      false)))

(defn namespace
  "Returns the namespace String of a symbol or keyword, or nil if not present."
  [x]
  (if (corale.core/implements? INamed x)
    (-namespace ^not-native x)
    (throw (js/Error. (str "Doesn't support namespace: " x)))))

(defn ^boolean ident?
  "Return true if x is a symbol or keyword"
  [x] (or (keyword? x) (symbol? x)))

(defn ^boolean simple-ident?
  "Return true if x is a symbol or keyword without a namespace"
  [x] (and (ident? x) (nil? (namespace x))))

(defn ^boolean qualified-ident?
  "Return true if x is a symbol or keyword with a namespace"
  [x] (and (ident? x) (namespace x) true))

(defn ^boolean simple-symbol?
  "Return true if x is a symbol without a namespace"
  [x] (and (symbol? x) (nil? (namespace x))))

(defn ^boolean qualified-symbol?
  "Return true if x is a symbol with a namespace"
  [x] (and (symbol? x) (namespace x) true))

(defn ^boolean simple-keyword?
  "Return true if x is a keyword without a namespace"
  [x] (and (keyword? x) (nil? (namespace x))))

(defn ^boolean qualified-keyword?
  "Return true if x is a keyword with a namespace"
  [x] (and (keyword? x) (namespace x) true))

(defn keyword
  "Returns a Keyword with the given namespace and name.  Do not use :
  in the keyword strings, it will be added automatically."
  ([name] (cond
            (keyword? name) name
            (symbol? name) (Keyword.
                             (corale.core/namespace name)
                             (corale.core/name name) (.-str name) nil)
            (string? name) (let [parts (.split name "/")]
                             (if (== (alength parts) 2)
                               (Keyword. (aget parts 0) (aget parts 1) name nil)
                               (Keyword. nil (aget parts 0) name nil)))))
  ([ns name]
   (let [ns   (cond
                (keyword? ns) (corale.core/name ns)
                (symbol? ns)  (corale.core/name ns)
                :else ns)
         name (cond
                (keyword? name) (corale.core/name name)
                (symbol? name) (corale.core/name name)
                :else name)]
     (Keyword. ns name (str (when ns (str ns "/")) name) nil))))

(defn to-array
  "Naive impl of to-array as a start."
  [s]
  (if (nil? s)
    (array)
    (arr s)))

(defn to-array-2d
  "Returns a (potentially-ragged) 2-dimensional array
  containing the contents of coll."
  [coll]
  (if (nil? coll)
    (array)
    (let [a (array)]
      (doseq [x coll]
        (.push a (arr x)))
      a)))

(defn int-array
  "Creates an array of ints. Does not coerce array, provided for compatibility
  with Clojure."
  ([size-or-seq]
     (if (number? size-or-seq)
       (int-array size-or-seq nil)
       (into-array size-or-seq)))
  ([size init-val-or-seq]
     (let [a (make-array size)]
       (if (array? init-val-or-seq)
         (loop [i 0]
           (if (< i size)
             (do
               (aset a i (aget init-val-or-seq i))
               (recur (inc i)))
             a))
         (do
           (dotimes [i size]
             (aset a i init-val-or-seq))
           a)))))

(defn long-array
  "Creates an array of longs. Does not coerce array, provided for compatibility
  with Clojure."
  ([size-or-seq]
     (if (number? size-or-seq)
       (long-array size-or-seq nil)
       (into-array size-or-seq)))
  ([size init-val-or-seq]
     (let [a (make-array size)]
       (if (array? init-val-or-seq)
         (loop [i 0]
           (if (< i size)
             (do
               (aset a i (aget init-val-or-seq i))
               (recur (inc i)))
             a))
         (do
           (dotimes [i size]
             (aset a i init-val-or-seq))
           a)))))

(defn double-array
  "Creates an array of doubles. Does not coerce array, provided for compatibility
  with Clojure."
  ([size-or-seq]
     (if (number? size-or-seq)
       (double-array size-or-seq nil)
       (into-array size-or-seq)))
  ([size init-val-or-seq]
   (let [a (make-array size)]
     (if (array? init-val-or-seq)
       (loop [i 0]
         (if (< i size)
           (do
             (aset a i (aget init-val-or-seq i))
             (recur (inc i)))
           a))
       (do
         (dotimes [i size]
           (aset a i init-val-or-seq))
         a)))))

(defn object-array
  "Creates an array of objects. Does not coerce array, provided for compatibility
  with Clojure."
  ([size-or-seq]
     (if (number? size-or-seq)
       (object-array size-or-seq nil)
       (into-array size-or-seq)))
  ([size init-val-or-seq]
     (let [a (make-array size)]
       (if (array? init-val-or-seq)
         (loop [i 0]
           (if (< i size)
             (do
               (aset a i (aget init-val-or-seq i))
               (recur (inc i)))
             a))
         (do
           (dotimes [i size]
             (aset a i init-val-or-seq))
           a)))))

(defn concat
  "Returns the concatenation of the elements in the supplied colls."
  ([] (array))
  ([x] (arr x))
  ([x y]
   (.concat (arr x) (arr y)))
  ([x y & zs]
   (let [copy (aclone x)]
     (doseq [e (arr y)]
       (.push copy e))
     (doseq [z zs]
       (doseq [e (arr z)]
         (.push copy e)))
     copy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; apply ;;;;;;;;;;;;;;;;

;; see core.clj
(corale.core/gen-apply-to)

(set! cljs.core/*unchecked-if* true)
(defn apply
  "Applies fn f to the argument array formed by prepending intervening arguments to args."
  ([f args]
   (let [fixed-arity (.-cljs$lang$maxFixedArity f)]
     (if (.-cljs$lang$applyTo f)
       (let [bc (count (arr args))]
         (if (<= bc fixed-arity)
           (apply-to f bc args)
           (.cljs$lang$applyTo f args)))
       (.apply f f args))))
  ([f x args]
   (let [arglist (concat (array x) (arr args))
         fixed-arity (.-cljs$lang$maxFixedArity f)]
     (if (.-cljs$lang$applyTo f)
       (let [bc (alength arglist)]
         (if (<= bc fixed-arity)
           (apply-to f bc arglist)
           (.cljs$lang$applyTo f arglist)))
       (.apply f f arglist))))
  ([f x y args]
   (let [arglist (concat (array x y) (arr args))
         fixed-arity (.-cljs$lang$maxFixedArity f)]
     (if (.-cljs$lang$applyTo f)
       (let [bc (alength arglist)]
         (if (<= bc fixed-arity)
           (apply-to f bc arglist)
           (.cljs$lang$applyTo f arglist)))
       (.apply f f arglist))))
  ([f x y z args]
   (let [arglist (concat (array x y z) (arr args))
         fixed-arity (.-cljs$lang$maxFixedArity f)]
     (if (.-cljs$lang$applyTo f)
       (let [bc (alength arglist)]
         (if (<= bc fixed-arity)
           (apply-to f bc arglist)
           (.cljs$lang$applyTo f arglist)))
       (.apply f f arglist))))
  ([f a b c d & args]
   (let [arglist (array a b c d)
         rest (.pop args)
         _ (doseq [a args] (.push arglist a))
         _ (doseq [r rest] (.push arglist r))
         fixed-arity (.-cljs$lang$maxFixedArity f)]
     (if (.-cljs$lang$applyTo f)
       (let [bc (alength arglist)]
         (if (<= bc fixed-arity)
           (apply-to f bc arglist)
           (.cljs$lang$applyTo f arglist)))
       (.apply f f arglist)))))
(set! cljs.core/*unchecked-if* false)

(defn ^boolean not=
  "Same as (not (= obj1 obj2))"
  ([x] false)
  ([x y] (not (= x y)))
  ([x y & more]
   (not (apply = x y more))))

;;;;;;;;;

;; Used during destructuring
(defn- find-in-arr [arr k]
  (let [l (alength arr)]
    (loop [i 0]
      (when (< i l)
        (if (= k (aget arr i))
          (aget arr (inc i))
          (recur (+ i 2)))))))

;; Symbol is imported from cljs.core anyway

;;;;;;;;;;;;;;;;

;; when-let
;; Fn type -> cljs.core at the moment

;; find a way to easy diff cljs.core with corale (macros)

;;;;;;;;;;;;;;;

;; ci-reduce, array-reduce not implemented

;; fast-path-protocols
;; revert compiler emit :meta, throw error
;; constant table
