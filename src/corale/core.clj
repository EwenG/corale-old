(ns corale.core
  (:refer-clojure :exclude [identical? nil? number? some? string? dotimes instance? symbol?
                            make-array
                            and or if-not
                            alength aclone
                            deftype extend-type defprotocol
                            inc dec + - * / < <= > >= max min
                            false? true?
                            byte short float double unchecked-byte unchecked-char
                            unchecked-short unchecked-float unchecked-double
                            unchecked-add unchecked-add-int unchecked-dec unchecked-dec-int
                            unchecked-divide-int unchecked-inc unchecked-inc-int
                            unchecked-multiply unchecked-multiply-int unchecked-negate
                            unchecked-negate-int unchecked-remainder-int unchecked-subtract
                            unchecked-subtract-int
                            int
                            bit-not bit-and unsafe-bit-and bit-or bit-xor bit-and-not bit-clear
                            bit-flip bit-test bit-shift-left bit-shift-right
                            bit-shift-right-zero-fill unsigned-bit-shift-right bit-set
                            == pos? zero? neg?
                            str keyword?
                            fn defn defn- destructure let aset aget doseq loop])
  (:require [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.env :as env]))

(load "compiler")
(alias 'core 'clojure.core)

(core/defmacro exclude-core []
  `(refer-clojure :exclude
                  '[~'instance? ~'identical? ~'nil? ~'array? ~'number? ~'not ~'some? ~'object?
                    ~'string? ~'char? ~'any? ~'type ~'type->str
                    ~'apply
                    ~'make-array ~'array ~'dotimes
                    ~'and ~'or ~'if-not
                    ~'system-time
                    ~'alength ~'aclone
                    ~'aset ~'aget
                    ~'into-array ~'reduce
                    ~'js-invoke
                    ~'IFn ~'ICollection ~'ILookup ~'IEquiv ~'-invoke ~'-lookup ~'-equiv
                    ~'IWriter ~'IPrintWithWriter ~'StringBufferWriter ~'->StringBufferWriter
                    ~'-write ~'-flush ~'pr-str* ~'-pr-writer
                    ~'int-rotate-left ~'imul ~'m3-seed ~'m3-C1 ~'m3-C2 ~'m3-mix-K1 ~'m3-mix-H1
                    ~'m3-fmix ~'m3-hash-int ~'m3-hash-unencoded-chars
                    ~'string-hash-cache ~'string-hash-cache-count
                    ~'= ~'compare
                    ~'hash-string* ~'add-to-string-hash-cache ~'hash-string ~'hash
                    ~'IHash ~'-hash ~'hash-combine ~'caching-hash
                    ~'Symbol ~'->Symbol ~'symbol? ~'hash-symbol ~'compare-symbols
                    ~'deftype ~'dt->et ~'fast-path-protocols ~'defprotocol ~'missing-protocol
                    ~'fast-path-protocol-partitions-count ~'extend-type ~'extend-prefix
                    ~'get
                    ~'this-as ~'js-this
                    ~'INamed ~'-name ~'-namespace ~'symbol
                    ~'first
                    ~'mix-collection-hash ~'hash-ordered-coll ~'empty-ordered-hash
                    ~'hash-unordered-coll ~'empty-unordered-hash
                    ~'ICounted ~'-count ~'IComparable ~'-compare
                    ~'Inst ~'inst-ms* ~'inst-ms ~'inst?
                    ~'inc ~'dec ~'+
                    ~'IDeref ~'-deref ~'deref
                    ~'Reduced ~'->Reduced ~'reduced ~'reduced? ~'ensure-reduced ~'unreduced
                    ~'IIndexed ~'-nth ~'counted? ~'indexed?
                    ~'cons ~'drop ~'count ~'nth
                    ~'second ~'ffirst ~'last
                    ~'conj ~'-conj
                    ~'IEmptyableCollection ~'-empty ~'empty
                    ~'IAssociative ~'-contains-key? ~'-assoc ~'assoc
                    ~'IMap ~'dissoc ~'-dissoc
                    ~'fn?
                    ~'ISet ~'-disjoin ~'disj
                    ~'IStack ~'-peek ~'-pop ~'peek ~'pop
                    ~'empty? ~'coll? ~'set?
                    ~'IReduce ~'-reduce ~'ISequential
                    ~'ISorted ~'-sorted-seq ~'-sorted-seq-from ~'-entry-key ~'-comparator
                    ~'associative? ~'sequential? ~'sorted? ~'reduceable? ~'map?
                    ~'IRecord ~'record?
                    ~'js-obj ~'js-keys ~'js-delete
                    ~'lookup-sentinel ~'false? ~'true? ~'undefined? ~'boolean?
                    ~'boolean ~'ifn?
                    ~'integer? ~'int? ~'pos-int? ~'neg-int? ~'nat-int? ~'float? ~'double?
                    ~'infinite?
                    ~'find ~'contains? ~'distinct? ~'compare ~'sort ~'sort-by ~'shuffle
                    ~'array-reduce ~'reduce ~'reduce-kv
                    ~'IKVReduce ~'-kv-reduce
                    ~'identity ~'completing ~'transduce
                    ~'+ ~'- ~'* ~'/ ~'divide ~'< ~'<= ~'> ~'>= ~'max ~'min
                    ~'byte ~'short ~'float ~'double ~'char ~'mod
                    ~'unchecked-byte ~'unchecked-char
                    ~'unchecked-short ~'unchecked-float ~'unchecked-double
                    ~'unchecked-add ~'unchecked-add-int ~'unchecked-dec ~'unchecked-dec-int
                    ~'unchecked-divide-int ~'unchecked-inc ~'unchecked-inc-int
                    ~'unchecked-multiply ~'unchecked-multiply-int ~'unchecked-negate
                    ~'unchecked-negate-int ~'unchecked-remainder-int ~'unchecked-subtract
                    ~'unchecked-subtract-int
                    ~'fix ~'int ~'unchecked-int ~'long ~'unchecked-long
                    ~'booleans ~'bytes ~'chars ~'shorts ~'ints ~'floats ~'doubles ~'longs
                    ~'js-mod ~'mod ~'quot ~'rem
                    ~'bit-not ~'bit-and ~'unsafe-bit-and ~'bit-or ~'bit-xor ~'bit-and-not
                    ~'bit-clear ~'bit-flip ~'bit-test ~'bit-shift-left ~'bit-shift-right
                    ~'bit-shift-right-zero-fill ~'unsigned-bit-shift-right ~'bit-set ~'bit-count
                    ~'== ~'pos? ~'zero? ~'neg?
                    ~'str ~'subs ~'reverse
                    ~'Keyword ~'->Keyword ~'hash-keyword ~'compare-keywords
                    ~'keyword? ~'keyword-identical? ~'symbol-identical? ~'namespace ~'name
                    ~'ident? ~'simple-ident? ~'qualified-ident? ~'simple-symbol?
                    ~'qualified-symbol? ~'simple-keyword? ~'qualified-keyword?
                    ~'keyword
                    ~'to-array ~'to-array-2d
                    ~'int-array ~'long-array ~'double-array ~'object-array
                    ~'concat ~'spread ~'bounded-count
                    ~'apply-to ~'gen-apply-to
                    ~'not=
                    ~'defn defn- ~'fn ~'let
                    
                    ~'coercive-= ~'coercive-boolean  ~'fn->comparator ~'compare-indexed
                    
                    ~'Atom ~'->Atom ~'atom ~'add-watch ~'remove-watch ~'reset! ~'swap!
                    ~'compare-and-set! ~'set-validator! ~'get-validator]))

(core/defmacro require-corale []
  `(require '[corale.core :refer
              [~'instance? ~'identical? ~'nil? ~'array? ~'number? ~'not ~'some? ~'object?
               ~'string? ~'char? ~'any? ~'type
               ~'apply
               ~'make-array ~'array
               ~'and ~'or
               ~'system-time
               ~'alength ~'aclone
               ~'aset ~'aget
               ~'into-array ~'reduce
               ~'js-invoke
               ~'IFn ~'ICollection ~'ILookup ~'IEquiv ~'-lookup ~'-equiv ~'-invoke
               ~'IWriter ~'IPrintWithWriter ~'StringBufferWriter ~'->StringBufferWriter
               ~'-write ~'-flush ~'pr-str* ~'-pr-writer
               ~'int-rotate-left
               ~'= ~'compare
               ~'hash ~'IHash ~'-hash
               ~'Symbol ~'->Symbol ~'symbol? ~'compare-symbols
               ~'deftype ~'defprotocol
               ~'extend-type
               ~'get
               ~'this-as ~'js-this
               ~'INamed ~'-name ~'-namespace ~'symbol
               ~'first
               ~'ICounted ~'-count ~'IComparable ~'-compare
               ~'Inst ~'inst-ms* ~'inst-ms ~'inst?
               ~'inc ~'dec ~'+
               ~'IDeref ~'-deref ~'deref
               ~'Reduced ~'->Reduced ~'reduced ~'reduced? ~'ensure-reduced ~'unreduced
               ~'IIndexed ~'-nth ~'counted? ~'indexed?
               ~'cons ~'drop ~'count ~'nth
               ~'second ~'ffirst ~'last
               ~'conj ~'-conj
               ~'IEmptyableCollection ~'-empty ~'empty
               ~'IAssociative ~'-contains-key? ~'-assoc ~'assoc
               ~'IMap ~'dissoc ~'-dissoc
               ~'fn?
               ~'ISet ~'-disjoin ~'disj
               ~'IStack ~'-peek ~'-pop
               ~'empty? ~'coll? ~'set?
               ~'IReduce ~'-reduce ~'ISequential
               ~'ISorted ~'-sorted-seq ~'-sorted-seq-from ~'-entry-key ~'-comparator
               ~'associative? ~'sequential? ~'sorted? ~'reduceable? ~'map?
               ~'IRecord ~'record?
               ~'js-obj ~'js-keys ~'js-delete
               ~'false? ~'true? ~'undefined? ~'boolean?
               ~'boolean ~'ifn?
               ~'integer? ~'int? ~'pos-int? ~'neg-int? ~'nat-int? ~'float? ~'double?
               ~'infinite?
               ~'find ~'contains? ~'distinct? ~'compare ~'sort ~'sort-by ~'shuffle
               ~'array-reduce ~'reduce ~'reduce-kv
               ~'IKVReduce ~'-kv-reduce
               ~'identity ~'completing ~'transduce
               ~'+ ~'- ~'* ~'/ ~'divide ~'< ~'<= ~'> ~'>= ~'max ~'min
               ~'byte ~'short ~'float ~'double ~'char
               ~'unchecked-byte ~'unchecked-char 
               ~'unchecked-short ~'unchecked-float ~'unchecked-double
               ~'unchecked-add ~'unchecked-add-int ~'unchecked-dec ~'unchecked-dec-int
               ~'unchecked-divide-int ~'unchecked-inc ~'unchecked-inc-int
               ~'unchecked-multiply ~'unchecked-multiply-int ~'unchecked-negate
               ~'unchecked-negate-int ~'unchecked-remainder-int ~'unchecked-subtract
               ~'unchecked-subtract-int
               ~'fix ~'int ~'unchecked-int ~'long ~'unchecked-long
               ~'booleans ~'bytes ~'chars ~'shorts ~'ints ~'floats ~'doubles ~'longs
               ~'js-mod ~'mod ~'quot ~'rem
               ~'bit-not ~'bit-and ~'unsafe-bit-and ~'bit-or ~'bit-xor ~'bit-and-not
               ~'bit-clear ~'bit-flip ~'bit-test ~'bit-shift-left ~'bit-shift-right
               ~'bit-shift-right-zero-fill ~'unsigned-bit-shift-right ~'bit-set ~'bit-count
               ~'== ~'pos? ~'zero? ~'neg?
               ~'str ~'subs ~'reverse
               ~'Keyword ~'->Keyword ~'hash-keyword ~'compare-keywords
               ~'keyword? ~'keyword-identical? ~'symbol-identical? ~'namespace ~'name
               ~'ident? ~'simple-ident? ~'qualified-ident? ~'simple-symbol?
               ~'qualified-symbol? ~'simple-keyword? ~'qualified-keyword?
               ~'keyword
               ~'to-array ~'to-array-2d
               ~'int-array ~'long-array ~'double-array ~'object-array
               ~'concat
               ~'not=

               ~'arr ~'IArrayable ~'-arr
               
               ~'defn ~'fn ~'let]]))

(defmacro require-corale-macros []
  (ana/parse 'ns* &env
             '(ns*
               (:require-macros
                '[corale.core :refer
                  [exclude-core defn fn let loop dotimes and or
                   alength aget aset array js-obj extend-type deftype
                   defprotocol this-as + false? inc true? zero?
                   < > + - == / * int bit-xor bit-or bit-shift-left
                   unsigned-bit-shift-right bit-and js-mod bit-shift-right
                   str dec >= pos? neg? keyword? symbol? doseq unchecked-inc
                   coercive-boolean if-not defn-]]))
             nil nil)
  nil)

(def fast-path-protocols
  "protocol fqn -> [partition number, bit]"
  (zipmap (map #(symbol "corale.core" (core/str %))
               '[IFn ICounted IEmptyableCollection ICollection IIndexed ASeq ISeq INext
                 ILookup IAssociative IMap IMapEntry ISet IStack IVector IDeref
                 IDerefWithTimeout IMeta IWithMeta IReduce IKVReduce IEquiv IHash
                 ISeqable ISequential IList IRecord IReversible ISorted IPrintWithWriter IWriter
                 IPrintWithWriter IPending IWatchable IEditableCollection ITransientCollection
                 ITransientAssociative ITransientMap ITransientVector ITransientSet
                 IMultiFn IChunkedSeq IChunkedNext IComparable INamed ICloneable IAtom
                 IReset ISwap])
          (iterate (core/fn [[p b]]
                     (if (core/== 2147483648 b)
                       [(core/inc p) 1]
                       [p (core/bit-shift-left b 1)]))
                   [0 1])))

(def fast-path-protocol-partitions-count
  "total number of partitions"
  (core/let [c (count fast-path-protocols)
             m (core/mod c 32)]
    (if (core/zero? m)
      (core/quot c 32)
      (core/inc (core/quot c 32)))))

(core/defmacro str [& xs]
  (core/let [strs (core/->> (repeat (count xs) "corale.core.str.cljs$core$IFn$_invoke$arity$1(~{})")
                    (interpose ",")
                    (apply core/str))]
    (list* 'js* (core/str "[" strs "].join('')") xs)))

(core/defn- bool-expr [e]
  (vary-meta e assoc :tag 'boolean))

(core/defn- simple-test-expr? [env ast]
  (core/and
    (#{:var :invoke :constant :dot :js} (:op ast))
    ('#{boolean seq} (cljs.analyzer/infer-tag env ast))))

(core/defmacro and
  "Evaluates exprs one at a time, from left to right. If a form
  returns logical false (nil or false), and returns that value and
  doesn't evaluate any of the other expressions, otherwise it returns
  the value of the last expr. (and) returns true."
  ([] true)
  ([x] x)
  ([x & next]
   (core/let [forms (concat [x] next)]
     (if (every? #(simple-test-expr? &env %)
                 (map #(cljs.analyzer/analyze &env %) forms))
       (core/let [and-str (core/->> (repeat (count forms) "(~{})")
                                    (interpose " && ")
                                    (apply core/str))]
         (bool-expr `(~'js* ~and-str ~@forms)))
       `(let [and# ~x]
          (if and# (and ~@next) and#))))))

(core/defmacro or
  "Evaluates exprs one at a time, from left to right. If a form
  returns a logical true value, or returns that value and doesn't
  evaluate any of the other expressions, otherwise it returns the
  value of the last expression. (or) returns nil."
  ([] nil)
  ([x] x)
  ([x & next]
   (core/let [forms (concat [x] next)]
     (if (every? #(simple-test-expr? &env %)
                 (map #(cljs.analyzer/analyze &env %) forms))
       (core/let [or-str (core/->> (repeat (count forms) "(~{})")
                                   (interpose " || ")
                                   (apply core/str))]
         (bool-expr `(~'js* ~or-str ~@forms)))
       `(let [or# ~x]
          (if or# or# (or ~@next)))))))

(core/defmacro nil? [x]
  `(coercive-= ~x nil))

(core/defmacro some? [x]
  `(corale.core/not (nil? ~x)))

(core/defmacro if-not
  "Evaluates test. If logical false, evaluates and returns then expr, 
  otherwise else expr, if supplied, else nil."
  ([test then] `(if-not ~test ~then nil))
  ([test then else]
   `(if (corale.core/not ~test) ~then ~else)))

(core/defmacro coercive-= [x y]
  (bool-expr (core/list 'js* "(~{} == ~{})" x y)))

(core/defmacro coercive-boolean [x]
  (with-meta (core/list 'js* "~{}" x)
    {:tag 'boolean}))

(core/defmacro string? [x]
  (bool-expr (core/list 'js* "typeof ~{} === 'string'" x)))

;; TODO: x must be a symbol, not an arbitrary expression
(core/defmacro exists?
  "Return true if argument exists, analogous to usage of typeof operator
   in JavaScript."
  [x]
  (bool-expr
    (core/list 'js* "typeof ~{} !== 'undefined'"
               (vary-meta x assoc :cljs.analyzer/no-resolve true))))

(core/defmacro undefined?
  "Return true if argument is identical to the JavaScript undefined value."
  [x]
  (bool-expr (core/list 'js* "(void 0 === ~{})" x)))

(core/defmacro identical? [a b]
  (bool-expr (core/list 'js* "(~{} === ~{})" a b)))

(core/defmacro instance? [c x]
  ;; Google Closure warns about some references to RegExp, so
  ;; (instance? RegExp ...) needs to be inlined, but the expansion
  ;; should preserve the order of argument evaluation.
  (bool-expr (if (clojure.core/symbol? c)
               (core/list 'js* "(~{} instanceof ~{})" x c)
               `(let [c# ~c x# ~x]
                  (~'js* "(~{} instanceof ~{})" x# c#)))))

(core/defmacro number? [x]
  (bool-expr (core/list 'js* "typeof ~{} === 'number'" x)))

(core/defmacro symbol? [x]
  (bool-expr `(instance? corale.core/Symbol ~x)))

(core/defmacro keyword? [x]
  (bool-expr `(instance? corale.core/Keyword ~x)))

(core/defmacro aget
  ([a i]
   (core/list 'js* "(~{}[~{}])" a i))
  ([a i & idxs]
   (core/let [astr (apply core/str (repeat (count idxs) "[~{}]"))]
     `(~'js* ~(core/str "(~{}[~{}]" astr ")") ~a ~i ~@idxs))))

(core/defmacro aset
  ([a i v]
   (core/list 'js* "(~{}[~{}] = ~{})" a i v))
  ([a idx idx2 & idxv]
   (core/let [n    (core/dec (count idxv))
              astr (apply core/str (repeat n "[~{}]"))]
     `(~'js* ~(core/str "(~{}[~{}][~{}]" astr " = ~{})") ~a ~idx ~idx2 ~@idxv))))

(core/defmacro ^::ana/numeric +
  ([] 0)
  ([x] x)
  ([x y] (core/list 'js* "(~{} + ~{})" x y))
  ([x y & more] `(+ (+ ~x ~y) ~@more)))

(core/defmacro byte [x] x)
(core/defmacro short [x] x)
(core/defmacro float [x] x)
(core/defmacro double [x] x)

(core/defmacro unchecked-byte [x] x)
(core/defmacro unchecked-char [x] x)
(core/defmacro unchecked-short [x] x)
(core/defmacro unchecked-float [x] x)
(core/defmacro unchecked-double [x] x)

(core/defmacro ^::ana/numeric unchecked-add
  ([& xs] `(+ ~@xs)))

(core/defmacro ^::ana/numeric unchecked-add-int
  ([& xs] `(+ ~@xs)))

(core/defmacro ^::ana/numeric unchecked-dec
  ([x] `(dec ~x)))

(core/defmacro ^::ana/numeric unchecked-dec-int
  ([x] `(dec ~x)))

(core/defmacro ^::ana/numeric unchecked-divide-int
  ([& xs] `(/ ~@xs)))

(core/defmacro ^::ana/numeric unchecked-inc
  ([x] `(inc ~x)))

(core/defmacro ^::ana/numeric unchecked-inc-int
  ([x] `(inc ~x)))

(core/defmacro ^::ana/numeric unchecked-multiply
  ([& xs] `(* ~@xs)))

(core/defmacro ^::ana/numeric unchecked-multiply-int
  ([& xs] `(* ~@xs)))

(core/defmacro ^::ana/numeric unchecked-negate
  ([x] `(- ~x)))

(core/defmacro ^::ana/numeric unchecked-negate-int
  ([x] `(- ~x)))

(core/defmacro ^::ana/numeric unchecked-remainder-int
  ([x n] `(corale.core/mod ~x ~n)))

(core/defmacro ^::ana/numeric unchecked-subtract
  ([& xs] `(- ~@xs)))

(core/defmacro ^::ana/numeric unchecked-subtract-int
  ([& xs] `(- ~@xs)))

(core/defmacro ^::ana/numeric -
  ([x] (core/list 'js* "(- ~{})" x))
  ([x y] (core/list 'js* "(~{} - ~{})" x y))
  ([x y & more] `(- (- ~x ~y) ~@more)))

(core/defmacro ^::ana/numeric *
  ([] 1)
  ([x] x)
  ([x y] (core/list 'js* "(~{} * ~{})" x y))
  ([x y & more] `(* (* ~x ~y) ~@more)))

(core/defmacro ^::ana/numeric /
  ([x] `(/ 1 ~x))
  ([x y] (core/list 'js* "(~{} / ~{})" x y))
  ([x y & more] `(/ (/ ~x ~y) ~@more)))

(core/defmacro ^::ana/numeric divide
  ([x] `(/ 1 ~x))
  ([x y] (core/list 'js* "(~{} / ~{})" x y))
  ([x y & more] `(/ (/ ~x ~y) ~@more)))

(core/defmacro ^::ana/numeric <
  ([x] true)
  ([x y] (bool-expr (core/list 'js* "(~{} < ~{})" x y)))
  ([x y & more] `(and (< ~x ~y) (< ~y ~@more))))

(core/defmacro ^::ana/numeric <=
  ([x] true)
  ([x y] (bool-expr (core/list 'js* "(~{} <= ~{})" x y)))
  ([x y & more] `(and (<= ~x ~y) (<= ~y ~@more))))

(core/defmacro ^::ana/numeric >
  ([x] true)
  ([x y] (bool-expr (core/list 'js* "(~{} > ~{})" x y)))
  ([x y & more] `(and (> ~x ~y) (> ~y ~@more))))

(core/defmacro ^::ana/numeric >=
  ([x] true)
  ([x y] (bool-expr (core/list 'js* "(~{} >= ~{})" x y)))
  ([x y & more] `(and (>= ~x ~y) (>= ~y ~@more))))

(core/defmacro ^::ana/numeric ==
  ([x] true)
  ([x y] (bool-expr (core/list 'js* "(~{} === ~{})" x y)))
  ([x y & more] `(and (== ~x ~y) (== ~y ~@more))))

(core/defmacro ^::ana/numeric dec [x]
  `(- ~x 1))

(core/defmacro ^::ana/numeric inc [x]
  `(+ ~x 1))

(core/defmacro ^::ana/numeric zero? [x]
  `(== ~x 0))

(core/defmacro ^::ana/numeric pos? [x]
  `(> ~x 0))

(core/defmacro ^::ana/numeric neg? [x]
  `(< ~x 0))

(core/defmacro ^::ana/numeric max
  ([x] x)
  ([x y] `(let [x# ~x, y# ~y]
            (~'js* "((~{} > ~{}) ? ~{} : ~{})" x# y# x# y#)))
  ([x y & more] `(max (max ~x ~y) ~@more)))

(core/defmacro ^::ana/numeric min
  ([x] x)
  ([x y] `(let [x# ~x, y# ~y]
            (~'js* "((~{} < ~{}) ? ~{} : ~{})" x# y# x# y#)))
  ([x y & more] `(min (min ~x ~y) ~@more)))

(core/defmacro ^::ana/numeric js-mod [num div]
  (core/list 'js* "(~{} % ~{})" num div))

(core/defmacro ^::ana/numeric bit-not [x]
  (core/list 'js* "(~ ~{})" x))

(core/defmacro ^::ana/numeric bit-and
  ([x y] (core/list 'js* "(~{} & ~{})" x y))
  ([x y & more] `(bit-and (bit-and ~x ~y) ~@more)))

;; internal do not use
(core/defmacro ^::ana/numeric unsafe-bit-and
  ([x y] (bool-expr (core/list 'js* "(~{} & ~{})" x y)))
  ([x y & more] `(unsafe-bit-and (unsafe-bit-and ~x ~y) ~@more)))

(core/defmacro ^::ana/numeric bit-or
  ([x y] (core/list 'js* "(~{} | ~{})" x y))
  ([x y & more] `(bit-or (bit-or ~x ~y) ~@more)))

(core/defmacro ^::ana/numeric int [x]
  `(bit-or ~x 0))

(core/defmacro ^::ana/numeric bit-xor
  ([x y] (core/list 'js* "(~{} ^ ~{})" x y))
  ([x y & more] `(bit-xor (bit-xor ~x ~y) ~@more)))

(core/defmacro ^::ana/numeric bit-and-not
  ([x y] (core/list 'js* "(~{} & ~~{})" x y))
  ([x y & more] `(bit-and-not (bit-and-not ~x ~y) ~@more)))

(core/defmacro ^::ana/numeric bit-clear [x n]
  (core/list 'js* "(~{} & ~(1 << ~{}))" x n))

(core/defmacro ^::ana/numeric bit-flip [x n]
  (core/list 'js* "(~{} ^ (1 << ~{}))" x n))

(core/defmacro bit-test [x n]
  (bool-expr (core/list 'js* "((~{} & (1 << ~{})) != 0)" x n)))

(core/defmacro ^::ana/numeric bit-shift-left [x n]
  (core/list 'js* "(~{} << ~{})" x n))

(core/defmacro ^::ana/numeric bit-shift-right [x n]
  (core/list 'js* "(~{} >> ~{})" x n))

(core/defmacro ^::ana/numeric bit-shift-right-zero-fill [x n]
  (core/list 'js* "(~{} >>> ~{})" x n))

(core/defmacro ^::ana/numeric unsigned-bit-shift-right [x n]
  (core/list 'js* "(~{} >>> ~{})" x n))

(core/defmacro ^::ana/numeric bit-set [x n]
  (core/list 'js* "(~{} | (1 << ~{}))" x n))

;; internal
(core/defmacro caching-hash [coll hash-fn hash-key]
  (core/assert (clojure.core/symbol? hash-key) "hash-key is substituted twice")
  `(let [h# ~hash-key]
     (if-not (nil? h#)
       h#
       (let [h# (~hash-fn ~coll)]
         (set! ~hash-key h#)
         h#))))

(core/defn- protocol-prefix [psym]
  (core/str (core/-> (core/str psym)
                     (.replace \. \$)
              (.replace \/ \$))
    "$"))

(def ^:private base-type
     {nil "null"
      'object "object"
      'string "string"
      'number "number"
      'array "array"
      'function "function"
      'boolean "boolean"
      'default "_"})

(def ^:private js-base-type
     {'js/Boolean "boolean"
      'js/String "string"
      'js/Array "array"
      'js/Object "object"
      'js/Number "number"
      'js/Function "function"})

(core/defmacro ^:private js-this []
  (core/list 'js* "this"))

(core/defmacro this-as
  "Defines a scope where JavaScript's implicit \"this\" is bound to the name provided."
  [name & body]
  `(let [~name (js-this)]
     ~@body))

(core/defn- to-property [sym]
  (symbol (core/str "-" sym)))

(core/defn- warn-and-update-protocol [p type env]
  (core/when-not (= 'Object p)
    (core/if-let [var (cljs.analyzer/resolve-existing-var (dissoc env :locals) p)]
      (do
        (core/when-not (:protocol-symbol var)
          (cljs.analyzer/warning :invalid-protocol-symbol env {:protocol p}))
        (core/when (core/and (:protocol-deprecated cljs.analyzer/*cljs-warnings*)
                (core/-> var :deprecated)
                (not (core/-> p meta :deprecation-nowarn)))
          (cljs.analyzer/warning :protocol-deprecated env {:protocol p}))
        (core/when (:protocol-symbol var)
          (swap! env/*compiler* update-in [:cljs.analyzer/namespaces]
            (core/fn [ns]
              (update-in ns [(:ns var) :defs (symbol (name p)) :impls]
                conj type)))))
      (core/when (:undeclared cljs.analyzer/*cljs-warnings*)
        (cljs.analyzer/warning :undeclared-protocol-symbol env {:protocol p})))))

(core/defn- resolve-var [env sym]
  (core/let [ret (:name (cljs.analyzer/resolve-var env sym))]
    (core/assert ret (core/str "Can't resolve: " sym))
    ret))

(core/defn- ->impl-map [impls]
  (core/loop [ret {} s impls]
    (if (seq s)
      (recur (assoc ret (first s) (take-while seq? (next s)))
        (drop-while seq? (next s)))
      ret)))

(core/defn- base-assign-impls [env resolve tsym type [p sigs]]
  (warn-and-update-protocol p tsym env)
  (core/let [psym       (resolve p)
             pfn-prefix (subs (core/str psym) 0
                          (clojure.core/inc (.indexOf (core/str psym) "/")))]
    (cons `(aset ~psym ~type true)
      (map (core/fn [[f & meths :as form]]
             `(aset ~(symbol (core/str pfn-prefix f))
                ~type ~(with-meta `(fn ~@meths) (meta form))))
           sigs))))

(core/defmulti ^:private extend-prefix (core/fn [tsym sym] (core/-> tsym meta :extend)))

(core/defmethod extend-prefix :instance
  [tsym sym] `(.. ~tsym ~(to-property sym)))

(core/defmethod extend-prefix :default
  [tsym sym] `(.. ~tsym ~'-prototype ~(to-property sym)))

(core/defn- adapt-obj-params [type [[this & args :as sig] & body]]
  (core/list (vec args)
             (list* 'this-as (vary-meta this assoc :tag type) body)))

(core/defn- adapt-ifn-params [type [[this & args :as sig] & body]]
  (core/let [self-sym (with-meta 'self__ {:tag type})]
    `(~(vec (cons self-sym args))
       (this-as ~self-sym
         (let [~this ~self-sym]
           ~@body)))))

;; for IFn invoke implementations, we need to drop first arg
(core/defn- adapt-ifn-invoke-params [type [[this & args :as sig] & body]]
  `(~(vec args)
     (this-as ~(vary-meta this assoc :tag type)
       ~@body)))

(core/defn- adapt-proto-params [type [[this & args :as sig] & body]]
  (core/let [this' (vary-meta this assoc :tag type)]
    `(~(vec (cons this' args))
      (this-as ~this'
        ~@body))))

(core/defn- add-obj-methods [type type-sym sigs]
  (map (core/fn [[f & meths :as form]]
         (core/let [[f meths] (if (vector? (first meths))
                                [f [(rest form)]]
                                [f meths])]
           `(set! ~(extend-prefix type-sym f)
                  ~(with-meta `(fn ~@(map #(adapt-obj-params type %) meths)) (meta form)))))
       sigs))

(core/defn- ifn-invoke-methods [type type-sym [f & meths :as form]]
  (map
    (core/fn [meth]
      (core/let [arity (count (first meth))]
        `(set! ~(extend-prefix type-sym (symbol (core/str "cljs$core$IFn$_invoke$arity$" arity)))
           ~(with-meta `(fn ~meth) (meta form)))))
    (map #(adapt-ifn-invoke-params type %) meths)))

(core/defn- add-ifn-methods [type type-sym [f & meths :as form]]
  (core/let [meths    (map #(adapt-ifn-params type %) meths)
             this-sym (with-meta 'self__ {:tag type})
             argsym   (gensym "args")]
    (concat
      [`(set! ~(extend-prefix type-sym 'call) ~(with-meta `(fn ~@meths) (meta form)))
       `(set! ~(extend-prefix type-sym 'apply)
          ~(with-meta
             `(fn ~[this-sym argsym]
                (this-as ~this-sym
                  (.apply (.-call ~this-sym) ~this-sym
                    (.concat (array ~this-sym) (corale.core/aclone ~argsym)))))
             (meta form)))]
      (ifn-invoke-methods type type-sym form))))

(core/defn- add-proto-methods* [pprefix type type-sym [f & meths :as form]]
  (core/let [pf (core/str pprefix (name f))]
    (if (vector? (first meths))
      ;; single method case
      (core/let [meth meths]
        [`(set! ~(extend-prefix type-sym (core/str pf "$arity$" (count (first meth))))
            ~(with-meta `(fn ~@(adapt-proto-params type meth)) (meta form)))])
      (map (core/fn [[sig & body :as meth]]
             `(set! ~(extend-prefix type-sym (core/str pf "$arity$" (count sig)))
                ~(with-meta `(fn ~(adapt-proto-params type meth)) (meta form))))
        meths))))

(core/defn- proto-assign-impls [env resolve type-sym type [p sigs]]
  (warn-and-update-protocol p type env)
  (core/let [psym      (resolve p)
             pprefix   (protocol-prefix psym)
             skip-flag (set (core/-> type-sym meta :skip-protocol-flag))]
    (if (= p 'Object)
      (add-obj-methods type type-sym sigs)
      (concat
        (core/when-not (skip-flag psym)
          [`(set! ~(extend-prefix type-sym pprefix) cljs.core/PROTOCOL_SENTINEL)])
        (mapcat
          (core/fn [sig]
            (if (= psym 'cljs.core/IFn)
              (add-ifn-methods type type-sym sig)
              (add-proto-methods* pprefix type type-sym sig)))
          sigs)))))

(core/defn- validate-impl-sigs [env p method]
  (core/when-not (= p 'Object)
    (core/let [var (ana/resolve-var (dissoc env :locals) p)
               minfo (core/-> var :protocol-info :methods)
               method-name (first method)
               ->name (comp symbol name)
               [fname sigs] (if (core/vector? (second method))
                              [(->name method-name) [(second method)]]
                              [(->name method-name) (map first (rest method))])
               decmeths (core/get minfo fname ::not-found)]
      (core/when (= decmeths ::not-found)
        (ana/warning :protocol-invalid-method env {:protocol p :fname fname :no-such-method true}))
      (core/when (namespace method-name)
        (core/let [method-var (ana/resolve-var (dissoc env :locals) method-name
                                ana/confirm-var-exist-warning)]
          (core/when-not (= (:name var) (:protocol method-var))
            (ana/warning :protocol-invalid-method env
              {:protocol p :fname method-name :no-such-method true}))))
      (core/loop [sigs sigs seen #{}]
        (core/when (seq sigs)
          (core/let [sig (first sigs)
                     c   (count sig)]
            (core/when (contains? seen c)
              (ana/warning :protocol-duped-method env {:protocol p :fname fname}))
            (core/when (core/and (not= decmeths ::not-found) (not (some #{c} (map count decmeths))))
              (ana/warning :protocol-invalid-method env {:protocol p :fname fname :invalid-arity c}))
            (recur (next sigs) (conj seen c))))))))

(core/defn- validate-impls [env impls]
  (core/loop [protos #{} impls impls]
    (core/when (seq impls)
      (core/let [proto   (first impls)
                 methods (take-while seq? (next impls))
                 impls   (drop-while seq? (next impls))]
        (core/when (contains? protos proto)
          (ana/warning :protocol-multiple-impls env {:protocol proto}))
        (core/loop [seen #{} methods methods]
          (core/when (seq methods)
            (core/let [[fname :as method] (first methods)]
              (core/when (contains? seen fname)
                (ana/warning :extend-type-invalid-method-shape env
                  {:protocol proto :method fname}))
              (validate-impl-sigs env proto method)
              (recur (conj seen fname) (next methods)))))
        (recur (conj protos proto) impls)))))

(core/defn- type-hint-first-arg
  [type-sym argv]
  (assoc argv 0 (vary-meta (argv 0) assoc :tag type-sym)))

(core/defn- type-hint-single-arity-sig
  [type-sym sig]
  (list* (first sig) (type-hint-first-arg type-sym (second sig)) (nnext sig)))

(core/defn- type-hint-multi-arity-sig
  [type-sym sig]
  (list* (type-hint-first-arg type-sym (first sig)) (next sig)))

(core/defn- type-hint-multi-arity-sigs
  [type-sym sigs]
  (list* (first sigs) (map (partial type-hint-multi-arity-sig type-sym) (rest sigs))))

(core/defn- type-hint-sigs
  [type-sym sig]
  (if (vector? (second sig))
    (type-hint-single-arity-sig type-sym sig)
    (type-hint-multi-arity-sigs type-sym sig)))

(core/defn- type-hint-impl-map
  [type-sym impl-map]
  (reduce-kv (core/fn [m proto sigs]
               (assoc m proto (map (partial type-hint-sigs type-sym) sigs)))
    {} impl-map))

(core/defmacro extend-type
  "Extend a type to a series of protocols. Useful when you are
  supplying the definitions explicitly inline. Propagates the
  type as a type hint on the first argument of all fns.

  type-sym may be

   * default, meaning the definitions will apply for any value,
     unless an extend-type exists for one of the more specific
     cases below.
   * nil, meaning the definitions will apply for the nil value.
   * any of object, boolean, number, string, array, or function,
     indicating the definitions will apply for values of the
     associated base JavaScript types. Note that, for example,
     string should be used instead of js/String.
   * a JavaScript type not covered by the previous list, such
     as js/RegExp.
   * a type defined by deftype or defrecord.

  (extend-type MyType
    ICounted
    (-count [c] ...)
    Foo
    (bar [x y] ...)
    (baz ([x] ...) ([x y & zs] ...))"
  [type-sym & impls]
  (core/let [env &env
             _ (validate-impls env impls)
             resolve (partial resolve-var env)
             impl-map (->impl-map impls)
             impl-map (if ('#{boolean number} type-sym)
                        (type-hint-impl-map type-sym impl-map)
                        impl-map)
             [type assign-impls] (core/if-let [type (base-type type-sym)]
                                   [type base-assign-impls]
                                   [(resolve type-sym) proto-assign-impls])]
    (core/when (core/and (:extending-base-js-type cljs.analyzer/*cljs-warnings*)
            (js-base-type type-sym))
      (cljs.analyzer/warning :extending-base-js-type env
        {:current-symbol type-sym :suggested-symbol (js-base-type type-sym)}))
    `(do ~@(mapcat #(assign-impls env resolve type-sym type %) impl-map))))

(core/defn- prepare-protocol-masks [env impls]
  (core/let [resolve  (partial resolve-var env)
             impl-map (->impl-map impls)
             fpp-pbs  (seq
                       (keep fast-path-protocols
                             (map resolve
                                  (keys impl-map))))]
    (if fpp-pbs
      (core/let [fpps  (into #{}
                             (filter (partial contains? fast-path-protocols)
                                     (map resolve (keys impl-map))))
                 parts (core/as-> (group-by first fpp-pbs) parts
                         (into {}
                               (map (juxt key (comp (partial map peek) val))
                                    parts))
                         (into {}
                               (map (juxt key (comp (partial reduce core/bit-or) val))
                                    parts)))]
        [fpps (reduce (core/fn [ps p] (update-in ps [p] (core/fnil identity 0)))
                      parts
                      (range fast-path-protocol-partitions-count))]))))

(core/defn- annotate-specs [annots v [f sigs]]
  (conj v
    (vary-meta (cons f (map #(cons (second %) (nnext %)) sigs))
      merge annots)))

(core/defn dt->et
  ([type specs fields]
   (dt->et type specs fields false))
  ([type specs fields inline]
   (core/let [annots {:cljs.analyzer/type type
                      :cljs.analyzer/protocol-impl true
                      :cljs.analyzer/protocol-inline inline}]
     (core/loop [ret [] specs specs]
       (if (seq specs)
         (core/let [p     (first specs)
                    ret   (core/-> (conj ret p)
                            (into (reduce (partial annotate-specs annots) []
                                    (group-by first (take-while seq? (next specs))))))
                    specs (drop-while seq? (next specs))]
           (recur ret specs))
         ret)))))

(core/defn- collect-protocols [impls env]
  (core/->> impls
      (filter core/symbol?)
      (map #(:name (cljs.analyzer/resolve-var (dissoc env :locals) %)))
      (into #{})))

(core/defn- build-positional-factory
  [rsym rname fields]
  (core/let [fn-name (with-meta (symbol (core/str '-> rsym))
                       (assoc (meta rsym) :factory :positional))
        field-values (if (core/-> rsym meta :internal-ctor) (conj fields nil nil nil) fields)]
    `(defn ~fn-name
       [~@fields]
       (new ~rname ~@field-values))))

(core/defn- validate-fields
  [case name fields]
  (core/when-not (vector? fields)
    (throw
     (AssertionError. (core/str case " " name ", no fields vector given.")))))

(core/defmacro deftype
  "(deftype name [fields*]  options* specs*)

  Currently there are no options.

  Each spec consists of a protocol or interface name followed by zero
  or more method bodies:

  protocol-or-Object
  (methodName [args*] body)*

  The type will have the (by default, immutable) fields named by
  fields, which can have type hints. Protocols and methods
  are optional. The only methods that can be supplied are those
  declared in the protocols/interfaces.  Note that method bodies are
  not closures, the local environment includes only the named fields,
  and those fields can be accessed directly. Fields can be qualified
  with the metadata :mutable true at which point (set! afield aval) will be
  supported in method bodies. Note well that mutable fields are extremely
  difficult to use correctly, and are present only to facilitate the building
  of higherlevel constructs, such as ClojureScript's reference types, in
  ClojureScript itself. They are for experts only - if the semantics and
  implications of :mutable are not immediately apparent to you, you should not
  be using them.

  Method definitions take the form:

  (methodname [args*] body)

  The argument and return types can be hinted on the arg and
  methodname symbols. If not supplied, they will be inferred, so type
  hints should be reserved for disambiguation.

  Methods should be supplied for all methods of the desired
  protocol(s). You can also define overrides for methods of Object. Note that
  a parameter must be supplied to correspond to the target object
  ('this' in JavaScript parlance). Note also that recur calls to the method
  head should *not* pass the target object, it will be supplied
  automatically and can not be substituted.

  In the method bodies, the (unqualified) name can be used to name the
  class (for calls to new, instance? etc).

  One constructor will be defined, taking the designated fields.  Note
  that the field names __meta and __extmap are currently reserved and
  should not be used when defining your own types.

  Given (deftype TypeName ...), a factory function called ->TypeName
  will be defined, taking positional parameters for the fields"
  [t fields & impls]
  (validate-fields "deftype" t fields)
  (core/let [env &env
             r (:name (cljs.analyzer/resolve-var (dissoc env :locals) t))
             [fpps pmasks] (prepare-protocol-masks env impls)
             protocols (collect-protocols impls env)
             t (vary-meta t assoc
                          :protocols protocols
                          :skip-protocol-flag fpps) ]
    `(do
       (deftype* ~t ~fields ~pmasks
         ~(if (seq impls)
            `(extend-type ~t ~@(dt->et t impls fields))))
       (set! (.-getBasis ~t) (fn [] (corale.core/array ~@(map (core/fn [x] (core/list 'quote x)) fields))))
       (set! (.-cljs$lang$type ~t) true)
       (set! (.-cljs$lang$ctorStr ~t) ~(core/str r))
       (set! (.-cljs$lang$ctorPrWriter ~t) (fn [this# writer# opt#] (cljs.core/-write writer# ~(core/str r))))

       ~(build-positional-factory t r fields)
       ~t)))

(core/defmacro defprotocol
  "A protocol is a named set of named methods and their signatures:

  (defprotocol AProtocolName
    ;optional doc string
    \"A doc string for AProtocol abstraction\"

  ;method signatures
    (bar [this a b] \"bar docs\")
    (baz [this a] [this a b] [this a b c] \"baz docs\"))

  No implementations are provided. Docs can be specified for the
  protocol overall and for each method. The above yields a set of
  polymorphic functions and a protocol object. All are
  namespace-qualified by the ns enclosing the definition The resulting
  functions dispatch on the type of their first argument, which is
  required and corresponds to the implicit target object ('this' in
  JavaScript parlance). defprotocol is dynamic, has no special compile-time
  effect, and defines no new types.

  (defprotocol P
    (foo [this])
    (bar-me [this] [this y]))

  (deftype Foo [a b c]
    P
    (foo [this] a)
    (bar-me [this] b)
    (bar-me [this y] (+ c y)))

  (bar-me (Foo. 1 2 3) 42)
  => 45

  (foo
    (let [x 42]
      (reify P
        (foo [this] 17)
        (bar-me [this] x)
        (bar-me [this y] x))))
  => 17"
  [psym & doc+methods]
  (core/let [p (:name (cljs.analyzer/resolve-var (dissoc &env :locals) psym))
             [doc methods] (if (core/string? (first doc+methods))
                             [(first doc+methods) (next doc+methods)]
                             [nil doc+methods])
             psym (vary-meta psym assoc
                             :doc doc
                             :protocol-symbol true)
             ns-name (core/-> &env :ns :name)
             fqn (core/fn [n] (symbol (core/str ns-name "." n)))
             prefix (protocol-prefix p)
             _ (core/doseq [[mname & arities] methods]
                 (core/when (some #{0} (map count (filter vector? arities)))
                   (throw
                    (Exception.
                             (core/str "Invalid protocol, " psym
                                       " defines method " mname " with arity 0")))))
             expand-sig (core/fn [fname slot sig]
                          (core/let [sig (core/if-not (every? core/symbol? sig)
                                           (mapv (core/fn [arg]
                                                   (core/cond
                                                     (core/symbol? arg) arg
                                                     (core/and (map? arg) (core/some? (:as arg))) (:as arg)
                                                     :else (gensym))) sig)
                                           sig)]
                            `(~sig
                              (if (and (corale.core/not (nil? ~(first sig)))
                                       (corale.core/not (nil? (. ~(first sig) ~(symbol (core/str "-" slot)))))) ;; Property access needed here.
                                (. ~(first sig) ~slot ~@sig)
                                (let [x# (if (nil? ~(first sig)) nil ~(first sig))
                                      m# (aget ~(fqn fname) (goog/typeOf x#))]
                                  (core/if-not (nil? m#)
                                    (m# ~@sig)
                                    (let [m# (aget ~(fqn fname) "_")]
                                      (core/if-not (nil? m#)
                                        (m# ~@sig)
                                        (throw
                                         (missing-protocol
                                          ~(core/str psym "." fname) ~(first sig)))))))))))
             psym (core/-> psym
                           (vary-meta update-in [:jsdoc] conj
                                      "@interface")
                           (vary-meta assoc-in [:protocol-info :methods]
                                      (into {}
                                            (map
                                             (core/fn [[fname & sigs]]
                                               (core/let [doc (core/as-> (last sigs) doc
                                                                (core/when (core/string? doc) doc))
                                                          sigs (take-while vector? sigs)]
                                                 [(vary-meta fname assoc :doc doc)
                                                  (vec sigs)]))
                                             methods))))
             method (core/fn [[fname & sigs]]
                      (core/let [doc (core/as-> (last sigs) doc
                                       (core/when (core/string? doc) doc))
                                 sigs (take-while vector? sigs)
                                 amp (core/when (some #{'&} (apply concat sigs))
                                       (cljs.analyzer/warning
                                        :protocol-with-variadic-method
                                        &env {:protocol psym :name fname}))
                                 slot (symbol (core/str prefix (name fname)))
                                 fname (vary-meta fname assoc
                                                  :protocol p
                                                  :doc doc)]
                        `(defn ~fname
                           ~@(map (core/fn [sig]
                                    (expand-sig fname
                                                (symbol (core/str slot "$arity$" (count sig)))
                                                sig))
                                  sigs))))]
    `(do
       (set! ~'*unchecked-if* true)
       (def ~psym (~'js* "function(){}"))
       ~@(map method methods)
       (set! ~'*unchecked-if* false))))

(core/defmacro dotimes
  "bindings => name n

  Repeatedly executes body (presumably for side-effects) with name
  bound to integers from 0 through n-1."
  [bindings & body]
  (core/let [i (first bindings)
             n (second bindings)]
    `(let [n# ~n]
       (loop [~i 0]
         (when (< ~i n#)
           ~@body
           (recur (inc ~i)))))))

(core/defmacro array [& rest]
  (core/let [xs-str (core/->> (repeat "~{}")
                              (take (count rest))
                              (interpose ",")
                              (apply core/str))]
    (vary-meta
     (list* 'js* (core/str "[" xs-str "]") rest)
     assoc :tag 'array)))

(core/defmacro make-array
  ([size]
   (vary-meta
    (if (core/number? size)
      `(array ~@(take size (repeat nil)))
      `(js/Array. ~size))
    assoc :tag 'array))
  ([type size]
   `(corale.core/make-array ~size))
  ([type size & more-sizes]
   (vary-meta
    `(let [dims#     (array ~@more-sizes)
           dimarray# (make-array ~size)]
       (dotimes [i# (alength dimarray#)]
         (aset dimarray# i# (corale.core/apply make-array nil dims#)))
       dimarray#)
    assoc :tag 'array)))

(core/defmacro alength [a]
  (vary-meta
    (core/list 'js* "~{}.length" a)
    assoc :tag 'number))

;;;;;;;;;;;

(core/defn- bool-expr [e]
  (vary-meta e assoc :tag 'boolean))

(core/defn- js-obj* [kvs]
  (core/let [kvs-str (core/->> (repeat "~{}:~{}")
                               (take (count kvs))
                               (interpose ",")
                               (apply core/str))]
    (vary-meta
     (list* 'js* (core/str "{" kvs-str "}") (apply concat kvs))
     assoc :tag 'object)))

(core/defmacro js-obj [& rest]
  (core/let [sym-or-str? (core/fn [x] (core/or (core/symbol? x) (core/string? x)))
             filter-on-keys (core/fn [f coll]
                              (core/->> coll
                                        (filter (core/fn [[k _]] (f k)))
                                        (into {})))
             kvs (into {} (map vec (partition 2 rest)))
             sym-pairs (filter-on-keys core/symbol? kvs)
             expr->local (zipmap
                          (filter (complement sym-or-str?) (keys kvs))
                          (repeatedly gensym))
             obj (gensym "obj")]
    (if (empty? rest)
      (js-obj* '())
      `(let [~@(apply concat (clojure.set/map-invert expr->local))
             ~obj ~(js-obj* (filter-on-keys core/string? kvs))]
         ~@(map (core/fn [[k v]] `(aset ~obj ~k ~v)) sym-pairs)
         ~@(map (core/fn [[k v]] `(aset ~obj ~v ~(core/get kvs k))) expr->local)
         ~obj))))

#_(require '[clojure.pprint :refer [pprint]])
#_(pprint (destructure '[{:keys [e] :or {e 3} :as b} e]))
#_(pprint (destructure '[[a b & r] e]))

(core/defn destructure [bindings]
  (core/let [bents (partition 2 bindings)
             pb (core/fn pb [bvec b v]
                  (core/let [pvec
                             (core/fn [bvec b val]
                               (core/let [gvec (gensym "vec__")
                                          gseq (gensym "seq__")
                                          gfirst (gensym "first__")
                                          has-rest (some #{'&} b)]
                                 (core/loop [ret (core/let [ret (conj bvec gvec val)]
                                                   (if has-rest
                                                     (conj ret gseq (core/list `arr gvec))
                                                     ret))
                                             n 0
                                             bs b
                                             seen-rest? false]
                                   (if (seq bs)
                                     (core/let [firstb (first bs)]
                                       (core/cond
                                         (= firstb '&) (recur (pb ret (second bs)
                                                                  (core/list `corale.core/drop
                                                                             n gseq))
                                                              n
                                                              (nnext bs)
                                                              true)
                                         (= firstb :as) (pb ret (second bs) gvec)
                                         :else (if seen-rest?
                                                 (throw (new Exception "Unsupported binding form, only :as can follow & parameter"))
                                                 (recur (pb ret
                                                            firstb
                                                            (if has-rest
                                                              gfirst
                                                              (core/list `corale.core/get
                                                                         gvec n nil)))
                                                        (core/inc n)
                                                        (next bs)
                                                        seen-rest?))))
                                     ret))))
                             pmap
                             (core/fn [bvec b v]
                               (core/let [gmap (gensym "map__")
                                          getfn (gensym "getfn__")
                                          defaults (:or b)]
                                 (core/loop [ret (core/-> bvec (conj gmap) (conj v)
                                                          (conj getfn) (conj `(if (instance? js/Array ~gmap) get find-in-arr))
                                                          (conj gmap) (conj gmap)
                                                          ((core/fn [ret]
                                                             (if (:as b)
                                                               (conj ret (:as b) gmap)
                                                               ret))))
                                             bes (core/let [transforms
                                                            (reduce
                                                             (core/fn [transforms mk]
                                                               (if (core/keyword? mk)
                                                                 (core/let [mkns (namespace mk)
                                                                            mkn (name mk)]
                                                                   (core/cond (= mkn "keys") (assoc transforms mk #(keyword (core/or mkns (namespace %)) (name %)))
                                                                              (= mkn "syms") (assoc transforms mk #(core/list `quote (symbol (core/or mkns (namespace %)) (name %))))
                                                                              (= mkn "strs") (assoc transforms mk core/str)
                                                                              :else transforms))
                                                                 transforms))
                                                             {}
                                                             (keys b))]
                                                   (reduce
                                                    (core/fn [bes entry]
                                                      (reduce #(assoc %1 %2 ((val entry) %2))
                                                              (dissoc bes (key entry))
                                                              ((key entry) bes)))
                                                    (dissoc b :as :or)
                                                    transforms))]
                                   (if (seq bes)
                                     (core/let [bb (key (first bes))
                                                bk (val (first bes))
                                                local (if (core/instance? clojure.lang.Named bb)
                                                        (with-meta (symbol nil (name bb)) (meta bb))
                                                        bb)
                                                bv (if (contains? defaults local)
                                                     (core/list 'corale.core/get gmap bk (defaults local))
                                                     (core/list 'corale.core/get gmap bk))]
                                       (recur
                                        (if (core/or (core/keyword? bb) (core/symbol? bb)) ;(ident? bb)
                                          (core/-> ret (conj local bv))
                                          (pb ret bb bv))
                                        (next bes)))
                                     ret))))]
                    (core/cond
                      (core/symbol? b) (core/-> bvec (conj (if (namespace b) (symbol (name b)) b)) (conj v))
                      (core/keyword? b) (core/-> bvec (conj (symbol (name b))) (conj v))
                      (vector? b) (pvec bvec b v)
                      (map? b) (pmap bvec b v)
                      :else (throw
                             (new Exception (core/str "Unsupported binding form: " b))))))
             process-entry (core/fn [bvec b] (pb bvec (first b) (second b)))]
    (if (every? core/symbol? (map first bents))
      bindings
      (core/if-let [kwbs (seq (filter #(core/keyword? (first %)) bents))]
        (throw
         (new Exception (core/str "Unsupported binding key: " (ffirst kwbs))))
        (reduce process-entry [] bents)))))

(core/defmacro let
  "binding => binding-form init-expr

  Evaluates the exprs in a lexical context in which the symbols in
  the binding-forms are bound to their respective init-exprs or parts
  therein."
  [bindings & body]
  (cljs.support/assert-args
   let
   (vector? bindings) "a vector for its binding"
   (even? (count bindings)) "an even number of forms in binding vector")
  `(let* ~(destructure bindings) ~@body))

(core/defn ^{:private true}
  maybe-destructured
  [params body]
  (if (core/every? core/symbol? params)
    (core/cons params body)
    (core/loop [params params
                new-params (core/with-meta [] (core/meta params))
                lets []]
      (if params
        (if (core/symbol? (core/first params))
          (recur (core/next params) (core/conj new-params (core/first params)) lets)
          (let [gparam (core/gensym "p__")]
            (recur (core/next params) (core/conj new-params gparam)
                   (-> lets (core/conj (core/first params)) (core/conj gparam)))))
        `(~new-params
          (let ~lets
            ~@body))))))

(core/defmacro fn
  "params => positional-params* , or positional-params* & next-param
  positional-param => binding-form
  next-param => binding-form
  name => symbol

  Defines a function"
  [& sigs]
  (core/let [name (if (core/symbol? (core/first sigs)) (core/first sigs) nil)
             sigs (if name (core/next sigs) sigs)
             sigs (if (core/vector? (core/first sigs)) 
                    (core/list sigs) 
                    (if (core/seq? (core/first sigs))
                      sigs
                      ;; Assume single arity syntax
                      (throw (IllegalArgumentException. 
                              (if (core/seq sigs)
                                (core/str "Parameter declaration " 
                                          (core/first sigs)
                                          " should be a vector")
                                (core/str "Parameter declaration missing"))))))
             psig (fn* [sig]
                       ;; Ensure correct type before destructuring sig
                       (core/when (core/not (core/seq? sig))
                         (throw (IllegalArgumentException.
                                 (core/str "Invalid signature " sig
                                           " should be a list"))))
                       (core/let [[params & body] sig
                                  _ (core/when (core/not (core/vector? params))
                                      (throw (IllegalArgumentException. 
                                              (if (core/seq? (core/first sigs))
                                                (core/str "Parameter declaration " params
                                                          " should be a vector")
                                                (core/str "Invalid signature " sig
                                                          " should be a list")))))
                                  conds (core/when (core/and (core/next body)
                                                             (core/map? (core/first body))) 
                                          (core/first body))
                                  body (if conds (core/next body) body)
                                  conds (core/or conds (core/meta params))
                                  pre (:pre conds)
                                  post (:post conds)                       
                                  body (if post
                                         `((let [~'% ~(if (core/< 1 (count body)) 
                                                        `(do ~@body) 
                                                        (first body))]
                                             ~@(map (fn* [c] `(assert ~c)) post)
                                             ~'%))
                                         body)
                                  body (if pre
                                         (concat (map (fn* [c] `(assert ~c)) pre) 
                                                 body)
                                         body)]
                         (maybe-destructured params body)))
             new-sigs (core/map psig sigs)]
    (core/with-meta
      (if name
        (list* 'fn* name new-sigs)
        (core/cons 'fn* new-sigs))
      (core/meta &form))))

(core/defn- variadic-fn*
  ([sym method]
   (variadic-fn* sym method true))
  ([sym [arglist & body :as method] solo]
   (core/let [sig (remove '#{&} arglist)
              restarg (gensym "seq")]
     (core/letfn [(get-delegate []
                    'cljs$core$IFn$_invoke$arity$variadic)
                  (get-delegate-prop []
                    (symbol (core/str "-" (get-delegate))))
                  (param-bind [param i]
                    `[~param (^::ana/no-resolve corale.core/get ~restarg ~i)])
                  (apply-to []
                    (if (core/< 1 (count sig))
                      (core/let [sig-count (core/dec (count sig))
                                 params (repeatedly sig-count gensym)]
                        `(fn
                           ([~restarg]
                            (let [~@(mapcat param-bind params (range))]
                              (. ~sym (~(get-delegate) ~@params (.slice ~restarg ~sig-count)))))))
                      `(fn
                         ([~restarg]
                          (. ~sym (~(get-delegate) ~restarg))))))]
       `(do
          (set! (. ~sym ~(get-delegate-prop))
                (fn (~(vec sig) ~@body)))
          ~@(core/when solo
              `[(set! (. ~sym ~'-cljs$lang$maxFixedArity)
                      ~(core/dec (count sig)))])
          (set! (. ~sym ~'-cljs$lang$applyTo)
                ~(apply-to)))))))

(core/defmacro js-arguments []
  (core/list 'js* "arguments"))

(core/defmacro js-delete [obj key]
  (core/list 'js* "delete ~{}[~{}]" obj key))

(core/defmacro true? [x]
  (bool-expr (core/list 'js* "~{} === true" x)))

(core/defmacro false? [x]
  (bool-expr (core/list 'js* "~{} === false" x)))

(core/defmacro copy-arguments [dest]
  `(let [len# (alength (js-arguments))]
     (loop [i# 0]
       (when (< i# len#)
         (.push ~dest (aget (js-arguments) i#))
         (recur (inc i#))))))

(core/defn- variadic-fn [name meta [[arglist & body :as method] :as fdecl] emit-var?]
  (core/letfn [(dest-args [c]
                 (map (core/fn [n] `(aget (js-arguments) ~n))
                      (range c)))]
    (core/let [rname (symbol (core/str ana/*cljs-ns*) (core/str name))
               sig   (remove '#{&} arglist)
               c-1   (core/dec (count sig))
               meta  (assoc meta
                            :top-fn
                            {:variadic true
                             :max-fixed-arity c-1
                             :method-params [sig]
                             :arglists (core/list arglist)
                             :arglists-meta (doall (map meta [arglist]))})]
      `(do
         (def ~(with-meta name meta)
           (fn [~'var_args]
             (let [args# (array)]
               (copy-arguments args#)
               (let [argseq# (when (< ~c-1 (alength args#))
                               (.slice args# ~c-1))]
                 (. ~rname
                    (~'cljs$core$IFn$_invoke$arity$variadic ~@(dest-args c-1) argseq#))))))
         ~(variadic-fn* rname method)
         ~(core/when emit-var? `(var ~name))))))

(comment
  (require '[clojure.pprint :as pprint])
  (pprint (variadic-fn 'nn {} '[([] nil)] nil))
  )

(core/defn- multi-arity-fn [name meta fdecl emit-var?]
  (core/letfn [(dest-args [c]
                 (map (core/fn [n] `(aget (js-arguments) ~n))
                      (range c)))
               (fixed-arity [rname sig]
                 (core/let [c (count sig)]
                   [c `(. ~rname
                          (~(symbol
                             (core/str "cljs$core$IFn$_invoke$arity$" c))
                           ~@(dest-args c)))]))
               (fn-method [[sig & body :as method]]
                 (if (some '#{&} sig)
                   (variadic-fn* name method false)
                   `(set!
                     (. ~name
                        ~(symbol (core/str "-cljs$core$IFn$_invoke$arity$"
                                           (count sig))))
                     (fn ~method))))]
    (core/let [rname    (symbol (core/str ana/*cljs-ns*) (core/str name))
               arglists (map first fdecl)
               varsig?  #(some '#{&} %)
               variadic (boolean (some varsig? arglists))
               sigs     (remove varsig? arglists)
               maxfa    (apply core/max
                               (concat
                                (map count sigs)
                                [(core/- (count (first (filter varsig? arglists))) 2)]))
               meta     (assoc meta
                               :top-fn
                               {:variadic variadic
                                :max-fixed-arity maxfa
                                :method-params sigs
                                :arglists arglists
                                :arglists-meta (doall (map meta arglists))})
               args-sym (gensym "args")]
      `(do
         (def ~(with-meta name meta)
           (fn [~'var_args]
             (let [~args-sym (array)]
               (copy-arguments ~args-sym)
               (case (alength ~args-sym)
                 ~@(mapcat #(fixed-arity rname %) sigs)
                 ~(if variadic
                    `(let [argseq# (.slice ~args-sym ~maxfa)]
                       (. ~rname
                          (~'cljs$core$IFn$_invoke$arity$variadic
                           ~@(dest-args maxfa)
                           argseq#)))
                    (if (:macro meta)
                      `(throw (js/Error.
                               (str "Invalid arity: " (- (alength ~args-sym) 2))))
                      `(throw (js/Error.
                               (str "Invalid arity: " (alength ~args-sym))))))))))
         ~@(map fn-method fdecl)
         ;; optimization properties
         (set! (. ~name ~'-cljs$lang$maxFixedArity) ~maxfa)
         ~(core/when emit-var? `(var ~name))))))

(core/defn- ^{:dynamic true} assert-valid-fdecl
  "A good fdecl looks like (([a] ...) ([a b] ...)) near the end of defn."
  [fdecl]
  (core/when (empty? fdecl)
    (throw
     (IllegalArgumentException. "Parameter declaration missing")))
  (core/let [argdecls
             (map
              #(if (seq? %)
                 (first %)
                 (throw
                  (IllegalArgumentException.
                   (if (seq? (first fdecl))
                     (core/str "Invalid signature \""
                               %
                               "\" should be a list")
                     (core/str "Parameter declaration \""
                               %
                               "\" should be a vector")))))
              fdecl)
             bad-args (seq (remove #(vector? %) argdecls))]
    (core/when bad-args
      (throw
       (IllegalArgumentException.
        (core/str "Parameter declaration \"" (first bad-args)
                  "\" should be a vector"))))))

(def
  ^{:private true}
  sigs
  (core/fn [fdecl]
    (assert-valid-fdecl fdecl)
    (core/let [asig
               (core/fn [fdecl]
                 (core/let [arglist (first fdecl)
                                        ;elide implicit macro args
                            arglist (if (clojure.lang.Util/equals '&form (first arglist))
                                      (clojure.lang.RT/subvec arglist 2 (clojure.lang.RT/count arglist))
                                      arglist)
                            body (next fdecl)]
                   (if (map? (first body))
                     (if (next body)
                       (with-meta arglist (conj (if (meta arglist) (meta arglist) {}) (first body)))
                       arglist)
                     arglist)))]
      (if (seq? (first fdecl))
        (core/loop [ret [] fdecls fdecl]
          (if fdecls
            (recur (conj ret (asig (first fdecls))) (next fdecls))
            (seq ret)))
        (core/list (asig fdecl))))))

(core/defn- multi-arity-fn? [fdecl]
  (core/< 1 (count fdecl)))

(core/defn- variadic-fn? [fdecl]
  (core/and (= 1 (count fdecl))
            (some '#{&} (ffirst fdecl))))

(def
  ^{:doc "Same as (def name (fn [params* ] exprs*)) or (def
    name (fn ([params* ] exprs*)+)) with any doc-string or attrs added
    to the var metadata. prepost-map defines a map with optional keys
    :pre and :post that contain collections of pre or post conditions."
    :arglists '([name doc-string? attr-map? [params*] prepost-map? body]
                [name doc-string? attr-map? ([params*] prepost-map? body)+ attr-map?])}
  defn (core/fn defn [&form &env name & fdecl]
         ;; Note: Cannot delegate this check to def because of the call to (with-meta name ..)
         (if (core/instance? clojure.lang.Symbol name)
           nil
           (throw (IllegalArgumentException. "First argument to defn must be a symbol")))
         (core/let [m (if (core/string? (first fdecl))
                        {:doc (first fdecl)}
                        {})
                    fdecl (if (core/string? (first fdecl))
                            (next fdecl)
                            fdecl)
                    m (if (map? (first fdecl))
                        (conj m (first fdecl))
                        m)
                    fdecl (if (map? (first fdecl))
                            (next fdecl)
                            fdecl)
                    fdecl (if (vector? (first fdecl))
                            (core/list fdecl)
                            fdecl)
                    m (if (map? (last fdecl))
                        (conj m (last fdecl))
                        m)
                    fdecl (if (map? (last fdecl))
                            (butlast fdecl)
                            fdecl)
                    m (conj {:arglists (core/list 'quote (sigs fdecl))} m)
                    ;; no support for :inline
                                        ;m (core/let [inline (:inline m)
                                        ;             ifn (first inline)
                                        ;             iname (second inline)]
                                        ;    ;; same as: (if (and (= 'fn ifn) (not (symbol? iname))) ...)
                                        ;    (if (if #?(:clj (clojure.lang.Util/equiv 'fn ifn)
                                        ;               :cljs (= 'fn ifn))
                                        ;          (if #?(:clj (core/instance? clojure.lang.Symbol iname)
                                        ;                 :cljs (core/instance? Symbol iname)) false true))
                                        ;      ;; inserts the same fn name to the inline fn if it does not have one
                                        ;      (assoc m
                                        ;        :inline (cons ifn
                                        ;                  (cons (clojure.lang.Symbol/intern
                                        ;                          (.concat (.getName ^clojure.lang.Symbol name) "__inliner"))
                                        ;                    (next inline))))
                                        ;      m))
                    m (conj (if (meta name) (meta name) {}) m)]
           (core/cond
             (multi-arity-fn? fdecl)
             (multi-arity-fn name
                             (if (comp/checking-types?)
                               (update-in m [:jsdoc] conj "@param {...*} var_args")
                               m) fdecl (:def-emits-var &env))

             (variadic-fn? fdecl)
             (variadic-fn name
                          (if (comp/checking-types?)
                            (update-in m [:jsdoc] conj "@param {...*} var_args")
                            m) fdecl (:def-emits-var &env))

             :else
             (core/list 'def (with-meta name m)
                        ;;todo - restore propagation of fn name
                        ;;must figure out how to convey primitive hints to self calls first
                        (cons `fn fdecl))))))

(. (var defn) (setMacro))

(core/defmacro defn-
  "same as defn, yielding non-public def"
  [name & decls]
  (list* `defn (with-meta name (assoc (meta name) :private true)) decls))

(core/defmacro loop
  "Evaluates the exprs in a lexical context in which the symbols in
  the binding-forms are bound to their respective init-exprs or parts
  therein. Acts as a recur target."
  [bindings & body]
  (cljs.support/assert-args loop
                            (vector? bindings) "a vector for its binding"
                            (even? (count bindings)) "an even number of forms in binding vector")
  (core/let [db (destructure bindings)]
    (if (= db bindings)
      `(loop* ~bindings ~@body)
      (core/let [vs (take-nth 2 (drop 1 bindings))
                 bs (take-nth 2 bindings)
                 gs (map (core/fn [b] (if (core/symbol? b) b (gensym))) bs)
                 bfs (reduce (core/fn [ret [b v g]]
                               (if (core/symbol? b)
                                 (conj ret g v)
                                 (conj ret g v b g)))
                             [] (map core/vector bs vs gs))]
        `(let ~bfs
           (loop* ~(vec (interleave gs gs))
                  (let ~(vec (interleave bs gs))
                    ~@body)))))))

(core/defmacro doseq
  "Repeatedly executes body (presumably for side-effects) with
  bindings and filtering as provided by \"for\".  Does not retain
  the head of the sequence. Returns nil."
  [seq-exprs & body]
  (cljs.support/assert-args doseq
    (vector? seq-exprs) "a vector for its binding"
    (even? (count seq-exprs)) "an even number of forms in binding vector")
  (core/let [err (core/fn [& msg] (throw (ex-info (apply core/str msg) {})))
             step (core/fn step [recform exprs]
                    (core/if-not exprs
                      [true `(do ~@body)]
                      (core/let [k (first exprs)
                                 v (second exprs)

                                 seqsym (gensym "seq__")
                                 indexsym (gensym "index__")
                                 lengthsym (gensym "length__")
                                 recform (if (core/keyword? k)
                                           recform
                                           `(recur ~seqsym ~lengthsym nil 0 0 (inc ~indexsym)))
                                 steppair (step recform (nnext exprs))
                                 needrec (steppair 0)
                                 subform (steppair 1)]
                        (core/cond
                          (= k :let) [needrec `(let ~v ~subform)]
                          (= k :while) [false `(when ~v
                                                 ~subform
                                                 ~@(core/when needrec [recform]))]
                          (= k :when) [false `(if ~v
                                                (do
                                                  ~subform
                                                  ~@(core/when needrec [recform]))
                                                ~recform)]
                          (core/keyword? k) (err "Invalid 'doseq' keyword" k)
                          :else (core/let [chunksym (with-meta (gensym "chunk__")
                                                      {:tag 'not-native})
                                           countsym (gensym "count__")
                                           isym     (gensym "i__")
                                           recform-chunk  `(recur ~seqsym ~lengthsym
                                                                  ~chunksym ~countsym
                                                                  (unchecked-inc ~isym)
                                                                  ~indexsym)
                                           steppair-chunk (step recform-chunk (nnext exprs))
                                           subform-chunk  (steppair-chunk 1)]
                                  [true `(loop [~seqsym   (arr ~v)
                                                ~lengthsym (corale.core/count ~seqsym)
                                                ~chunksym nil
                                                ~countsym 0
                                                ~isym     0
                                                ~indexsym 0]
                                           (if (coercive-boolean (< ~isym ~countsym))
                                             (let [~k (cljs.core/-nth ~chunksym ~isym)]
                                               ~subform-chunk
                                               ~@(core/when needrec [recform-chunk]))
                                             (when (< ~indexsym ~lengthsym)
                                               (let [~k (aget ~seqsym ~indexsym)]
                                                 ~subform
                                                 ~@(core/when needrec [recform])))))])))))]
    (nth (step nil (seq seq-exprs)) 1)))

(def ^:private cs (into [] (map (comp gensym core/str core/char) (range 97 118))))

(core/defn- gen-apply-to-helper
  ([] (gen-apply-to-helper 1))
  ([n]
   (core/let [prop (symbol (core/str "-cljs$core$IFn$_invoke$arity$" n))
              f (symbol (core/str "cljs$core$IFn$_invoke$arity$" n))]
     (if (core/<= n 20)
       `(let [~(cs (core/dec n)) (aget ~'args ~(core/dec n))]
          (if (== ~'argc ~n)
            (if (. ~'f ~prop)
              (. ~'f (~f ~@(take n cs)))
              (~'f ~@(take n cs)))
            ~(gen-apply-to-helper (core/inc n))))
       `(throw (js/Error. "Only up to 20 arguments supported on functions"))))))

(core/defmacro gen-apply-to []
  `(do
     (set! cljs.core/*unchecked-if* true)
     (defn ~'apply-to [~'f ~'argc ~'args]
       (let [~'args (arr ~'args)]
         (if (zero? ~'argc)
           (~'f)
           ~(gen-apply-to-helper))))
     (set! cljs.core/*unchecked-if* false)))

(comment
  (pprint (macroexpand '(doseq [a [1 2]
                                :while true
                                :when (true? a)]
                          a)))
  )
