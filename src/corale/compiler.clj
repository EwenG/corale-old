(in-ns 'cljs.compiler)

(defn corale-emits-keyword [kw]
  (let [ns   (namespace kw)
        name (name kw)]
    (emits "new corale.core.Keyword(")
    (emit-constant ns)
    (emits ",")
    (emit-constant name)
    (emits ",")
    (emit-constant (if ns
                     (str ns "/" name)
                     name))
    (emits ",")
    (emit-constant (hash kw))
    (emits ")")))

(defn corale-emits-symbol [sym]
  (let [ns     (namespace sym)
        name   (name sym)
        symstr (if-not (nil? ns)
                 (str ns "/" name)
                 name)]
    (emits "new corale.core.Symbol(")
    (emit-constant ns)
    (emits ",")
    (emit-constant name)
    (emits ",")
    (emit-constant symstr)
    (emits ",")
    (emit-constant (hash sym))
    (emits ",")
    (emit-constant nil)
    (emits ")")))

(defmethod emit-constant clojure.lang.Keyword [x]
  (if (-> @env/*compiler* :options :emit-constants)
    (let [value (-> @env/*compiler* ::ana/constant-table x)]
      (emits "cljs.core." value))
    (corale-emits-keyword x)))

(defmethod emit-constant clojure.lang.Symbol [x]
  (if (-> @env/*compiler* :options :emit-constants)
    (let [value (-> @env/*compiler* ::ana/constant-table x)]
      (emits "cljs.core." value))
    (corale-emits-symbol x)))

(defn corale-emit-apply-to
  [{:keys [name params env]}]
  (let [arglist (gensym "arglist__")
        delegate-name (str (munge name) "__delegate")]
    (emitln "(function (" arglist "){")
    (doseq [[i param] (map-indexed vector (drop-last 2 params))]
      (emits "var ")
      (emit param)
      (emits " = corale.core.aget(")
      (emitln arglist "," i  ");"))
    (if (< 1 (count params))
      (do
        (emits "var ")
        (emit (last (butlast params)))
        (emitln " = corale.core.aget(" arglist "," (dec (dec (count params))) ");")
        (emits "var ")
        (emit (last params))
        (emitln "= " arglist".slice(" (dec (count params)) ");")
        (emits "return " delegate-name "(")
        (doseq [param params]
          (emit param)
          (when-not (= param (last params)) (emits ",")))
        (emitln ");"))
      (do
        (emits "var ")
        (emit (last params))
        (emitln " = " arglist ";")
        (emits "return " delegate-name "(")
        (doseq [param params]
          (emit param)
          (when-not (= param (last params)) (emits ",")))
        (emitln ");")))
    (emits "})")))

(defn corale-emit-variadic-fn-method
  [{:keys [type name variadic params expr env recurs max-fixed-arity] :as f}]
  (emit-wrap env
    (let [name (or name (gensym))
          mname (munge name)
          delegate-name (str mname "__delegate")]
      (emitln "(function() { ")
      (emits "var " delegate-name " = function (")
      (doseq [param params]
        (emit param)
        (when-not (= param (last params)) (emits ",")))
      (emitln "){")
      (when recurs (emitln "while(true){"))
      (emits expr)
      (when recurs
        (emitln "break;")
        (emitln "}"))
      (emitln "};")

      (emitln "var " mname " = function (" (comma-sep
                                             (if variadic
                                               (concat (butlast params) ['var_args])
                                               params)) "){")
      (when type
        (emitln "var self__ = this;"))
      (when variadic
        (emits "var ")
        (emit (last params))
        (emitln " = null;")
        (emitln "if (arguments.length > " (dec (count params)) ") {")
        (let [a (emit-arguments-to-array (dec (count params)))]
          (emitln "  " (last params) " = " a ";"))
        (emitln "} "))
      (emits "return " delegate-name ".call(this,")
      (doseq [param params]
        (emit param)
        (when-not (= param (last params)) (emits ",")))
      (emits ");")
      (emitln "};")

      (emitln mname ".cljs$lang$maxFixedArity = " max-fixed-arity ";")
      (emits mname ".cljs$lang$applyTo = ")
      (corale-emit-apply-to (assoc f :name name))
      (emitln ";")
      (emitln mname ".cljs$core$IFn$_invoke$arity$variadic = " delegate-name ";")
      (emitln "return " mname ";")
      (emitln "})()"))))

(defmethod emit* :fn
  [{:keys [name env methods max-fixed-arity variadic recur-frames loop-lets]}]
  ;;fn statements get erased, serve no purpose and can pollute scope if named
  (when-not (= :statement (:context env))
    (let [loop-locals (->> (concat (mapcat :params (filter #(and % @(:flag %)) recur-frames))
                                   (mapcat :params loop-lets))
                           (map munge)
                           seq)]
      (when loop-locals
        (when (= :return (:context env))
          (emits "return "))
        (emitln "((function (" (comma-sep (map munge loop-locals)) "){")
        (when-not (= :return (:context env))
          (emits "return ")))
      (if (= 1 (count methods))
        (if variadic
          (corale-emit-variadic-fn-method (assoc (first methods) :name name))
          (emit-fn-method (assoc (first methods) :name name)))
        (let [name (or name (gensym))
              mname (munge name)
              maxparams (apply max-key count (map :params methods))
              mmap (into {}
                         (map (fn [method]
                                [(munge (symbol (str mname "__" (count (:params method)))))
                                 method])
                              methods))
              ms (sort-by #(-> % second :params count) (seq mmap))]
          (when (= :return (:context env))
            (emits "return "))
          (emitln "(function() {")
          (emitln "var " mname " = null;")
          (doseq [[n meth] ms]
            (emits "var " n " = ")
            (if (:variadic meth)
              (corale-emit-variadic-fn-method meth)
              (emit-fn-method meth))
            (emitln ";"))
          (emitln mname " = function(" (comma-sep (if variadic
                                                    (concat (butlast maxparams) ['var_args])
                                                    maxparams)) "){")
          (when variadic
            (emits "var ")
            (emit (last maxparams))
            (emitln " = var_args;"))
          (emitln "switch(arguments.length){")
          (doseq [[n meth] ms]
            (if (:variadic meth)
              (do (emitln "default:")
                  (let [restarg (munge (gensym))]
                    (emitln "var " restarg " = null;")
                    (emitln "if (arguments.length > " max-fixed-arity ") {")
                    (let [a (emit-arguments-to-array max-fixed-arity)]
                      (emitln restarg " = " a ";"))
                    (emitln "}")
                    (emitln "return " n ".cljs$core$IFn$_invoke$arity$variadic("
                            (comma-sep (butlast maxparams))
                            (when (> (count maxparams) 1) ", ")
                            restarg ");")))
              (let [pcnt (count (:params meth))]
                (emitln "case " pcnt ":")
                (emitln "return " n ".call(this" (if (zero? pcnt) nil
                                                     (list "," (comma-sep (take pcnt maxparams)))) ");"))))
          (emitln "}")
          (emitln "throw(new Error('Invalid arity: ' + arguments.length));")
          (emitln "};")
          (when variadic
            (emitln mname ".cljs$lang$maxFixedArity = " max-fixed-arity ";")
            (emitln mname ".cljs$lang$applyTo = " (some #(let [[n m] %] (when (:variadic m) n)) ms) ".cljs$lang$applyTo;"))
          (doseq [[n meth] ms]
            (let [c (count (:params meth))]
              (if (:variadic meth)
                (emitln mname ".cljs$core$IFn$_invoke$arity$variadic = " n ".cljs$core$IFn$_invoke$arity$variadic;")
                (emitln mname ".cljs$core$IFn$_invoke$arity$" c " = " n ";"))))
          (emitln "return " mname ";")
          (emitln "})()")))
      (when loop-locals
        (emitln ";})(" (comma-sep loop-locals) "))")))))

(defmethod emit* :invoke
  [{:keys [f args env] :as expr}]
  (let [info (:info f)
        fn? (and ana/*cljs-static-fns*
                 (not (:dynamic info))
                 (:fn-var info))
        protocol (:protocol info)
        tag      (ana/infer-tag env (first (:args expr)))
        proto? (and protocol tag
                 (or (and ana/*cljs-static-fns* protocol (= tag 'not-native)) 
                     (and
                       (or ana/*cljs-static-fns*
                           (:protocol-inline env))
                       (or (= protocol tag)
                           ;; ignore new type hints for now - David
                           (and (not (set? tag))
                                (not ('#{any clj clj-or-nil clj-nil number string boolean function object array js} tag))
                                (when-let [ps (:protocols (ana/resolve-existing-var env tag))]
                                  (ps protocol)))))))
        opt-not? (and (= (:name info) 'corale.core/not)
                      (= (ana/infer-tag env (first (:args expr))) 'boolean))
        ns (:ns info)
        js? (or (= ns 'js) (= ns 'Math))
        goog? (when ns
                (or (= ns 'goog)
                    (when-let [ns-str (str ns)]
                      (= (get (string/split ns-str #"\.") 0 nil) "goog"))))
        keyword? (and (= (-> f :op) :constant)
                      (keyword? (-> f :form)))
        [f variadic-invoke]
        (if fn?
          (let [arity (count args)
                variadic? (:variadic info)
                mps (:method-params info)
                mfa (:max-fixed-arity info)]
            (cond
             ;; if only one method, no renaming needed
             (and (not variadic?)
                  (= (count mps) 1))
             [f nil]

             ;; direct dispatch to variadic case
             (and variadic? (> arity mfa))
             [(update-in f [:info]
                (fn [info]
                  (-> info
                    (assoc :name (symbol (str (munge info) ".cljs$core$IFn$_invoke$arity$variadic")))
                    ;; bypass local fn-self-name munging, we're emitting direct
                    ;; shadowing already applied
                    (update-in [:info]
                      #(-> % (dissoc :shadow) (dissoc :fn-self-name))))))
              {:max-fixed-arity mfa}]

             ;; direct dispatch to specific arity case
             :else
             (let [arities (map count mps)]
               (if (some #{arity} arities)
                 [(update-in f [:info]
                    (fn [info]
                      (-> info
                        (assoc :name (symbol (str (munge info) ".cljs$core$IFn$_invoke$arity$" arity)))
                        ;; bypass local fn-self-name munging, we're emitting direct
                        ;; shadowing already applied
                        (update-in [:info]
                          #(-> % (dissoc :shadow) (dissoc :fn-self-name)))))) nil]
                 [f nil]))))
          [f nil])]
    (emit-wrap env
      (cond
       opt-not?
       (emits "!(" (first args) ")")

       proto?
       (let [pimpl (str (munge (protocol-prefix protocol))
                        (munge (name (:name info))) "$arity$" (count args))]
         (emits (first args) "." pimpl "(" (comma-sep (cons "null" (rest args))) ")"))

       keyword?
       (emits f ".cljs$core$IFn$_invoke$arity$" (count args) "(" (comma-sep args) ")")
       
       variadic-invoke
       (let [mfa (:max-fixed-arity variadic-invoke)]
         (emits f "(" (comma-sep (take mfa args))
                (when-not (zero? mfa) ",")
                "[" (comma-sep (drop mfa args)) "])"))
       
       (or fn? js? goog?)
       (emits f "(" (comma-sep args)  ")")
       
       :else
       (if (and ana/*cljs-static-fns* (= (:op f) :var))
         ;; higher order case, static information missing
         (let [fprop (str ".cljs$core$IFn$_invoke$arity$" (count args))]
           (emits "(" f fprop " ? " f fprop "(" (comma-sep args) ") : " f ".call(" (comma-sep (cons "null" args)) "))"))
         (emits f ".call(" (comma-sep (cons "null" args)) ")"))))))

(comment
  (def aenv (assoc-in (ana/empty-env) [:ns :name] 'corale.core))
  (def cenv (cljs.env/default-compiler-env))

  (cljs.env/with-compiler-env cenv
    (cljs.compiler/emit
     (cljs.analyzer/analyze
      aenv
      '(do (def foo (corale.core/fn [a b & c] (loop [dd "e"] dd)))
           (foo 1 2 3)))))

  (cljs.env/with-compiler-env cenv
    (cljs.compiler/emit
     (cljs.analyzer/analyze
      aenv
      '(prn 'e))))
  
  )
