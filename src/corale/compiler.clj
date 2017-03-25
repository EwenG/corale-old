(in-ns 'cljs.compiler)

(defn override-emit []
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
            (emit-variadic-fn-method (assoc (first methods) :name name))
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
                (emit-variadic-fn-method meth)
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
                        (emitln restarg " = new cljs.core.IndexedSeq(" a ",0);"))
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
          (emitln ";})(" (comma-sep loop-locals) "))"))))))



(comment
  (def aenv (assoc-in (ana/empty-env) [:ns :name] 'corale.core))
  (def cenv (cljs.env/default-compiler-env))
  
  (cljs.env/with-compiler-env cenv
    (cljs.compiler/emit
     (cljs.analyzer/analyze
      aenv
      '(corale.core/defn foo ([a b & c] nil)))))
  )
