(ns uibuilder.analyze
  (:require clojure.walk))


(defn seqable?
  "Returns true if (seq x) will succeed, false otherwise."
  [x]
  (or (seq? x)
      (instance? clojure.lang.Seqable x)
      (nil? x)
      (instance? Iterable x)
      (-> x .getClass .isArray)
      (string? x)
      (instance? java.util.Map x)))


;; topo sort
;; https://gist.github.com/alandipert/1263783
;; clj-graph
;; https://github.com/bonjure/clj-graph/blob/master/src/clj_graph.clj

(defn ignore-loop-and-let-bindings [form]
  ;; assumes already macroexpanded
  (clojure.walk/postwalk
   (fn [form]
     (if (and (seq? form)
              (#{'loop* 'let*} (first form)))
       (let [[letsym bindings & body] form]
         `(~letsym ~(->> bindings (drop 1) (take-nth 2)) ~@body))
       form))
   form))

(defn ignore-new-bindings [form]
  (clojure.walk/postwalk
   (fn [form]
     (if (and (seq? form)
              (= 'new (first form)))
       (let [[newsym constructor-sym body] form]
         body)
       form))
   form))

(defn ignore-fns [form]
  (clojure.walk/prewalk
   (fn [form]
     (if (and (seq? form)
              (= (first form) 'fn*))
       nil
       form))
   form))

(defn unbound-syms [form]
  (cond
   (= '() form) nil

   (seq? form)
   (case (first form)
     (let* loop* )
     (let [[letsym bindings & body] form
           newbindings (apply hash-set (take-nth 2 bindings))
           unbound (concat
                    (unbound-syms (->> bindings (drop 1) (take-nth 2)))
                    (unbound-syms body))
           unbound (remove newbindings unbound)]
       unbound)

     ;; this doesn't cover all of the binding forms
     fn*
     (let [sigs (rest form)
           fn-name (if (symbol? (first sigs)) (first sigs) nil)
           sigs (if fn-name (next sigs) sigs)
           sigs (if (vector? (first sigs)) 
                  (list sigs) 
                  (if (seq? (first sigs))
                    sigs
                    ;; Assume single arity syntax
                    (throw (IllegalArgumentException. 
                            (if (seq sigs)
                              (str "Parameter declaration " 
                                   (first sigs)
                                   " should be a vector")
                              (str "Parameter declaration missing"))))))
           fnsym (first form)
           unbound-body-syms (apply concat
                                    (for [[bindings & body] sigs
                                          :let [bindings (into #{} bindings)]]
                                      (remove bindings (unbound-syms body))))
           unbound (remove #{fn-name} unbound-body-syms)
           ]
       unbound) 

     new
     (let [[newsym classname & args] form]
       (unbound-syms args))

     . (unbound-syms (rest form))

     quote nil

     var nil

     catch (throw (Exception. "can't handle catch") )

     (mapcat unbound-syms form))

   (symbol? form) (list form)

   (seqable? form) (mapcat unbound-syms form)

   :default nil))


(defn cell-deps
  ([expr]
     (->> expr
          (clojure.walk/macroexpand-all)
          unbound-syms
          (remove namespace)
          (remove special-symbol?)
          (distinct)))
  ([cname expr]
     (->> expr
          (clojure.walk/macroexpand-all)
          unbound-syms
          (remove namespace)
          (remove special-symbol?)
          (remove #{cname})
          (distinct))))

