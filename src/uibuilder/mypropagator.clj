(ns uibuilder.mypropagator
  (:require [propaganda.system :as psystem :refer [add-value get-value add-propagator]]
            [propaganda.values :as pvalues]
            [clojure.data.priority-map :refer [priority-map]]))

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


(defn cell-deps [cname expr]
  (->> expr
       (clojure.walk/macroexpand-all)
       unbound-syms
       (remove namespace)
       (remove special-symbol?)
       (remove #{cname})
       (distinct)))


;; (defn cellf [cname deps expr]
;;   (let [args (cons cname deps)
;;         f `(fn [args#]
;;              (let [[~cname ~@deps] args#]
;;                ~expr))]
;;     (eval f)))

;; (defn update-deps [sheet cname cdeps]
;;   (let [olddeps (get-in sheet [:indeps cname])
;;         sheet (reduce
;;                (fn [sheet old]
;;                  (update-in sheet [:outdeps old]
;;                             disj cname))
;;                sheet
;;                olddeps)
;;         sheet (reduce
;;                (fn [sheet new]
;;                  (update-in sheet [:outdeps new]
;;                             clojure.set/union #{cname}))
;;                sheet
;;                cdeps)
;;         sheet (assoc-in sheet [:indeps cname]
;;                         cdeps)]
;;     sheet))




(def ^:dynamic *sheet* nil)
(defprotocol ISheet

  (update-cell [sheet cname])
  (update-cell-deps [sheet cname])
  (set-cell [sheet cname init])
  (get-cell [sheet cname])
  (get-triggers [sheet cname])
  (get-triggees [sheet cname])
  (set-cell-fn [sheet cname f trigs dps])
  ;; need a remove cell-fn
  )


(defn set-cell-expr
  ([sheet cname expr]
     (set-cell-expr sheet cname expr (cell-deps cname expr)))
  ([sheet cname expr triggers]
     (let [deps (apply sorted-set (cell-deps cname expr))
           fn (eval
               `(fn [[~@(cons cname deps)]]
                  ~expr))]
       (set-cell-fn sheet cname fn triggers deps))))


;; todo
;; write functions that call set-cell, update-cell, update-cell-deps, etc.


(def messenger (agent nil))
(def out *out*)
(defn msg [msg]
  (send messenger (fn [_ msg]
                    (binding [*out* out]
                      (println msg)
                      msg))
        msg))

(defrecord Sheet [fns vals triggers deps]

  ISheet
  (update-cell [sheet cname]

    (let [f (get fns cname)
          cins (get deps cname)
          sheet (if (every? (partial contains? vals)
                            cins)
                  (let [oldval (get vals cname)
                        fvals (map vals cins)
                        newval (binding [*sheet* sheet]
                                 (f (cons oldval fvals)))]
                    (assoc-in sheet [:vals cname] newval))
                  sheet)]
      sheet))

  (update-cell-deps [sheet cname]
    ;; need to guarentee we process dependencies based on the distance from the observed cell
    ;; also, let's print out if we encounter the same node twice
    ;; still have a problem where you have two paths
    ;; eg. a -> b -> c
    ;;     x -> y -> z -> c
    ;; it's ok if we visit it twice as long as we pass in the old value of c
    ;; but if there is a cycle then we're screwed
    (loop [sheet sheet
           queue (into (priority-map)
                       (for [trig (get-triggees sheet cname)]
                         [trig 1]))
           visited #{cname}]

      (if-let [[cname dist] (first queue)]
        (do
          (if (visited cname)
            (do
              (msg (str "warning! already processed " cname "... skipping..."))
              (recur sheet (pop queue) visited))
            (let [sheet (update-cell sheet cname)
                  visited (conj visited cname)
                  new-dist (inc dist)
                  queue (into (pop queue)
                              (for [trig (get-triggees sheet cname)]
                                [trig new-dist]))]
              (recur sheet queue visited))))
        sheet)))

  (set-cell [sheet cname val]
    (assoc-in sheet [:vals cname] val))

  (get-cell [sheet cname]
    (get vals cname))

  (get-triggers [sheet cname]
    (get triggers cname))

  (get-triggees [sheet cname]
    (for [[trigee triggers] triggers
          :when (contains? triggers cname)
          ]
    trigee))

  (set-cell-fn [sheet cname f trigs dps]
    (-> sheet
        (assoc-in [:fns cname] f)
        (assoc-in [:triggers cname] (apply sorted-set trigs))
        (assoc-in [:deps cname] (apply sorted-set dps)))))


(defn make-sheet []
  (Sheet. {} (ns-publics 'clojure.core) {} {}))


