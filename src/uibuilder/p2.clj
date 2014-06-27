(ns uibuilder.p2
  (:require clojure.walk
            clojure.stacktrace)
  (:use [uibuilder.analyze :only [cell-deps]]))

(defrecord Environment [refs forms deps])
(def ^:dynamic *root* (ref (Environment. {} {} {})))

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


(defmacro watch [name]
  `(add-watch ~name :p
              (fn [_# _# _# v#]
                (println ~(str name) v#)))
  )

(defn shake [env sym]
  (dosync
   (let [ref (get-in env [:refs sym])
         oldval @ref
         ;; _ (println "replacements: " (into {}
         ;;                     (for [[name ref] (:refs env)]
         ;;                       [name (deref ref)])))
         ;; _ (println "eval: " (clojure.walk/postwalk-replace
         ;;                      (into {}
         ;;                     (for [[name ref] (:refs env)]
         ;;                       [name (deref ref)]))
         ;;               (get-in env [:forms sym])))
         f (get-in env [:forms sym])
         args (for [dep (get-in env [:deps sym])]
                (deref (get-in env [:refs dep])))
         newval (apply f args)]
     (ref-set ref
             newval)
     (when (not= oldval newval)
      (doseq [[other-sym deps] (:deps env)
              :when (and (deps sym)
                         )]
        (shake env other-sym))))
   ))



(defn- -defval [name deps val]
  (let [deps (->> deps
                  (remove #{name})
                  (filter (set (keys (:refs @*root*))))
                  set)
        ]
    `(do
       (defonce ~name (ref nil))
       (dosync
        (alter *root* assoc-in [:forms (quote ~name)]
               (fn [~@deps]
                 ~val))
        (alter *root* assoc-in [:deps (quote ~name)] (quote ~deps))
        (alter *root* assoc-in [:refs (quote ~name)] ~name)
        (shake @*root* (quote ~name))
        (deref ~name))
       )))

(defmacro defval
  ([name val]
     (-defval name (cell-deps val) val))
  ([name deps val]
     (-defval name deps val)))

(defmacro deffn [name & args]
  `(defval ~name (fn ~@args)))









