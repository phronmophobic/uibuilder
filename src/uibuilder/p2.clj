(ns uibuilder.p2
  (:require clojure.walk
            clojure.stacktrace
            [clojure.core.async :as async :refer [go
                                                  >!
                                                  <!
                                                  <!!
                                                  >!!
                                                  chan
                                                  thread
                                                  timeout
                                                  mult
                                                  tap
                                                  close!
                                                  put!
                                                  take!]])
  (:use [uibuilder.analyze :only [cell-deps]]
        ))

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
                  ;; should filter locals too!
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
        (shake @*root* (quote ~name)))
       (deref ~name)
       )))

(defmacro defval
  ([name val]
     (-defval name (cell-deps val) val))
  ([name deps val]
     (-defval name deps val)))

(defmacro deffn [name & args]
  `(defval ~name (fn ~@args)))


(defn reduce<
  ([f init ch]
     (reduce< f init ch nil))
  ([f init ch buf-or-n]
     (let [out (chan buf-or-n)]
       (go (loop [last init]
             (let [v (<! ch)]
               (when (not (nil? v))
                 (let [next (f last v)]
                  (>! out next)
                  (recur next)))))
           (close! out))
       out)))

(defmacro defstream
  ([name init f ch]
     `(do
        (let [init# ~init]
          (defonce ~name (ref nil))
          (dosync
           (ref-set ~name init#)
           (alter *root* assoc-in [:refs (quote ~name)] ~name)))


       (let [ch# ~ch
             f# ~f
             out# *out*]
         (go
          (try
            (loop []
              (let [v# (<! ch#)]
                (when (not (nil? v#))
                  (dosync
                   (let [last# (deref ~name)
                         next# (f# last# v#)]
                     (when (not= next# last#)
                       (ref-set ~name next#)
                       (doseq [[other-sym# deps#] (:deps @*root*)
                               :when (and (deps# (quote ~name)))]
                         (shake *root* other-sym#)))))
                  (recur))))
            (catch Exception e
              (binding [*out* out#]
                (println (with-out-str
                           (clojure.stacktrace/print-stack-trace e))))))))
       
       (deref ~name))))


