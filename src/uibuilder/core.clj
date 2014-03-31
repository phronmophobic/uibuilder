(ns uibuilder.core
  (:use [penumbra.opengl :exclude [color]]
        [penumbra.utils :only [defn-memo]])
  (:require [penumbra.app :as app]
            [penumbra.app.event :as event]
            [penumbra.app.loop :as loop]
            [penumbra.opengl.core :refer [get-integer]]
            [penumbra.app.controller :as controller]
            [clojure.walk]
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
                                                  take!]]
            [propaganda.system :as psystem :refer [add-value get-value add-propagator]]
            [uibuilder.mypropagator :as mypropagator :refer [get-cell set-cell-expr update-cell update-cell-deps cell-deps set-cell]]
            [propaganda.values :as pvalues]
)
  (:import [java.awt Font]
           [java.awt.font TextAttribute]
           [org.newdawn.slick TrueTypeFont]
           [org.newdawn.slick.opengl TextureImpl])
  (:import [java.awt Font]
           [java.util Date]

           [org.newdawn.slick.opengl TextureImpl])
  (:gen-class))

(defn random-char []
  (rand-nth "abcdefghijklmnopqrstuvwxyz"))
(defn random-string []
  (apply str (for [i (range (rand-int 20))]
               (random-char))))



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


(defmacro post-load [& body]
  `(do
     (defonce loaded# false)
     (when loaded#
       ~@body)
     (def loaded# true)))


(defmacro defcomponent [name [& fields] & opts+specs]
  `(do
     (defrecord ~name [ ~@fields]
       IComponent
       ~@opts+specs
       ;; ~@(->> opts+specs
       ;;     (clojure.walk/postwalk-replace
       ;;      (into {} (for [field fields]
       ;;                 [field `(deref ~field)])))
       ;;     (clojure.walk/postwalk
       ;;      (fn [form]
       ;;        (if (and (seq? form)
       ;;                 (= 'clojure.core/unquote (first form)))
       ;;          (-> form
       ;;              second
       ;;              second)
       ;;          form))))
       )
     #_(defn ~(symbol (.toLowerCase (clojure.core/name name))) [~@fields]
       (~(symbol (str (clojure.core/name name) "."))
        ~@(for [field fields]
            `(if (instance? clojure.lang.IDeref ~field)
               ~field
               (ref ~field))) ))))

(defprotocol IDraw
  (draw [this]))

(extend-protocol IDraw
  nil
  (draw [this]))

(defprotocol IComponent)

(defprotocol IBounds
  (-bounds [this]))

(defn bounds [x]
  (when-not (satisfies? IBounds x) (satisfies? IComponent x)
    (throw (Exception. (str "Expecting IBounds or IComponent, got " x))))
  (if (satisfies? IBounds x)
    (-bounds x)
    [0 0]))

(defprotocol IOrigin
  (-origin [this]))

(defn origin [x]
  (when-not (or (satisfies? IOrigin x) (satisfies? IComponent x))
    (throw (Exception. (str "Expecting IOrigin or IComponent, got " x))))
  (if (satisfies? IOrigin x)
    (-origin x)
    [0 0]))

(defprotocol IChildren
  (-children [this]))

(defn children [x]
  (when-not (or (satisfies? IChildren x) (satisfies? IComponent x))
    (throw (Exception. (str "Expecting IChildren or IComponent, got " x))))
  (if (satisfies? IChildren x)
    (-children x)
    []))

(defn width [ibounds]
  (let [[width height] (bounds ibounds)]
    width))
(defn height [ibounds]
  (let [[width height] (bounds ibounds)]
    height))

(def ^:dynamic *font-cache* (atom {}))
(def ^:dynamic *font* nil)

(defmacro with-font [f & body]
  `(binding [*font* ~f]
     ~@body))


(defn-memo text-attribute
  "Takes :keyword and returns TextAttribute/KEYWORD"
  [k]
  (eval `(. TextAttribute ~(-> k name (.replace \- \_) .toUpperCase symbol))))


(defn font [name & modifiers]
  (if-let [font (@*font-cache* (list* name modifiers))]
    font
    (let [hash (-> (apply hash-map modifiers)
                   (update-in [:size] float)
                   (assoc :family name))
          hash (zipmap (map text-attribute (keys hash)) (vals hash))
          font (TrueTypeFont. (Font. hash) true)]
      (swap! *font-cache* assoc (list* name modifiers) font)
      font)))

(defcomponent Label [text options]
  IBounds
  (-bounds [_]
    (let [f (or *font* (font (get options :font "Tahoma")
                             :size (get options :font-size 20)))]
      [(.getWidth f text)
       (.getHeight f text)]))

  IDraw
  (draw [this]
    (with-font (or *font* (font (get options :font "Tahoma")
                                :size (get options :font-size 20)))
      (try-with-program
       nil
       (with-disabled [:texture-rectangle :lighting]
         (with-enabled [:texture-2d :blend]
           (let [blend-dst (get-integer :blend-dst)
                 blend-src (get-integer :blend-src)]
             (push-matrix
              (when (or (get options :x)
                        (get options :y))
                (translate (get options :x 0) (get options :y 0) 0))
              (when-let [color (get options :color)]
                  (apply penumbra.opengl/color color))
              (blend-func :src-alpha :one-minus-src-alpha)
              (TextureImpl/bindNone)
              (.drawString *font* 0 0 text)
              (blend-func blend-src blend-dst))))))))
  )

(defn label [text & options]
  (Label. text (apply hash-map options)))

(defcomponent Group [drawables]
  IDraw
  (draw [this]
    (doseq [drawable drawables]
      (draw drawable)))
  IChildren
  (-children [this]
    drawables))
(defn group [& drawables]
  (Group. drawables))




(defcomponent Move [x y drawable]
  IOrigin
  (-origin [this]
    [x y])
  IChildren
  (-children [this]
    [~drawable])
  IBounds
  (-bounds [this]
    (bounds drawable))
  IDraw
  (draw [this]
    (push-matrix
     (translate x y 0)
     (draw drawable))))
(defn move [x y drawable]
  (Move. x y drawable))

(defn myempty [o]
  (if (instance? clojure.lang.IRecord o)
    o
    (empty o)))

(defn walk
  "Traverses form, an arbitrary data structure.  inner and outer are
  functions.  Applies inner to each element of form, building up a
  data structure of the same type, then applies outer to the result.
  Recognizes all Clojure data structures. Consumes seqs as with doall."

  {:added "1.1"}
  [inner outer form]
  (cond
   (list? form) (outer (apply list (map inner form)))
   (instance? clojure.lang.IMapEntry form) (outer (vec (map inner form)))
   (seq? form) (outer (doall (map inner form)))
   (coll? form) (outer (into (myempty form) (map inner form)))
   :else (outer form)))

;; (defn postwalk
;;   "Performs a depth-first, post-order traversal of form.  Calls f on
;;   each sub-form, uses f's return value in place of the original.
;;   Recognizes all Clojure data structures. Consumes seqs as with doall."
;;   {:added "1.1"}
;;   [f form]
;;   (walk (partial postwalk f) f form))

(defn prewalk
  "Like postwalk, but does pre-order traversal."
  {:added "1.1"}
  [f form]
  (walk (partial prewalk f) identity (f form)))


(defn deref-all [form]
  (prewalk
   (fn [form]
     (if (instance? clojure.lang.IDeref form)
       @form
       form))
   form))


(defcomponent Path [points]
  IBounds
  (-bounds [this]
    (let [maxx (apply max (map first points))
          maxy (apply max (map second points))]
      [maxx maxy]))
  IDraw
  (draw [this]
    (push-matrix
     (draw-lines
      (doseq [[[x1 y1] [x2 y2]] (map vector points (rest points))]
        (vertex x1 y1)
        (vertex x2 y2))))))
(defn path [& points]
  (Path. points))

(defcomponent Polygon [color points]
  IBounds
  (-bounds [this]
    (let [maxx (apply max (map first points))
          maxy (apply max (map second points))]
      [maxx maxy]))
  IDraw
  (draw [this]
    (push-matrix
     (draw-polygon
      (doseq [[x y] points]
        (when color
          (apply penumbra.opengl/color color))
        (vertex x y))))))
(defn polygon [color & points]
  (Polygon. color points))

(defcomponent Arc [radius rad-start rad-end steps]
  IDraw
  (draw [this]
    (let [arc-length (- rad-end rad-start)]
      (draw-line-strip
       (doseq [i (range (inc steps))
               :let [pct (/ (float i) steps)
                     rad (- (+ rad-start
                               (* arc-length pct)))
                     x (* radius (Math/cos rad))
                     y (* radius (Math/sin rad))]]
         (vertex x y))))))

(defn arc [radius rad-start rad-end]
  (Arc. radius rad-start rad-end 10))

(defn rectangle [width height]
  (path [0 0] [0 height] [width height] [width 0] [0 0]))

(defn filled-rectangle [color width height]
  (polygon color [0 0] [0 height]  [width height] [width 0]  [0 0]))



(defn maybe-ref [val]
  (if (instance? clojure.lang.IDeref val)
    val
    (ref val)))


(declare system)
(defn update-state [state]
  (swap! system put-cell 't  (.getTime (Date.)))
  state)

(defn init [state]
  (render-mode :wireframe)
  (app/periodic-update! 30  #'update-state )
  (app/vsync! true)
  state)

(defn reshape [[x y width height] state]
;;  (frustum-view 60.0 (/ (double width) height) 1.0 100.0)
  (load-identity)
  (translate 0 0 -4)
  (light 0 :position [1 1 1 0])
  state)



;; (defn same [system & cells]
;;   (reduce
;;    (fn [system [a b]]
;;      (add-propagator
;;       system
;;       [a]
;;       (fn [system]
;;         (update-cell system
;;                    b
;;                    (get-value system a)))))
;;    system
;;    (conj (map vector cells (rest cells))
;;          [(last cells) (first cells)])))



(defn apply-simple-rule [system rule]
  (let [[cellname constant deps expr]
        (case (count rule)
          (2 4)
          rule
          3
          (let [[cellname constant expr] rule
                deps (->> expr
                          (tree-seq seqable? seq)
                          (filter #(and (symbol? %)
                                        (.startsWith (name %) "?")))
                          (distinct)
                          (remove (partial = cellname)))]
           [cellname constant deps expr])

          (throw (Exception. "Invalid number of arguments for rule")))
        system-cells (->> expr
                          (tree-seq seqable? seq)
                          (filter #(and (symbol? %)
                                        (.startsWith (name %) "?")))
                          (distinct))
        system (add-value system cellname (eval constant))
        system (if (nil? expr)
                 system
                 (let [symbolfy (fn [sym]
                                  (-> sym
                                      str
                                      rest
                                      (->> (apply str))
                                      symbol))
                       system-sym (gensym "system")
                       expr (clojure.walk/postwalk-replace
                             (into {}
                                   (for [cell system-cells]
                                     [cell `(get-value ~system-sym '~cell)]))
                             expr)
                       propagtor-raw `(fn [~system-sym]
                                        (if (some (partial = :propaganda.values/nothing)
                                                  (map (partial get-value ~system-sym)
                                                       [~@(map (partial list 'quote) system-cells)]))
                                          ~system-sym
                                          (add-value ~system-sym '~cellname
                                                     ~expr)))
                       ;; _ (clojure.pprint/pprint propagtor-raw)
                       propagator-fn (eval
                                      propagtor-raw)]
                   (add-propagator system
                                   deps
                                   propagator-fn)))]
    system))


(defn add-rule [system rule]
  (let [[rule-name & rest] rule]
    (assoc-in system [:rules rule-name] rest)))


(defn expand-rule-fn [system rule]
  (let [[[rule-name & args]] rule
        [args* & subrules*] (get-in system [:rules rule-name])
        syms-map (into {} (map vector  args* args))
        subrules (clojure.walk/postwalk
                  (fn [form]
                    (if-let [sym (->> (map first syms-map)
                                      (filter #(and (symbol? form)
                                                    (.startsWith (name form) (str % "."))))
                                      first)]
                      (symbol (str (get syms-map sym) "." (subs (name form) (inc (.indexOf (name form) ".")))))
                      form))
                  subrules*)
        subrules (clojure.walk/postwalk-replace
                  syms-map
                  subrules)]
    subrules))






(defn apply-rule [system rule]
  ;; (println "addring rule " rule)
  (if (list? (first rule))
    (reduce apply-rule system (expand-rule-fn system rule))
    (apply-simple-rule system rule)))


(def test-system (psystem/make-system
                  (fn [a b]
                    (if (= :propaganda.values/nothing b)
                      a
                      b))
                  (pvalues/default-contradictory?)))

(do

  
  (def rules
    '[mouse-x 0
      mouse-y 0
      keypress nil
      mouse-down false

      components nil
      ])

  (def system (atom (mypropagator/make-sheet)))
  (def sheet system)  
  (defn put-cell-init
    ([sheet cname init expr]
       (put-cell-init sheet cname init expr (cell-deps cname expr)))
    ([sheet cname init expr triggers]
       (-> sheet
           (set-cell-expr cname expr triggers)
           (set-cell cname init)
           (update-cell-deps cname))))

  (defn put-cell
    ([sheet cname expr]
       (put-cell sheet cname expr (cell-deps cname expr)))
    ([sheet cname expr triggers]
       (-> sheet
           (set-cell-expr cname expr triggers)
           (update-cell cname)
           (update-cell-deps cname))))

  (defmacro ? [cname]
    `(get-cell @~'sheet (quote ~cname)))

  (defmacro !
    ([cname expr]
       `(do
          (swap! ~'sheet put-cell (quote ~cname) (quote ~expr))
          (? ~cname)))
    ([cname expr triggers]
       `(do
          (swap! ~'sheet put-cell (quote ~cname) (quote ~expr)
                 ~(vec
                   (for [trig triggers]
                     `(quote ~trig))))
          (? ~cname))))


  (defmacro !!
    ([cname init expr]
       `(do
          (swap! ~'sheet put-cell-init (quote ~cname) ~init (quote ~expr))
          (? ~cname)))
    ([cname init expr triggers]
       `(do
          (swap! ~'sheet put-cell-init (quote ~cname) ~init
                 (quote ~expr)
                 ~(vec
                   (for [trig triggers]
                     `(quote ~trig))))
          (? ~cname))))





  (swap! system
         (fn [system]
           (update-in system [:vals]
                      merge (ns-publics 'uibuilder.core))))


  (swap! system
         #(reduce
           (fn [system [cname expr]]
             (mypropagator/put-cell system cname expr))
           %
           (partition 2 rules)))

  ;; (def rules
  ;;   '[[?mouse-x 0]
  ;;     [?mouse-y 0]
  ;;     [?mouse-down false]
  ;;     [?t 1]
  ;;     [?t1 1 (int (/ ?t 1000))]
  ;;     [?oldts '()
  ;;      [?t]
  ;;      (take 2 (cons ?t ?oldts))]
  ;;     [?lastt 0
  ;;      (second ?oldts)]
  ;;     [?dt 1
  ;;      (if (and ?lastt ?t)
  ;;        (- ?t ?lastt)
  ;;        1)]
  ;;     [?thalf 0 (long (/ ?t 300))]
  ;;     [?letters []
  ;;      (cond
  ;;       (= :back ?keypress)
  ;;       (vec (butlast ?letters))

  ;;       (string? ?keypress)
  ;;       (conj ?letters ?keypress)

  ;;       :else
  ;;       ?letters)]
  ;;     [(make-button ?mybutton)]
  ;;     [?acceleration 0.010]
  ;;     [?keypress ""]
  ;;     [?dx 0
  ;;      [?t]
  ;;      (cond

  ;;       ?mouse-down
  ;;       (+ ?dx (* ?acceleration (- ?mouse-x ?mybutton.x)))
        
  ;;       (#{100 500} ?mybutton.x)
  ;;       0
  ;;       :else ?dx)]
  ;;     [?mybutton.x 150
  ;;      [?t]
  ;;      (let [newx (+ ?mybutton.x ?dx)]
  ;;       (cond 
  ;;        (< newx 100)
  ;;        100
  ;;        (> newx 500)
  ;;        500
  ;;        :else newx))]
  ;;     [?draw []]
  ;;     [?components nil
  ;;      (group
  ;;       (move 50 150
  ;;             (label (str "dx: " ?dx )))
  ;;       (move 50 170
  ;;             (label (str "dt: " ?dt )))
  ;;       (move 50 200
  ;;             (label (str "mouse-x: " ?mouse-x )))
  ;;       (move 50 250
  ;;             (label (str "mouse-down: " ?mouse-down )))

  ;;       (move 50 270
  ;;             (label (str "mybutton.x: " ?mybutton.x )))
  ;;       (move 50 290
  ;;             (label (str "keypress: " ?keypress )))
  ;;       (move 50 310
  ;;             (label (str "letters: " (apply str ?letters) )))
  ;;       (apply group ?draw)
  ;;       ?mybutton.drawable)]])

  ;; (def rulefns
  ;;   '[[make-button [?b]
  ;;      [?b.x 0]
  ;;      [?b.y 0]
  ;;      [?b.width 100]
  ;;      [?b.height 100]
  ;;      [?b.drawable nil
  ;;       (group
  ;;        (move ?b.x ?b.y
  ;;              (filled-rectangle [0.3 0.3 0.3]
  ;;                           ?b.width ?b.height)))]]
  ;;     [center-x [?a ?b]
  ;;      [?a.x (+ ?b.x
  ;;               (/ (- ?b.width ?a.width)
  ;;                  2.0))]
  ;;      [?b.x (+ ?a.x
  ;;               (/ (- ?a.width ?b.width)
  ;;                  2.0))]]])

  ;; (def system (atom (psystem/make-system
  ;;                    (fn [a b]
  ;;                      (if (= :propaganda.values/nothing b)
  ;;                        a
  ;;                        b))
  ;;                    (pvalues/default-contradictory?))))
  ;; (swap! system #(reduce add-rule % rulefns))
  ;; (swap! system #(reduce apply-rule % rules))
  
)

(defn display [[dt t] state]
  (render-mode :solid)

  (let [[x-origin y-origin w h] @penumbra.opengl.core/*view*]
    (let [root (get-cell @system 'components)]
      (when-not (= root :propaganda.values/nothing)
        (with-projection (ortho-view x-origin (+ x-origin w) (+ y-origin h) y-origin -1 1)
          (push-matrix
           (load-identity)
           (TextureImpl/bindNone)
           (draw root))))))

  )



(defn close [state]
  "Called once, when application ends.")

(defn mouse-drag [[dx dy] [x y] button state]
  "Called when mouse moves with a button pressed. [dx dy] contains relative motion since last time :mouse-drag was called, and [x y] contains absolute position of the mouse. button will be equal to one of :left, :right, :center, :mouse-4, or :mouse-5. If the mouse is moving when two or more buttons are pressed, :mouse-drag will be called once for each button."
  (swap! system put-cell 'mouse-x x)
  (swap! system put-cell 'mouse-y y)
  state)

(defn box-contains? [[x y width height] [px py]]
  (and (<= px (+ x width))
       (>= px x)
       (<= py (+ y height))
       (>= py y)))


(defn mouse-move [[dx dy] [mx my] state]
  "Called the same as :mouse-drag, but when no button is pressed."
  (swap! system put-cell 'mouse-x mx)
  (swap! system put-cell 'mouse-y my)
  state)



(defn mouse-down [[x y] button state]
  "Called whenever a button is pressed."
  (swap! system put-cell 'mouse-x x)
  (swap! system put-cell 'mouse-y y)
  (swap! system put-cell 'mouse-down true)
  (-> state
      (assoc :move-hello? (not (:move-hello? state)))))


(defn mouse-up [[x y] button state]
  "Called whenever a button is released."
  (swap! system put-cell 'mouse-x x)
  (swap! system put-cell 'mouse-y y)
  (swap! system put-cell 'mouse-down false)
  (-> state
      (assoc :mousedown false)))


(defn mouse-click [[x y] button state]
  "Called whenever a mouse is clicked (pressed and released). Gives [x y] of where mouse was originally pressed."
  state)


(defn key-press [key state]
  "Called whenever a key is pressed. If the key is something that would normally show up in a text entry field, key is a case-sensitive string. Examples include “a”, “&”, and " ". If it is not, key is a keyword. Examples include :left, :control, and :escape"

  
  (swap! system put-cell 'keypress nil)
  (swap! system put-cell 'keypress key)

  state)


(defn key-release [key state]
  "Called whenever a key is released."
  state)


(defn key-type [key state]
  "Called whenever a key is pressed and released. This is the only key event which will expose auto-repeated keys."
  state)



(defn event-chan
  ([app event-types] (event-chan app event-types (chan)))
  ([app event-types ch]
     (let [events #{:display
                    :init
                    :close
                    :reshape
                    :mouse-drag
                    :mouse-move
                    :mouse-down
                    :mouse-up
                    :mouse-click
                    :key-press
                    :key-release
                    :key-type}
           event-types (if (keyword? event-types)
                             [event-types]
                             event-types)]
       
       (doseq [event-type event-types]
         (when-not (contains? events event-type)
           (throw (Exception. (str "Subscribing to unknown event " event-type))))
        (event/subscribe! app event-type
                          (fn [& args] (put! ch (if (nil? args)
                                                  true
                                                  args)))))
       ch)))

(defonce current-app (atom nil))
(defn start []
  (go
   (when @current-app
     (controller/stop! @current-app)
     (reset! current-app nil))
   (<! (timeout 100))
   (reset! current-app (app/create
                        {:display #'display
                         :init #'init
                         :close #'close
                         :reshape #'reshape
                         :mouse-drag #'mouse-drag
                         :mouse-move #'mouse-move
                         :mouse-down #'mouse-down
                         :mouse-up #'mouse-up
                         :mouse-click #'mouse-click
                         :key-press #'key-press
                         :key-release #'key-release
                         :key-type #'key-type}
                        {:rot 0 :fluid true
                         :width 650
                         :height 878
                         :top 0
                         :jiggle-y 0
                         :jiggle-x 0}))
   (go
    (<! (event-chan @current-app :init))
    (let [md (event-chan @current-app :mouse-drag (chan (async/dropping-buffer 0)))]
      (loop []
        (when-let [[[dx dy :as mdelta] [mx my :as mp]] (<! md)]
          (swap! system put-cell 'mouse-x mx)
          (swap! system put-cell 'mouse-y my)
          ;; (dosync
          ;;  (ref-set mouse-position mp)
          ;;  (ref-set mouse-delta mdelta))
          
          (recur)))))
   #_(go
      (<! (event-chan @current-app :init))
      (let [mu (event-chan @current-app :mouse-up (chan (async/dropping-buffer 0)))]
        (loop []
          (when-let [_ (<! mu)]
            (dosync
             (ref-set mouse-delta [0 0]))
            (recur)))))
   
   (app/start-single-thread @current-app loop/basic-loop))
  
  )





;; (defcomponent rich-text-editor
;;   :things
;;   [[:toolbar
;;     [:toggle :#bold]
;;     [:toggle :#italics "itatlics"]
;;     [:toggle :#underline]
;;     [:dropdown :#font ["Normal"
;;                        "Courier New"
;;                        "Georgia"]]
;;     [:toggle :#bullets "bullets"]
;;     [:toggle :#left-aligned]
;;     [:toggle :#right-aligned]
;;     [:toggle :#center-aligned]]
;;    [:textarea]]
;;   :rules
;;   [(stack :toolbar :textarea)
;;    (one-selected [:#left-aligned
;;                   :#right-aligned
;;                   :#center-aligned])
;;    (= (:textarea/bold) :#bold/on)
;;    (= (:textarea/italics) :#italics/on)
;;    (= (:textarea/underline) :#underline/on)
;;    (= (:textarea/font) :#font/on)
;;    (= (:textarea/bullets) :#bullets/on)
;;    (= (:textarea/alignment) (cond
;;                              (selected #left-aligned) :left
;;                              (selected #right-aligned) :right
;;                              (selected #center-aligned) :center))
;; ])




(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))


(! components (group
               (move x y (label "hi"))))

;; (mypropagator/get-triggees @sheet 't)
;; (mypropagator/set-cell @sheet 't 10)
;; (mypropagator/get-triggers @sheet 'components)

(!! x 0 (+ x dx))

(!! dx
    0
    (if mouse-down
      (+ dx (* (- mouse-x x) 0.01))
      0)
    [t])


(!! y 10 (+ y dy))
(!! dy 0 (if mouse-down
           (+ dy (* (- mouse-y y) 0.01))
           0) [t])


