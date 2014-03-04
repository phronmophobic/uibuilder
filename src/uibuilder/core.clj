(ns uibuilder.core
  (:use [penumbra.opengl]
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

(defn quad []
  (push-matrix
   (translate -0.5 -0.5 0.5)
   (normal 0 0 1)

   (color (rand) (rand) (rand))
   (vertex 0 0)
   (color (rand) (rand) (rand))
   (vertex 1 0)
   (color (rand) (rand) (rand))
   (vertex 1 1)
   (color (rand) (rand) (rand))
   (vertex 0 1)))

(defn cube []
  (push-matrix
   (draw-quads
    (dotimes [_ 4]
      (rotate 90 0 1 0)
      (quad))
    (rotate 90 1 0 0)
    (quad)
    (rotate 180 1 0 0)
    (quad))))

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

(defcomponent Label [text]
  IBounds
  (-bounds [_]
    (let [f (or *font* (font "Tahoma" :size 20))]
      [(.getWidth f text)
       (.getHeight f text)]))

  IDraw
  (draw [this]
    (with-font (or *font* (font "Tahoma" :size 20))
      (try-with-program
       nil
       (with-disabled [:texture-rectangle :lighting]
         (with-enabled [:texture-2d :blend]
           (let [blend-dst (get-integer :blend-dst)
                 blend-src (get-integer :blend-src)]
             (blend-func :src-alpha :one-minus-src-alpha)
             (TextureImpl/bindNone)
             (.drawString *font* 0 0 text)
             (blend-func blend-src blend-dst)))))))
  )

(defn label [text]
  (Label. text))

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
    (draw-lines
     (doseq [[[x1 y1] [x2 y2]] (map vector points (rest points))]
       (vertex x1 y1)
       (vertex x2 y2)))))
(defn path [& points]
  (Path. points))


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


(defn maybe-ref [val]
  (if (instance? clojure.lang.IDeref val)
    val
    (ref val)))


(declare system)
(defn update-state [state]
  (swap! system add-value 't  (.getTime (Date.)))
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



(defn same [system & cells]
  (reduce
   (fn [system [a b]]
     (add-propagator
      system
      [a]
      (fn [system]
        (add-value system
                   b
                   (get-value system a)))))
   system
   (conj (map vector cells (rest cells))
         [(last cells) (first cells)])))

(defmacro p= [out body]
  (let [symbolfy (fn [sym]
                   (-> sym
                       str
                       rest
                       (->> (apply str))
                       symbol))
        out-sym (symbolfy out)
        sources (->> (tree-seq seqable? seq body)
                     (filter (fn [o]
                               (and (symbol? o)
                                    (.startsWith (str o) "?"))))
                     (distinct)
                     (map symbolfy)
                     (remove #(= % out-sym)))
        body (clojure.walk/postwalk
              (fn [obj]
                (if (and (symbol? obj)
                         (.startsWith (str obj) "?"))
                  `(get-value ~'system (quote ~(symbolfy obj)))
                  obj))
              body)]
    `(swap! system add-propagator
            [~@(for [source sources]
                 `(quote ~source))]
            (fn [~'system]
              (-> ~'system
                  (add-value 
                          (quote ~out-sym)
                          ~body))))))

(defmacro pwhen [pred & body]
  (let [symbolfy (fn [sym]
                   (-> sym
                       str
                       rest
                       (->> (apply str))
                       symbol))
        sources (->> (tree-seq seqable? seq pred)
                     (filter (fn [o]
                               (and (symbol? o)
                                    (.startsWith (str o) "?"))))
                     (distinct)
                     (map symbolfy))
        pred (clojure.walk/postwalk
              (fn [obj]
                (if (and (symbol? obj)
                         (.startsWith (str obj) "?"))
                  `(get-value ~'system (quote ~(symbolfy obj)))
                  obj))
              pred)
        body (clojure.walk/postwalk
              (fn [obj]
                (if (and (symbol? obj)
                         (.startsWith (str obj) "?"))
                  `(get-value ~'system (quote ~(symbolfy obj)))
                  obj))
              body)]
    `(swap! system add-propagator
            [~@(for [source sources]
                 `(quote ~source))]
            (fn [~'system]
              (if ~pred
                (-> ~'system
                    ~@body)
                ~'system)))))


(do

  (def system (atom (psystem/make-system
                     (fn [a b]
                       (if (= :propaganda.values/nothing b)
                         a
                         b))
                     (pvalues/default-contradictory?))))


 (swap! system add-value 'v1 "hi ")
 (swap! system add-value 'v2 "bob: ")
 (swap! system add-value 'x 10)
 (swap! system add-value 'y 10)


 (swap! system add-value 'mouse-x 0)
 (swap! system add-value 'mouse-y 0)


 (p= ?text
     (str "hi " ?x ", " ?y))
 (swap! system add-value 'x2 0)
 (swap! system add-value 'y2 0)
 (pwhen ?t
   (add-value 'x2
              (+ ?x2
                 (* (- ?mouse-x ?x2)
                    0.3))))
 (pwhen ?t
   (add-value 'y2
              (+ ?y2
                 (* (- ?mouse-y ?y2)
                    0.3))))
 (swap! system add-value 'width 500)
 (swap! system add-value 'height 500)
 (swap! system add-value 'random-text "rando")
 (swap! system add-value 't 1)
 (p= ?thalf (long (/ ?t 300)))
 (swap! system add-value 'mynum 100)
 (p= ?mynum
     (do ?thalf
         (dec ?mynum)))
 (swap! system add-value 'mynums [])
 (p= ?mynums
     (conj ?mynums ?thalf))
 (p= ?t1 (int (/ ?t 1000)))
 (swap! system add-value 'letters [])
 (swap! system add-value 'keypress "a")
 (swap! system add-value 'mouse-down false)
 (swap! system add-value 'rx 0)
 (swap! system add-value 'ry 0)
 (pwhen ?mouse-down
     (add-value 'rx ?mouse-x)
     (add-value 'ry ?mouse-y))
 (p= ?rwidth (- ?mouse-x ?rx))
 (p= ?rheight (- ?mouse-y ?ry))

 (p= ?letters
     (cond
      (= :back ?keypress)
      (vec (butlast ?letters))

      (string? ?keypress)
      (conj ?letters ?keypress)

      :else
      ?letters))
 (swap! system add-value 'awayx 250)
 (swap! system add-value 'awayy 240)
 (swap! system add-value 'awaytext ":)")
 (p= ?pclose
     (< (+ (Math/abs (- ?mouse-x ?awayx))
                 (Math/abs (- ?mouse-y ?awayy)))
        120))
 (p= ?awaytext
     (if ?pclose
       (str (-  ?awayx ?mouse-x))
       ":)"))
 (p= ?awayx
     (if ?pclose
       ?awayx
       #_(+ ?awayx
          (* 10
             (/ (-  ?awayx ?mouse-x)
                120)))
       ?awayx))
 (p= ?awaything
     (move ?awayx ?awayy
           (label ?awaytext)))

 (swap! system add-value 'arcx 200)
 (swap! system add-value 'arcy 50)
 (p= ?arcx
     (+ ?mouse-x
        (* 20 (Math/sin (/ ?t 300)))))
 (p= ?arcy
     (+ ?mouse-y
        (* 20 (Math/cos (/ ?t 300)))))
 (p= ?components
     (group
      (move ?x ?y
            (label ?text))
      (move ?x ?y
            (rectangle ?width ?height))
      (move 200 200
            (label (str ?mynum " bottles on the wall")))
      (move 200 100
            (label (str "letters: " (apply str ?letters))))
      (move 200 50
            (label (str "keypress: " ?keypress)))
      (move 300 300
            (label ?random-text))
      (move ?arcx ?arcy
            (arc 30 0 (* 2 Math/PI (Math/cos (/ ?t 300)))))
      ?awaything
      (move 100 200
            (apply
             group
             (for [[i num] (map-indexed vector (take-last 10 ?mynums))]
               (move 0 (* i 30) (label (str num))))))
      (when (every? #(not= % :propaganda.values/nothing)
                    [?rx ?ry ?rheight ?rwidth])
        (move ?rx ?ry
              (rectangle ?rwidth ?rheight)))
      (move ?x2
            ?y2
            (label (str "moving " (int ?x2) ", " (int ?y2))))))
 
 (p= ?width
     (* 0.5 (-  ?x2 ?x)))
 (p= ?height
     (max 100 (* 0.5 ?width)))
 
 
 (p= ?t30 (long (/ ?t 30)))

 (p= ?random-text
     (do
       ?t1
       (str (mod ?t1 1000) " " (random-string))))
 (swap! system same 'x 'mouse-x)
 (swap! system same 'y 'mouse-y)


 )

(defn display [[dt t] state]
  (render-mode :solid)

  (let [[x-origin y-origin w h] @penumbra.opengl.core/*view*]
    (let [root (get-value @system 'components)]
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
  state)

(defn box-contains? [[x y width height] [px py]]
  (and (<= px (+ x width))
       (>= px x)
       (<= py (+ y height))
       (>= py y)))


(defn mouse-move [[dx dy] [mx my] state]
  "Called the same as :mouse-drag, but when no button is pressed."

  state)



(defn mouse-down [[x y] button state]
  "Called whenever a button is pressed."
  (swap! system add-value 'mouse-x x)
  (swap! system add-value 'mouse-y y)
  (swap! system add-value 'mouse-down true)
  (-> state
      (assoc :move-hello? (not (:move-hello? state)))))


(defn mouse-up [[x y] button state]
  "Called whenever a button is released."
  (swap! system add-value 'mouse-x x)
  (swap! system add-value 'mouse-y y)
  (swap! system add-value 'mouse-down false)
  (-> state
      (assoc :mousedown false)))


(defn mouse-click [[x y] button state]
  "Called whenever a mouse is clicked (pressed and released). Gives [x y] of where mouse was originally pressed."
  state)


(defn key-press [key state]
  "Called whenever a key is pressed. If the key is something that would normally show up in a text entry field, key is a case-sensitive string. Examples include “a”, “&”, and " ". If it is not, key is a keyword. Examples include :left, :control, and :escape"

  
  (swap! system add-value 'keypress nil)
  (swap! system add-value 'keypress key)

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
          (swap! system add-value 'mouse-x mx)
          (swap! system add-value 'mouse-y my)
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


;; (defcomponent toolbar [things]
;;   :things things
;;   :rules
;;   [(evenly-spaced (children this))])

;; (defcomponent textarea
;;   :things []
;;   :?)

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

