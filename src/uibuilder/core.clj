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
            [uibuilder.p2 :refer [defval deffn watch]]
            [clojure.stacktrace])
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
  (when-not (or (satisfies? IBounds x) (satisfies? IComponent x))
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
    [drawable])
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

(defn update-state [state]
  state)

(defn init [state]
  (render-mode :wireframe)
  ;; (app/periodic-update! 30  #'update-state )
  (app/vsync! true)
  state)

(defn reshape [[x y width height] state]
;;  (frustum-view 60.0 (/ (double width) height) 1.0 100.0)
  (load-identity)
  (translate 0 0 -4)
  (light 0 :position [1 1 1 0])
  state)



(defval components nil)
(defn display [[dt t] state]
  (render-mode :solid)

  (let [[x-origin y-origin w h] @penumbra.opengl.core/*view*]
    (when-let [root @components]
      (with-projection (ortho-view x-origin (+ x-origin w) (+ y-origin h) y-origin -1 1)
        (push-matrix
         (load-identity)
         (TextureImpl/bindNone)
         (draw root)))))

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
  (-> state
      (assoc :move-hello? (not (:move-hello? state)))))


(defn mouse-up [[x y] button state]
  "Called whenever a button is released."
  (-> state
      (assoc :mousedown false)))


(defn mouse-click [[x y] button state]
  "Called whenever a mouse is clicked (pressed and released). Gives [x y] of where mouse was originally pressed."
  state)


(defn key-press [key state]
  "Called whenever a key is pressed. If the key is something that would normally show up in a text entry field, key is a case-sensitive string. Examples include “a”, “&”, and " ". If it is not, key is a keyword. Examples include :left, :control, and :escape"


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
        (when-let [[[dx dy :as mdelta] [mx my :as mp] button] (<! md)]
          (defval mouse-x mx)
          (defval mouse-y my)
          )
        (recur))))
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


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))


(break)

(add-watch components :redraw
           (fn [k r o n]
             (app/repaint! @current-app)))

(defval x 10)
(defval y (or mouse-x 10))
(defval mouse-x 0)
(defval mouse-y 0)

(deffn make-label [s x y]
  (move x y
        (label s)))

(defval selected nil)


(defn find-click [root [x y]]
  (let [[ox oy] (origin root)
        [width height] (bounds root)
        _x (- x ox)
        _y (- y oy)]
   (if-let [clicked (first (keep #(find-click % [_x _y]) (children root)))]
     clicked
     (if (box-contains? [ox oy width height] [x y])
       root
       nil))))

(defn on-click [[[x y] button]]
  (let [s (find-click @components [x y] )]
     (defval selected s))
  )

(let [out *out*]
 (go
  (try
    (let [ch (event-chan @current-app :mouse-up)]
      (loop []
        (when-let [click (<! ch)]
          (#'on-click click)
          (recur))))
    (catch Exception e
      (binding [*out* out]
        (println (with-out-str
                   (clojure.stacktrace/print-stack-trace e))))))))

(defn on-key [[key]]
  (println key)
  (if (instance? Label @selected)
    (defval typed [] (cond
                      (= key :back)
                      (subs @typed 0 (dec (count @typed)))

                      (string? key)
                      (apply str @typed key)))))

(let [out *out*]
 (go
  (try
    (let [ch (event-chan @current-app :key-press)]
      (loop []
        (when-let [key (<! ch)]
          (#'on-key key)
          (recur))))
    (catch Exception e
      (binding [*out* out]
        (println (with-out-str
                   (clojure.stacktrace/print-stack-trace e))))))))

(defval hover-rect (filled-rectangle hover-color 70 70))
(defval hover-color [1 1 1])
(let [out *out*]
 (go
  (try
    (let [ch (event-chan @current-app :mouse-move)]
      (loop []
        (when-let [[_ pos] (<! ch)]
          (defval hover-color []
            (if (= @hover-rect (find-click @components pos))
              [1 0 0]
              [0 1 0]) )
          
          (recur))))
    (catch Exception e
      (binding [*out* out]
        (println (with-out-str
                   (clojure.stacktrace/print-stack-trace e))))))))

(defval typed "")
(defval test-label (make-label (str "typed: " typed) 20 20 ))



(defval components (group
                    test-label
                    (make-label (str "selected: " selected) 20 50 )
                    (move 50 100
                          hover-rect)
                    (move 50 200
                     (rectangle 100 200))
))

@test-label












