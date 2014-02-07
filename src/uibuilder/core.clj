(ns uibuilder.core
  (:use [penumbra.opengl]
        [penumbra.utils :only [defn-memo]])
  (:require [penumbra.app :as app]
            [penumbra.app.loop :as loop]
            [penumbra.opengl.core :refer [get-integer]]
            [penumbra.app.controller :as controller]
            [clojure.walk]
            [clojure.core.async :as async :refer [go >! <! chan thread timeout]])
  (:import [java.awt Font]
           [java.awt.font TextAttribute]
           [org.newdawn.slick TrueTypeFont]
           [org.newdawn.slick.opengl TextureImpl])
  (:import [java.awt Font]


           [org.newdawn.slick.opengl TextureImpl])
  (:gen-class))

(defmacro post-load [& body]
  `(do
     (defonce loaded# false)
     (when loaded#
       ~@body)
     (def loaded# true)))


(defmacro defcomponent [name [& fields] & opts+specs]
  `(defrecord ~name [ ~@fields]
     ~@(clojure.walk/postwalk-replace
         (into {} (for [field fields]
                    [field `(deref ~field)]))
         opts+specs)))

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

(defprotocol IBounds
  (-bounds [this]))

(defn bounds [x]
  (when-not (or (satisfies? IBounds x) (satisfies? IDraw x))
    (throw (Exception. (str "Expecting IBounds or IDraw, got " x))))
  (if (satisfies? IBounds x)
    (-bounds x)
    [0 0]))

(defprotocol IOrigin
  (-origin [this]))

(defn origin [x]
  (when-not (or (satisfies? IOrigin x) (satisfies? IDraw x))
    (throw (Exception. (str "Expecting IOrigin or IDraw, got " x))))
  (if (satisfies? IOrigin x)
    (-origin x)
    [0 0]))

(defprotocol IChildren
  (-children [this]))

(defn children [x]
  (when-not (or (satisfies? IChildren x) (satisfies? IDraw x))
    (throw (Exception. (str "Expecting IChildren or IDraw, got " x))))
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

(defcomponent Group [drawables]
  IDraw
  (draw [this]
    (doseq [drawable drawables]
      (draw @drawable)))
  IChildren
  (-children [this]
    (map deref drawables)))


(defcomponent Widget [drawable x y]
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


(def widgets (atom (Group. (ref []))))

(def mys (ref "hi"))
(dosync
 (ref-set mys "adsfadsf"))



(defcomponent Transform [rval f]
  clojure.lang.IDeref
  (deref [this]
    (f rval)))



(defmethod print-method Transform [v ^java.io.Writer w]
  (.write w "<Transform>"))
(defn transform [rval f]
  (Transform. rval f))


(defcomponent Path [points]
  IBounds
  (-bounds [this]
    (let [maxx (apply max (map first points))
          maxy (apply max (map second points))]
      [maxx maxy]))
  IDraw
  (draw [this]
    (draw-lines
     (doseq [[x y] points]
       (vertex x y)))))




(def label (Widget. (ref (Label. mys)) (ref 100) (ref 100)))
(def label2 (Widget. (ref (Label. (transform mys (ref #(str "count: " (count %)))))) (ref 100) (ref 200)))
(reset! widgets (Group. (ref [(ref label) (ref label2) (ref (Path. (ref [[100 100] [200 200]])))])))

(defn print-vars
  ([]
     (doseq [[i widget] (map-indexed vector @widgets)]
       (print-vars @widget [i])))
  ([widget path]
     (println path (type widget))
     (when (map? widget)
       (doseq [[k v] widget]
         (print-vars @v (conj path k))))))

(defn get-var
  ([[k & path]]
     (get-var (get @widgets k) path))
  ([obj path]
     (loop [obj obj
            [k & path] path]
       (if k
         (recur (get @obj k) path)
         obj))))





(defn update-state [state]
  (-> state
      (assoc :widgets @widgets)
      
      ))

(defn init [state]
  (render-mode :wireframe)
  (app/periodic-update! 30  #'update-state )
  (app/vsync! true)
  (assoc state
    :widgets @widgets
    :focus nil))

(defn reshape [[x y width height] state]
;;  (frustum-view 60.0 (/ (double width) height) 1.0 100.0)
  (load-identity)
  (translate 0 0 -4)
  (light 0 :position [1 1 1 0])
  state)

(defn display [[dt t] state]
  (render-mode :solid)

  (let [[x-origin y-origin w h] @penumbra.opengl.core/*view*]
              (with-projection (ortho-view x-origin (+ x-origin w) (+ y-origin h) y-origin -1 1)
                (push-matrix
                  (load-identity)
                  (TextureImpl/bindNone)
                  (draw (:widgets state)))))

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

;;  (println mx my)
  (try
   (doseq [widget (children (:widgets state))
           :let [[x y] (origin widget)]
           subwidget (children widget)]
     ;; (println "checking " subwidget x y (width subwidget) (height subwidget))
     (when (and (instance? Label subwidget)
                (box-contains? [x y (width subwidget) (height subwidget)]
                               [mx my]))
       (println "booya!" subwidget))
     )
   (catch Exception e
     (println (with-out-str
                (clojure.stacktrace/print-stack-trace e))))
   )

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
  (when (string? key)
    (dosync
     (alter mys #(str % key))))
  (when (= key :back)
    (dosync
     (alter mys #(subs % 0 (max 0 (dec (count %)))))))

  state)


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
                         :key-type #'key-type
                         }
                        {:rot 0 :fluid true
                         :width 650
                         :height 878
                         :top 0
                         :jiggle-y 0
                         :jiggle-x 0}))
 
   (app/start-single-thread @current-app loop/basic-loop)))









(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))










