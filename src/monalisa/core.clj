(ns monalisa.core
  "core monalisa code"
  (:use [monalisa.graphics])

  (import
    (javax.swing JFrame JLabel JTextField JButton)
    (java.awt.event ActionListener)
    (java.awt.geom GeneralPath)
    (java.awt.image BufferedImage)
    (java.awt Color)))

(println (str "buffered-image is: " buffered-image))

(defn random-color []
  {
    :r (rand-int 256)
    :g (rand-int 256)
    :b (rand-int 256)
    :a (rand-int 256)
    }
  )

(defn random-point [image-width image-height]
  {
    :x (double (rand-int image-width))
    :y (double (rand-int image-height))
    }
  )

(defn random-polygon [image-width image-height]
  {
    :points (repeatedly 3 #(random-point image-width image-height))
    :color (random-color)
    }
  )

(defn random-polygons [image-width image-height]
  (into [] (repeatedly 100 #(random-polygon image-width image-height))))

(defn path-from-polygon [polygon]
  (let [path (GeneralPath. GeneralPath/WIND_NON_ZERO)
        points (polygon :points)
        starting-point (first points)
        remaining-points (rest points)]

    (.moveTo path (starting-point :x) (starting-point :y))

    (doseq [point remaining-points]
      (.lineTo path (point :x ) (point :y)))

    (.closePath path)
    path))

(defn to-java-color [color]
  (Color. (:r color) (:g color) (:b color) (:a color)))

(defn draw-polygon [graphics-2d polygon]
  (.setColor graphics-2d (to-java-color (polygon :color)))
  (.fill graphics-2d (path-from-polygon polygon)))

(defn draw-polygons
  ([] (draw-polygons (random-polygons (.getWidth buffered-image) (.getHeight buffered-image))))
  ([polygons]
    (let [g2 (.getGraphics buffered-image)
          width (.getWidth buffered-image)
          height (.getHeight buffered-image)]

      (.clearRect g2 0 0 width height)
      (doseq [polygon polygons]
        (draw-polygon g2 polygon))

      (.repaint the-panel))))

(defn image-as-int-array [img]
  (let [raster (.getData img)
        byte-count (* (.getWidth img) (.getHeight img) 3)  ; 3 bytes for RGB
        int-array (int-array byte-count)]

    (.getPixels raster 0 0 (.getWidth img) (.getHeight img) int-array)
    int-array))

(defn difference-squared [val1 val2]
  (let [difference (- val1 val2)]
    (* difference difference)))

(defn compare-images [img1 img2]
  (let [int-array1 (image-as-int-array img1)
        int-array2 (image-as-int-array img2)]
    (reduce + (map difference-squared int-array1 int-array2))))


(defn doit []
  (init-images)
  (show-image)
  (draw-polygons))
