(ns monalisa.graphics
  "monalisa graphics/drawing code"
  (import
    (javax.swing JFrame JLabel JTextField JButton WindowConstants)
    (java.awt.event ActionListener)
    (java.awt BorderLayout Dimension RenderingHints)
    (java.awt.geom GeneralPath)
    (java.awt.image BufferedImage)
    (java.awt Color)))

; forward references
(def buffered-image)  ; for the display output
(def scratch-image)   ; for the comparison images

(defn allocate-int-array-for-image [img]
  (let [byte-count (* (.getWidth img) (.getHeight img) 3)  ; 3 bytes for RGB
        int-array (int-array byte-count)]
    int-array))

(defn copy-image-to-int-array [img int-array]
  (let [raster (.getData img)]
    (.getPixels raster 0 0 (.getWidth img) (.getHeight img) int-array)
    int-array))

(defn difference-squared [val1 val2]
  (let [difference (- val1 val2)]
    (* difference difference)))

(defn init-images []
  (def reference-image (javax.imageio.ImageIO/read (java.io.File. "src/monalisa/mona_lisa_crop.jpg")))
  (let [width  (.getWidth reference-image)
        height (.getHeight reference-image)]

    (def buffered-image (BufferedImage. width height BufferedImage/TYPE_INT_BGR))
    (def scratch-image  (BufferedImage. width height BufferedImage/TYPE_INT_BGR))
    (def target-width (.getWidth reference-image))
    (def target-height (.getHeight reference-image))
    (def reference-image-int-array (allocate-int-array-for-image reference-image))


    ; TODO: must create N of these if we use pmap in the future
    (def scratch-image-int-array (allocate-int-array-for-image scratch-image))

    ; copy reference image data ahead of time since we'll use it for each comparison
    (copy-image-to-int-array reference-image reference-image-int-array)))


(defn show-image []
  (let [panel (proxy [javax.swing.JPanel] []
                (paintComponent [g]
                  (proxy-super paintComponent g)
                  (.drawImage g buffered-image
                    0 0 (.getWidth this) (.getHeight this) nil)))]
    (def the-panel panel) ; surely this is not the proper way to set a "member" field in clojure...
    (.setSize panel (Dimension. target-width target-height))
    (.setPreferredSize panel (Dimension. target-width target-height))
    (doto (javax.swing.JFrame. "Annealer")
      (.add panel BorderLayout/CENTER)
      (.pack)
      (.setDefaultCloseOperation WindowConstants/EXIT_ON_CLOSE)
      (.setVisible true)
      (.toFront)
      )))


(defn to-java-color [color]
  (Color. (:r color) (:g color) (:b color) (:a color)))

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

(defn draw-polygon [graphics-2d polygon]
  (.setColor graphics-2d (to-java-color (polygon :color)))
  (.fill graphics-2d (path-from-polygon polygon)))

(defn draw-polygons [polygons image]
  (let [g2 (.getGraphics image)
        width (.getWidth image)
        height (.getHeight image)]

    (.clearRect g2 0 0 width height)
      (.setRenderingHint g2 RenderingHints/KEY_ANTIALIASING, RenderingHints/VALUE_ANTIALIAS_ON)
;    (.setRenderingHint g2 RenderingHints/KEY_ANTIALIASING, RenderingHints/VALUE_ANTIALIAS_OFF)
    (doseq [polygon polygons]
      (draw-polygon g2 polygon))))

(defn compare-image-to-reference [image]
  (copy-image-to-int-array image scratch-image-int-array)
  (reduce + (map difference-squared reference-image-int-array scratch-image-int-array)))

(defn update-display [polygons]
  (draw-polygons polygons buffered-image)
  (.repaint the-panel))
