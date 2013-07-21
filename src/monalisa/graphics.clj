(ns monalisa.graphics
  "monalisa graphics/drawing code"
  (import
    (javax.swing JFrame JLabel JTextField JButton)
    (java.awt.event ActionListener)
    (java.awt.geom GeneralPath)
    (java.awt.image BufferedImage)
    (java.awt Color)))

; forward reference
(def buffered-image)

(defn init-images []
  (def reference-image (javax.imageio.ImageIO/read (java.io.File. "src/monalisa/Rouen_Cathedrale.jpg")))
  (let [width  (/ (.getWidth reference-image) 2)
        height (/ (.getHeight reference-image) 2)]

    (def buffered-image (BufferedImage. width height BufferedImage/TYPE_INT_BGR))))


(defn show-image []
  (let [panel (proxy [javax.swing.JPanel] []
                (paintComponent [g]
                  (proxy-super paintComponent g)
                  (.drawImage g buffered-image
                    0 0 (.getWidth this) (.getHeight this) nil)))]
    (def the-panel panel) ; surely this is not the proper way to set a "member" field in clojure...
    (doto (javax.swing.JFrame.)
      (.setContentPane panel)
      (.setSize 300 300)
      (.setVisible true)
      (.toFront))))


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

(defn draw-polygons [polygons]
    (let [g2 (.getGraphics buffered-image)
          width (.getWidth buffered-image)
          height (.getHeight buffered-image)]

      (.clearRect g2 0 0 width height)
      (doseq [polygon polygons]
        (draw-polygon g2 polygon))

      (.repaint the-panel)))

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

