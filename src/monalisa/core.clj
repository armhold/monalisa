(ns monalisa.core)

(import '(javax.swing JFrame JLabel JTextField JButton)
  '(java.awt.event ActionListener)
  '(java.awt.geom GeneralPath)
  '(java.awt Color)
  '(java.awt GridLayout))

(defn init-images []
    (def reference-image (.getImage (javax.swing.ImageIcon. "src/monalisa/Rouen_Cathedrale.jpg")))
    (def buffered-image nil)

  (let [width  (/ (.getWidth reference-image) 2)
        height (/ (.getHeight reference-image) 2)]

    (def buffered-image (java.awt.image.BufferedImage. width height java.awt.image.BufferedImage/TYPE_4BYTE_ABGR))))


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

(defn random-color []
  (Color. (rand-int 256) (rand-int 256) (rand-int 256) (rand-int 256)))


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

(defn draw-polygon [graphics-2d polygon]
  (.setColor graphics-2d (polygon :color))
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

(defn doit []
  (init-images)
  (show-image)
  (draw-polygons))
