(ns monalisa.core)

(import '(javax.swing JFrame JLabel JTextField JButton)
  '(java.awt.event ActionListener)
  '(java.awt.geom GeneralPath)
  '(java.awt Color)
  '(java.awt GridLayout))

(defn show-image []
  (let [panel (proxy [javax.swing.JPanel] []
                (paintComponent [g]
                  (proxy-super paintComponent g)
                  (.drawImage g (.getImage
                                  (javax.swing.ImageIcon. "src/monalisa/Rouen_Cathedrale.jpg"))
                    0 0 (.getWidth this) (.getHeight this) nil)))]
    (doto (javax.swing.JFrame.)
      (.setContentPane panel)
      (.setSize 300 300)
      (.setVisible true))))

(defn random-color []
  (Color. (rand-int 256) (rand-int 256) (rand-int 256) (rand-int 256)))


(defn random-point [image-width image-height]
  {
    :x (rand-int image-width)
    :y (rand-int image-height)
    }
  )

(defn random-polygon [image-width image-height]
  {
    :points (repeatedly 3 #(random-point image-width image-height))
    :color (random-color)
    }
  )

(defn random-polygons [image-width image-height]
  (repeatedly 10 #(random-polygon image-width image-height)))

(defn draw-line-to-point [path point]
  (.moveTo path (point :x) (point :y))
  (.lineTo path))

(defn path-from-polygon [polygon]
  (let [path (. GeneralPath GeneralPath/WIND_NON_ZERO)]
    (map draw-line-to-point path (polygon :points))
    (.closePath path)
    path))

(defn draw-polygon [graphics-2d polygon]
  (.setColor graphics-2d (polygon :color))
  (.fill graphics-2d (path-from-polygon polygon)))

(defn draw-polygons [buffered-image polygons]

  (let [g2 (.getGraphics buffered-image)
        width (.getWidth buffered-image)
        height (.getHeight buffered-image)]

    (.clearRect g2 0 0 width height)
    (map draw-polygon buffered-image polygons))


;  g2.clearRect(0, 0, w, h);
;  for (Polygon p : getGenes()) {
;                                 g2.setColor(p.getColor());
;                                 g2.fill(p.getPath());
;                                 }
  )

