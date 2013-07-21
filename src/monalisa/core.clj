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


(defn doit []
  (init-images)
  (show-image)
  (draw-polygons (random-polygons (.getWidth buffered-image) (.getHeight buffered-image))))

