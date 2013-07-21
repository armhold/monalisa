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

