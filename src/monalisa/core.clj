(ns monalisa.core)

(import '(javax.swing JFrame JLabel JTextField JButton)
  '(java.awt.event ActionListener)
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
