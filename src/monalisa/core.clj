(ns monalisa.core
  "core monalisa code"
  (:use [monalisa.graphics]))

(println (str "buffered-image is: " buffered-image))


(def MAX-ITERATIONS 100000)
(def POLYGON-COUNT 200)    ; # of polygons for a given image
(def POPULATION-COUNT 10)
(def candidates)
(def current-best)

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

(defn random-polygons
  ([] (random-polygons (.getWidth buffered-image) (.getHeight buffered-image)))
  ([image-width image-height] into [] (repeatedly 100 #(random-polygon image-width image-height))))


(defn create-random-population []
  (into [] (repeatedly POPULATION-COUNT #(random-polygons))))

(defn evaluate-candidate [candidate]
  (draw-polygons candidate)
  (current-image-difference))

(defn run []
  (dotimes [n MAX-ITERATIONS]
    (let [population (create-random-population)
          best-score (reduce min (map evaluate-candidate population))]
      (println (str "best population score: " best-score)))))


(defn doit []
  (init-images)
  (show-image)
  (draw-polygons (random-polygons)))


