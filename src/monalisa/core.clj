(ns monalisa.core
  "core monalisa code"
  (:use [monalisa.graphics]))

(def MAX-ITERATIONS 100000)
(def POLYGON-COUNT 100)    ; # of polygons for a given image
(def POPULATION-COUNT 10)

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
  ([image-width image-height] into [] (repeatedly POLYGON-COUNT #(random-polygon image-width image-height))))


(defn create-random-population []
  (into [] (repeatedly POPULATION-COUNT #(random-polygons))))

(defn evaluate-candidate [candidate]
  (draw-polygons candidate)
  (current-image-difference))

(defn replace-worst [population]
  (println (str "replacing worst in: " population))

  (let [worst (apply min-key evaluate-candidate population)
        population (remove #(= worst %) population)
        population (conj population (random-polygons))]
    population))

(defn run []
  (loop [n 0 population (create-random-population)]
    (when (< n MAX-ITERATIONS)
      (recur (inc n) (replace-worst population)))))

(defn doit []
  (init-images)
  (show-image)
  (draw-polygons (random-polygons)))

