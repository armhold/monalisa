(ns monalisa.core
  "core monalisa code"
  (:use [monalisa.graphics]))

(def MAX-GENERATIONS 1000000)
(def POLYGON-COUNT 250)    ; # of polygons for a given image
(def POPULATION-COUNT 10)
(def MUTATION-RATE 0.005)

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

(defn evaluate-candidate [polygons]
  (draw-polygons polygons)
  (current-image-difference))

(defn new-individual
  ([] (new-individual (random-polygons)))
  ([polygons] {
      :polygons polygons
      :score (evaluate-candidate polygons)
      }))

(defn create-random-population []
  (into [] (repeatedly POPULATION-COUNT #(new-individual))))


; find index of best scoring candidate in the vector of (pre-computed) scores
(defn find-best-index [scores]
  (first (apply min-key second (map-indexed vector scores))))

(defn find-worst-index [scores]
  (first (apply max-key second (map-indexed vector scores))))

(defn possibly-mutate-polygon [polygon]
  (if (< (rand) MUTATION-RATE)
    (do
      (println "mutating!")
      (random-polygon (.getWidth buffered-image) (.getHeight buffered-image)))
    polygon))


(defn crossover [parent1-polygons parent2-polygons]
  (let [crossover (rand-int POLYGON-COUNT)]
    (loop [n 0 result []]
      (if (< n POLYGON-COUNT)
        (recur (inc n) (conj result (possibly-mutate-polygon (nth (if (< n crossover) parent1-polygons parent2-polygons) n))))
        result))))

(defn mate [parent1 parent2]
  (new-individual (crossover (:polygons parent1) (:polygons parent2))))


(defn replace-worst [population]
  (let [scores (map #(:score %) population)
        index-of-best (find-best-index scores)
        index-of-worst (find-worst-index scores)
        random-individual (population (rand-int POPULATION-COUNT))
        best-individual (population index-of-best)
        new-individual (mate best-individual random-individual)]
    (println (str "ousting: " index-of-worst))
    (assoc population index-of-worst new-individual)))

(defn run []
  (loop [n 0 population (create-random-population)]
    (when (< n MAX-GENERATIONS)
      (println (str "generation: " n))
      (recur (inc n) (replace-worst population))))
  (println "done"))

(defn doit []
  (init-images)
  (show-image)
  (draw-polygons (random-polygons)))

