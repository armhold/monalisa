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

; return list of scores in order of appearance in population
(defn evaluate-candidates [population]
  (map evaluate-candidate population))

; find index of best scoring candidate in the vector of (pre-computed) scores
(defn find-best-index [population-scores]
  (first (apply min-key second (map-indexed vector population-scores))))

(defn find-worst-index [population-scores]
  (first (apply max-key second (map-indexed vector population-scores))))

(defn mate [parent1 parent2]
  (let [crossover (rand-int POLYGON-COUNT)]
    (loop [n 0 result []]
      (if (< n POLYGON-COUNT)
        (recur (inc n) (conj result (nth (if (< n crossover) parent1 parent2) n)))
        result))))


(defn replace-worst [population]
  (let [scores (evaluate-candidates population)
        index-of-best (find-best-index scores)
        index-of-worst (find-worst-index scores)
        random-individual (population (rand-int POPULATION-COUNT))
        best-individual (population index-of-best)
        new-individual (mate best-individual random-individual)]
    (assoc population index-of-worst new-individual)))

(defn run []
  (loop [n 0 population (create-random-population)]
    (when (< n MAX-ITERATIONS)
      (println (str "iteration: " n))
      (recur (inc n) (replace-worst population))))
  (println "done"))

(defn doit []
  (init-images)
  (show-image)
  (draw-polygons (random-polygons)))

