(ns monalisa.core
  "core monalisa code"
  (:use [monalisa.graphics]))

(def MAX-GENERATIONS 100000)
(def POLYGON-COUNT 250)    ; # of polygons for a given image
(def POINTS-PER-POLYGON 6)
(def POPULATION-COUNT 10)
(def MUTATION-RATE 0.01)
(def POINT-MUTATION-MAX-DISTANCE 50)
(def COLOR-MUTATION-MAX-DISTANCE 5)

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

(defn random-int-between [start-num end-num]
  (+ start-num (rand-int (inc (- end-num start-num)))))

(defn random-polygon [image-width image-height]
  (let [point-count (random-int-between 3 POINTS-PER-POLYGON)]
    {
      :points (vec (repeatedly point-count #(random-point image-width image-height)))
      :color (random-color)
      }))

(defn random-polygons
  ([] (random-polygons (.getWidth buffered-image) (.getHeight buffered-image)))
  ([image-width image-height] (vec (into [] (repeatedly POLYGON-COUNT #(random-polygon image-width image-height))))))

(defn evaluate-candidate [polygons]
  (let [scratch-image (new-buffered-image)]
    (draw-polygons polygons scratch-image)
    (compare-image-to-reference scratch-image)))

(defn new-individual
  ([] (new-individual (random-polygons)))
  ([polygons] {
      :polygons polygons
      :score (evaluate-candidate polygons)
      }))

(defn create-random-population []
  (vec (into [] (repeatedly POPULATION-COUNT #(new-individual)))))


; find index of best scoring candidate in the vector of (pre-computed) scores
(defn find-best-index [scores]
  (first (apply min-key second (map-indexed vector scores))))

(defn find-worst-index [scores]
  (first (apply max-key second (map-indexed vector scores))))

(defn find-best-individual [population]
  (let [scores (map #(:score %) population)
        index-of-best (find-best-index scores)
        best-individual (population index-of-best)]
    best-individual))

(defn image-width []
  (.getWidth buffered-image))

(defn image-height []
  (.getHeight buffered-image))

(defn random-direction []
  (if (= 0 (rand-int 2)) 1 -1)) ; 50% chance negative

(defn move-by-random-delta [initial-val max-delta max-val]
  (let [amount-to-move (+ 1 (rand-int max-delta))]
    (mod (+ initial-val (* amount-to-move (random-direction))) max-val)))

(defn nearby-point [point]
;  (println "nearby-point")
  {
    :x (move-by-random-delta (:x point) POINT-MUTATION-MAX-DISTANCE (image-width))
    :y (move-by-random-delta (:y point) POINT-MUTATION-MAX-DISTANCE (image-height))
  })

(defn nearby-color [color]
;  (println "nearby-color")
  {
    :r (move-by-random-delta (:r color) COLOR-MUTATION-MAX-DISTANCE 256)
    :g (move-by-random-delta (:g color) COLOR-MUTATION-MAX-DISTANCE 256)
    :b (move-by-random-delta (:b color) COLOR-MUTATION-MAX-DISTANCE 256)
    :a (move-by-random-delta (:a color) COLOR-MUTATION-MAX-DISTANCE 256)
  })

(defn nearby-alpha [color]
;  (println "nearby-alpha")
  {
    :r (:r color)
    :g (:g color)
    :b (:b color)
    :a (move-by-random-delta (:a color) COLOR-MUTATION-MAX-DISTANCE 256)
  })

(defn mutate-point [points index]
  (vec (assoc points index (nearby-point (points index)))))

(defn mutate-polygon [polygon]

  (let [points (:points polygon)
        point-count (count points)
        locus (rand-int (+ 2 point-count))
        points (if (< locus point-count) (mutate-point points locus) points)
        color (:color polygon)
        color (if (= locus point-count) (nearby-color color) color)
        color (if (= locus (+ 1 point-count)) (nearby-alpha color) color)
        ]

  ; TODO: change only ONE of the following:
  ; 1. a single point
  ; 2. polygon color
  ; 3. polygon alpha
  {
    :points points
    :color color
  }))


(defn possibly-mutate-polygon [polygon]
  (if (< (rand) MUTATION-RATE)
    (do
;      (println "mutating!")
      (random-polygon (.getWidth buffered-image) (.getHeight buffered-image)))
    polygon))


(defn random-mutation-indices [count]
  [
    (rand-int (+ count 1))
    (rand-int (+ count 1))
    (rand-int (+ count 1))
  ])

(defn mutate-random-polygon [polygons]
  (let [mutation-index (rand-int (count polygons))
        mutated-result (assoc polygons mutation-index (mutate-polygon (polygons mutation-index)))]
    (vec mutated-result)))

; mutate 3 random polygons in the given individual
(defn mutate-individual [individual]

  (let [polygons (:polygons individual)
        polygons (mutate-random-polygon polygons)
        polygons (mutate-random-polygon polygons)
        polygons (mutate-random-polygon polygons)]
      {
        :polygons polygons
        :score (evaluate-candidate polygons)
      }))


(defn new-population-from-progenitor [progenitor]
  (vec (into [] (repeatedly POPULATION-COUNT #(mutate-individual progenitor)))))

; create a new population based on the best individual
(defn new-population-from-mutated-best [population best-individual]
  (vec (map (fn [_] (mutate-individual best-individual)) population)))

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
;    (println (str "ousting: " index-of-worst))
    (assoc population index-of-worst new-individual)))

(defn run []
  (loop [n 0 population (create-random-population)]
    (when (< n MAX-GENERATIONS)
      (if (= 0 (mod n 100)) (println (str "generation: " n)))
      (recur (inc n) (replace-worst population))))
  (println "done"))

(defn run2 []
  (loop [n 0 population (create-random-population)]
    (when (< n MAX-GENERATIONS)
;      (if (= 0 (mod n 100)) (println (str "generation: " n)))
      (println (str "generation: " n))
      (recur (inc n) (new-population-from-mutated-best population (find-best-individual population)))))
  (println "done"))


(defn temperature [generation]
  (- MAX-GENERATIONS generation))

(defn update-display [best-candidate]
  (draw-polygons (:polygons best-candidate) buffered-image)
  (.repaint the-panel))

(defn run3 []
  (loop [generation 0 progenitor (new-individual)]
    (when (< generation MAX-GENERATIONS)

      (let [population (new-population-from-progenitor progenitor)
            best-of-population (find-best-individual population)
            diff (- (:score progenitor) (:score best-of-population))
            temp (temperature generation)
            p  (Math/exp (/ diff temp))
            chance (rand)
            new-progenitor (if
                             (or (< (:score best-of-population) (:score progenitor))
                                 (< chance p))
                             best-of-population
                             progenitor)
            ]
        (println (str "generation: " generation ", temperature: " temp ", best score: " (:score best-of-population) ", chance: " chance ", p: " p ", chance < p: " (< chance p)))
        (update-display best-of-population)
        (recur (inc generation) new-progenitor))))

  (println "done"))



(defn doit []
  (init-images)
  (show-image)
  (draw-polygons (random-polygons)))


(defn -main []
  (init-images)
  (show-image)
;  (draw-polygons (random-polygons))
  (run3)
  )
