(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def test-data-raw
  ["498,4 -> 498,6 -> 496,6"
   "503,4 -> 502,4 -> 502,9 -> 494,9"])

(def regolith-source '(500 0))

(def real-data-raw (str/split-lines (slurp "day-fourteen.txt")))

(defn parse-line [line]
  (map (fn [point] (map #(Integer/parseInt %) (str/split point #","))) (str/split line #" -> ")))

(defn parse-data [data]
  (map parse-line data))

(defn min-max-x-y [point-data]
  (let [[x-values y-values] (reduce (fn [reduction point]
                    (let [[x-values y-values] reduction]
                      [(conj x-values (first point)) (conj y-values (second point))]
                      )) [[] []] point-data)]
    [(apply min x-values)
      (apply max x-values)
      (apply min y-values)
      (apply max y-values)]))


(defn all-point-data [data]
  (reduce (fn [combined line] (apply conj combined line)) [] data))

(defn create-cave-map [data]
  (let [[min-x-value max-x-value min-y-value max-y-value]
        (min-max-x-y (conj (all-point-data data) regolith-source))
        columns (inc (- max-x-value min-x-value))
        rows    (inc (- max-y-value min-y-value))]
    [rows columns (apply vector (char-array (* (inc columns) (inc rows)) \.)) min-x-value]))


(defn out-of-range [cave-map row column]
  (let [[rows columns cave-data min-x-value] cave-map]
    (or (>= row rows)
        (< column 0)
        (>= column columns)
        )))

(defn update-cave-map-location [cave-map row column new-value]
  (let [[rows columns cave-data min-x-value] cave-map
        index (+ (* row columns) column)]
    (if (out-of-range cave-map row column) cave-map
        [rows columns (update cave-data index (fn [old-value] new-value)) min-x-value]
        )
    )
  )

(defn cave-map-location [cave-map row column]
  (let [[rows columns cave-data min-x-value] cave-map
        index (+ (* row columns) column)]
    (if (out-of-range cave-map row column)
      \.
      (cave-data index)
      )
    ))

(defn line-segment-for-points [point-one point-two]
  (let [[point-one-x point-one-y] point-one
        [point-two-x point-two-y] point-two
        start-x (min point-one-x point-two-x)
        start-y (min point-one-y point-two-y)
        end-x (max point-one-x point-two-x)
        end-y (max point-one-y point-two-y)]
    (assert (or (= start-x end-x) (= start-y end-y)))
    (if (= start-x end-x)
      (map #(seq [start-x %]) (range start-y (inc end-y)))
      (map #(seq [% start-y]) (range start-x (inc end-x)))
      )
    )
  )

(defn point-to-row-column [cave-map point]
  (let [[rows columns cave-data min-x-value] cave-map
        [x y] point
        column (- x min-x-value)
        ;; we have to subtract the min y value from the  y value to get a row index
        row y]
    [row column]))

(defn render-cave-points-reducer [cave-map point]
  (let [[row column] (point-to-row-column cave-map point)]
    (update-cave-map-location cave-map row column \#)))

(defn render-cave-line-segments-reducer [cave-map line-data]
  (loop [current-cave-map cave-map
         points (map (fn points [[point-one point-two]] (line-segment-for-points point-one point-two)) (partition 2 1 line-data))]
    (if (empty? points)
      current-cave-map
      (let [current-points (first points)
            new-cave-map (reduce render-cave-points-reducer current-cave-map current-points)]
        (recur new-cave-map (rest points)))
      )
    )
  )

;; Draw all the lines described in the data
(defn render-cave-map [data]
  (let [cave-map (create-cave-map data)]
    (reduce render-cave-line-segments-reducer cave-map data)
    )
  )

(defn compare-cave-maps [first-cave-map second-cave-map]
  (let [first-data (nth first-cave-map 2)
        second-data (nth second-cave-map 2)]
    (compare first-data second-data)))

;; Print out a cave map
(defn draw-cave-map [cave-map]
  (let [[rows columns cave-data min-x-value] cave-map]
    (doseq [y (range 0 (inc rows))]
      (let [start-index (* y columns)
            end-index (+ (* y columns) columns)]
        (println (format "%05d -" y) (apply str (subvec cave-data start-index end-index))))
      )
    )
  )

(defn free-locations [cave-map row column]
  (let [[rows columns cave-data min-x-value] cave-map
        target-row (inc row)]
    ;; return available spots in order
    (filter #(= (cave-map-location cave-map (first %) (second %)) \.) [
                                                                       ;; directly below first
                                                                       [target-row column]
                                                                       ;; to the left next
                                                                       [target-row (dec column)]
                                                                       ;; the n to the right
                                                                       [target-row (inc column)]])
    )
  )

(defn simulate-unit [cave-map regolith-source-point]
  (let [regolith-source-location (point-to-row-column cave-map regolith-source-point)]
    (loop [current-cave-map cave-map
           current-location regolith-source-location]
      (let [current-row (first current-location)
            current-column (second current-location)
            new-locations (free-locations current-cave-map current-row current-column)
            new-location (first new-locations)
            rows (first cave-map)]
        (cond
          ;; check if we didn't get new ocations and the old location is the source point
          (and (nil? new-location) (= regolith-source-location current-location)) [(update-cave-map-location current-cave-map current-row current-column \o) true]
          ;; Otherwise, check if we can't move this grain anymore
          (nil? new-location) [current-cave-map false]
          ;; if the new location is past the last row
          (> (first new-location) rows) [current-cave-map true]
          ;; keep going
          :else
          (let [new-row (first new-location)
                new-column (second new-location)
                new-cave-map (-> (update-cave-map-location current-cave-map current-row current-column \.)
                                 (update-cave-map-location new-row new-column \o)
                                 )]
            (recur new-cave-map new-location))
          )
        )
      ))
  )

(defn simulate-units [cave-map regolith-source-point]
  (loop [current-cave-map cave-map]
    (let [[new-cave-map finished] (simulate-unit current-cave-map regolith-source-point)]
      (if finished
        new-cave-map
        (recur new-cave-map)
      )
    )
  ))

(defn count-sand [cave-map]
  (count (filter #(= % \o) (nth cave-map 2))))

(defn create-floor [data]
  (let [[min-x-value max-x-value min-y-value max-y-value] (min-max-x-y (all-point-data data))
        floor-y (+ max-y-value 2)
        ;; guessing how much floor we need to catch all the sand for part two
        x-buffer (* 2 (- max-y-value min-y-value))]
    (conj data [[(- min-x-value x-buffer) floor-y] [(+ max-x-value x-buffer) floor-y]]))
  )
