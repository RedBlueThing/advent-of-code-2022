(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round]])

(def test-data-raw [
                    "#.#####"
                    "#.....#"
                    "#>....#"
                    "#.....#"
                    "#...v.#"
                    "#.....#"
                    "#####.#"
                    ])

(def more-complex-test-data-raw ["#.######"
                                 "#>>.<^<#"
                                 "#.<..<<#"
                                 "#>v.><>#"
                                 "#<^v^^>#"
                                 "######.#" ])

(def real-data-raw (str/split-lines (slurp "day-twenty-four.txt")))

(defn map-for-data [data]
  (let [height (count data)
        width (count (first data))
        buffer (apply vector (apply concat (map (fn [line] (char-array line)) data)))]
    {:width width :height height :buffer buffer}))

(defn index-for-row-and-column [map row column]
  (let [{width :width} map]
    (+ (* row width) column)))

(defn row-and-column-for-index [map index]
  (let [{width :width height :height} map]
    (if (or (< index 0) (>= index (* width height)))
      nil ; return nil if index is out of bounds
      [(quot index width) (mod index width)])))

(def directions [\> \< \^ \v])

(defn blizzard-set-for-map [map]
  ;; Find all the blizards. Not this only works for when the blizzards don't
  ;; overlap (ie before the simulation).
  (let [{width :width height :height buffer :buffer} map]
    (assoc map :blizzards (into {} (filter #(not= nil %) (map-indexed (fn [i ch]
                                                                        (when (some #(= % ch) directions)
                                                                          [(row-and-column-for-index map i) [ ch ]])) buffer))))))

(defn blizzards-for-updated-location [blizzards new-location direction]
  (let [vector-for-location (or (blizzards new-location) [])]
    (assoc blizzards new-location (conj vector-for-location direction))))

(defn new-location-in-direction [[row column] direction]
  (case direction
    \^ [(dec row) column]
    \> [row (inc column)]
    \v [(inc row) column]
    \< [row (dec column)]))

(defn wrap-row-and-column [map [row column]]
  (let [{width :width height :height} map]
    (cond
      (= row (dec height)) [1 column]
      (= row 0) [(dec height) column]
      (= column (dec column) [row 1])
      (= column 0 [row (dec column)])
      :else [row column])))

(defn simulate-round [map]
  (let [{width :width height :height blizzards :blizzards} map]
    ;; Iterate through our locations and create a new blizzard map after moving
    ;; everything in the appropriate direction
    (assoc map :blizzards (reduce (fn move-blizzard-reducer [new-blizzards [next-location directions]]
               (reduce (fn process-direction-reducer [blizzards-for-direction direction]
                         (let [new-location (wrap-row-and-column map (new-location-in-direction next-location direction))]
                           (blizzards-for-updated-location blizzards-for-direction new-location direction))
                         ) new-blizzards directions)
                                    ) {} (seq blizzards)))))

(def starting-location [0 1])

(defn neighbours [map location]
  (map (fn [direction] (new-location-in-direction location direction)) [ \^ \> \v \< ]))

(defn filter-locations-for-map [map locations]
  (let [{width :width height :height} map]
    (filter (fn [location] ) locations)

    )
  )

(defn available-moves [map [row column]]
  (let [{blizzards :blizzards} map

        ]
    )
  )
