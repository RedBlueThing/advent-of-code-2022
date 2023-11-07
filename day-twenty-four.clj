(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round]])

(def test-data-raw [
                    "#.#####"
                    "#<...>#"
                    "#.....#"
                    "#.....#"
                    "#.....#"
                    "#^...v#"
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

(defn index-for-row-and-column [blizzard-map row column]
  (let [{width :width} blizzard-map]
    (+ (* row width) column)))

(defn row-and-column-for-index [blizzard-map index]
  (let [{width :width height :height} blizzard-map]
    (if (or (< index 0) (>= index (* width height)))
      nil ; return nil if index is out of bounds
      [(quot index width) (mod index width)])))

(def directions [\> \< \^ \v])

(defn add-blizzard-set-to-map [blizzard-map]
  ;; Find all the blizards. Not this only works for when the blizzards don't
  ;; overlap (ie before the simulation).
  (let [{width :width height :height buffer :buffer} blizzard-map]
    (assoc blizzard-map :blizzards (into {} (filter #(not= nil %) (map-indexed (fn [i ch]
                                                                        (when (some #(= % ch) directions)
                                                                          [(row-and-column-for-index blizzard-map i) [ ch ]])) buffer))))))

(defn blizzards-for-updated-location [blizzards new-location direction]
  (let [vector-for-location (or (blizzards new-location) [])]
    (assoc blizzards new-location (conj vector-for-location direction))))

(defn new-location-in-direction [[row column] direction]
  (case direction
    \^ [(dec row) column]
    \> [row (inc column)]
    \v [(inc row) column]
    \< [row (dec column)]))

(defn wrap-row-and-column [blizzard-map [row column]]
  (let [{width :width height :height} blizzard-map]
    (cond
      (= row (dec height)) [1 column]
      (= row 0) [(- height 2) column]
      (= column (dec width)) [row 1]
      (= column 0) [row (- width 2)]
      :else [row column])))

(defn simulate-round [blizzard-map]
  (let [{width :width height :height blizzards :blizzards} blizzard-map]
    ;; Iterate through our locations and create a new blizzard map after moving
    ;; everything in the appropriate direction
    (assoc blizzard-map :blizzards (reduce (fn move-blizzard-reducer [new-blizzards [next-location directions]]
               (reduce (fn process-direction-reducer [blizzards-for-direction direction]
                         (let [new-location (wrap-row-and-column blizzard-map (new-location-in-direction next-location direction))]
                           (blizzards-for-updated-location blizzards-for-direction new-location direction))
                         ) new-blizzards directions)
                                    ) {} (seq blizzards)))))

(defn start-location [] [0 1])

(defn end-location [blizzard-map]
  (let [{width :width height :height} blizzard-map]
    [(dec height) (- width 2)]))

(defn neighbours [location]
  (map (fn [direction] (new-location-in-direction location direction)) [ \^ \> \v \< ]))

(defn filter-locations-for-map [blizzard-map locations]
  (let [{width :width height :height buffer :buffer blizzards :blizzards} blizzard-map
        blizzard-set (set (keys blizzards))]
    (filter (fn [[row column]]
              (and
               ;; filter locations that are in range
               (and (>= row 0) (< row height) (>= column 0) (< column width))
               ;; and that aren't walls
               (not= \# (nth buffer (index-for-row-and-column blizzard-map row column)))
               ;; and not in a blizzard
               (not (contains? blizzard-set [row column])))) locations)))

(defn available-moves [blizzard-map location]
  (let [neighbours-for-location (neighbours location)
        ;; find viable locations including the current location
        viable-locations (filter-locations-for-map blizzard-map (conj neighbours-for-location location))]
    viable-locations))

(defn update-visited [visited blizzard-map minute]
  (reduce (fn [new-visited [visited-location previous-minute]]
            (let [moves (available-moves blizzard-map visited-location)]
              (merge new-visited (into {} (map (fn [move-location] [move-location minute]) moves))))) {} (seq visited)))

(defn render-with-vector [blizzard-map overlay]
  (println overlay)
  (let [{buffer :buffer width :width height :height} blizzard-map
        underlying-buffer (apply vector (map (fn [ch] (if (= ch \#) \# \.)) buffer))]
    (doseq [row (range 0 height)]
      (let [sub-array (subvec underlying-buffer
                              (index-for-row-and-column blizzard-map row 0)
                              (index-for-row-and-column blizzard-map row width))
            updated-sub-array (map-indexed (fn [index item]
                                             (let [current [row index]]
                                               (or (let [mapped (overlay current)]
                                                     (cond
                                                       (integer? mapped) (mod mapped 10)
                                                       (vector? mapped) (if (> (count mapped) 1)
                                                                          (mod (count mapped) 10)
                                                                          (char (first mapped)))
                                                       :else nil)
                                                     ) item)
                                               )) sub-array)]
        (println (str (apply str updated-sub-array)))))))

(defn part-one [data limit]
  (let [blizzard-map (add-blizzard-set-to-map (map-for-data data))
        start (start-location)
        end (end-location blizzard-map)]
    (loop [minute 0
           visited {start minute}
           current-blizzard-map blizzard-map]
      (let [steps-at-end-location (visited end)]
        (if (or (>= minute limit)
                (not (nil? steps-at-end-location)))
          (do (println (format "Escape valve %d - steps %d" minute steps-at-end-location))
              [visited current-blizzard-map])
          (let [new-blizzard-map (simulate-round current-blizzard-map)
                new-visited (update-visited visited new-blizzard-map minute)]
            (recur (inc minute) new-visited new-blizzard-map)))))))


(defn part-two [data]
  (let [blizzard-map (add-blizzard-set-to-map (map-for-data data))
        start (start-location)
        end (end-location blizzard-map)]
    (reduce (fn origin-destination-reducer [reducer-blizzard-map [origin destination]]
              (loop [minute 0
                     visited {origin minute}
                     current-blizzard-map reducer-blizzard-map]
                (let [steps-at-destination-location (visited destination)]
                  (if (not (nil? steps-at-destination-location))
                    (do (println (format "Steps %d" (inc steps-at-destination-location)))
                        current-blizzard-map)
                    (let [new-blizzard-map (simulate-round current-blizzard-map)
                          new-visited (update-visited visited new-blizzard-map minute)]
                      (recur (inc minute) new-visited new-blizzard-map)))))) blizzard-map [[start end] [end start] [start end]])))



;; rendering
;;
;;

(defn render [data limit]
  (let [blizzard-map (add-blizzard-set-to-map (map-for-data data)) [visited new-blizzard-map] (part-one data limit)]
    ;; (render-with-vector blizzard-map (:blizzards new-blizzard-map))
    (render-with-vector blizzard-map visited)
    visited))
