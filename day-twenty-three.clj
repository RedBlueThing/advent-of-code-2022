(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round]])

(def test-data-raw [
                    "....#.."
                    "..###.#"
                    "#...#.#"
                    ".#...##"
                    "#.###.."
                    "##.#.##"
                    ".#..#.."
                    ])

(def small-test-data-raw [ "....."
  "..##."
  "..#.."
  "....."
  "..##."
  "....."])

(def real-data-raw (str/split-lines (slurp "day-twenty-three.txt")))

(defn elf-coordinates-for-data [data]
  (let [raw-elf-coordinates (map (fn [line] (char-array line)) data)
        elf-coordinates (map-indexed
                          (fn [line-index line-char-array]
                            (map-indexed (fn [char-index c]
                                           (when (= c \#) [line-index char-index])) line-char-array))
                        raw-elf-coordinates)]
    (set (remove nil? (apply concat elf-coordinates)))))

(defn neighbours-in-direction-set [elf direction]
  (let [[row column] elf]
    (case direction
      "N" #{ [(dec row) (dec column)] [(dec row) column] [(dec row) (inc column)] }
      "E" #{ [(dec row) (inc column)] [row (inc column)] [(inc row) (inc column)] }
      "S" #{ [(inc row) (dec column)] [(inc row) column] [(inc row) (inc column)] }
      "W" #{ [(dec row) (dec column)] [row (dec column)] [(inc row) (dec column)] }
      )))

(defn neighbours-set [[row column]]
  #{[(dec row) (dec column)] [(dec row) column] [(dec row) (inc column)]
    [row (dec column)] [row (inc column)]
    [(inc row) (dec column)] [(inc row) column] [(inc row) (inc column)]})

(defn coordinates-in-direction [[row column] direction]
  (case direction
    "N" [(dec row) column]
    "E" [row (inc column)]
    "S" [(inc row) column]
    "W" [row (dec column)]))

(defn direction-at-offset [index]
  (let [directions ["N" "S" "W" "E"]] (nth directions (mod index (count directions)))))

(def debug false)

(defn elf-proposal-reducer [proposals elf]
  ;; for each elf we queue their movement propsal
  (let [{proposal-map :proposal-map
         offset :offset
         current-coordinates :current-coordinates
         stationary-set :stationary-set} proposals
        directions (map direction-at-offset (range offset (+ offset 4)))
        all-neighbours (neighbours-set elf)]

    (if (empty? (set/intersection all-neighbours current-coordinates))
      ;; If there are no elves in neighbouring squares, we can add this one to the stationary-set
      (assoc proposals :stationary-set (conj stationary-set elf))
      ;; otherwise we check directions
      (loop [current-offset offset]
        (if (>= current-offset (+ offset 4))
          ;; we are done, we didn't find a viable move, so stick this elf in the
          ;; stationary set
          (assoc proposals :stationary-set (conj stationary-set elf))
          ;; more directions to check
          (let [direction (direction-at-offset current-offset)
                neighbours (neighbours-in-direction-set elf direction)
                neighbours-containing-elves (set/intersection neighbours current-coordinates)
                coordinates-in-direction (coordinates-in-direction elf direction)
                proposals-at-coordinates (or (proposal-map coordinates-in-direction) [])]
            (if debug
              (println (format "%s is checking %s and found %s" (str elf) direction (str neighbours-containing-elves))))
            (if (empty? neighbours-containing-elves)
              ;; we can move in that direction
              (assoc proposals :proposal-map (assoc proposal-map coordinates-in-direction (conj proposals-at-coordinates elf)))
              ;; keep checking directions
              (recur (inc current-offset)))))))))

(defn set-for-proposal-map [proposal-map]
  (set (apply concat (map (fn [destination] (let [elves-moving (proposal-map destination)]
                                          (if (= (count elves-moving) 1)
                                            ;; if it's just one elf moving, then the
                                            ;; destination gets added to the set
                                            [ destination ]
                                            ;; otherwise the moving elves get added to the
                                            ;; set (their old locations)
                                            elves-moving))) (keys proposal-map)))))

(defn simulate-round [round-index current-coordinates]
  (if debug
    (println (format " ------ %d -------" (inc round-index))))
  (let [proposals (reduce
                   elf-proposal-reducer
                   {:proposal-map {} :stationary-set #{} :offset round-index :current-coordinates current-coordinates} current-coordinates)
        {proposal-map :proposal-map stationary-set :stationary-set} proposals]
    ;; create a new set of coordinates based on the stationary set and the
    ;; proposal map
    (if (= (count current-coordinates) (count stationary-set))
      (println (format "Nothing moved after this round ---> %d" (inc round-index))))
    (set/union stationary-set (set-for-proposal-map proposal-map))))


(defn rows-and-columns-for-coordinates [coordinates]
  (let [{rows :rows columns :columns} (reduce (fn [split-coordinates [row column]] (let [{rows :rows columns :columns} split-coordinates]
                                                                                     {:rows (conj rows row)
                                                                                      :columns (conj columns column)}
                                                                                     )) {:rows [] :columns []} coordinates)]
    [rows columns]))

(defn simulate-rounds [rounds starting-coordinates]
  (loop [round-index 0
         current-coordinates starting-coordinates]
    (if (= round-index rounds)
      current-coordinates
      (recur (inc round-index) (simulate-round round-index current-coordinates)))))

(defn metrics-for-coordinates [coordinates]
  (let [[rows columns] (rows-and-columns-for-coordinates coordinates)
        max-rows (apply max rows)
        min-rows (apply min rows)
        max-columns (apply max columns)
        min-columns (apply min columns)
        width (inc (- max-columns min-columns))
        height (inc (- max-rows min-rows))
        ]
    {:width width :height height
     :min-rows min-rows :max-rows max-rows
     :min-columns min-columns :max-columns max-columns}
    )
  )

(defn index-for-row-and-column [coordinates row column offset]
  (let [{width :width height :height min-columns :min-columns min-rows :min-rows} (metrics-for-coordinates coordinates)
        maybe-offset-row (+ row (if offset (* -1 min-rows) 0))
        maybe-offset-column (+ column (if offset (* -1 min-columns) 0))
        ]
    (+ (* maybe-offset-row width) maybe-offset-column))
  )

(defn render-coordinates [coordinates]
  (let [{width :width height :height} (metrics-for-coordinates coordinates)
        buffer (apply vector (repeat (* height width) \.))
        buffer-with-elves (reduce (fn [buffer [row column]]
                                    (assoc buffer (index-for-row-and-column coordinates row column true) \#)) buffer coordinates)]
    (doseq [row (range 0 height)]
      (let [sub-array (subvec buffer-with-elves
                              (index-for-row-and-column coordinates row 0 false)
                              (inc (index-for-row-and-column coordinates row (dec width) false)))]
        (println (str (apply str sub-array)))))))

(defn solve-part-one [coordinates]
  (let [{width :width height :height} (metrics-for-coordinates coordinates)]
    (- (* width height) (count coordinates))))

(render-coordinates (simulate-rounds 1 (elf-coordinates-for-data small-test-data-raw)))
