(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round]])

(def test-data-raw [
                    "        ...#"
                    "        .#.."
                    "        #..."
                    "        ...."
                    "...#.......#"
                    "........#..."
                    "..#....#...."
                    "..........#."
                    "        ...#...."
                    "        .....#.."
                    "        .#......"
                    "        ......#."
                    ""
                    "10R5L5R10L4R5L5"
                    ])

(def real-data-raw (str/split-lines (slurp "day-twenty-two.txt")))

(defn board-for-data [data]
  (let [board-data (drop-last 2 data)
        max-line-length (apply max (map count board-data))
        columns max-line-length
        rows (count board-data)
        ;; to make our data array we concatinate all the lines (after we have
        ;; turned them in to char-arrays and added spaces on the end for the
        ;; ones that are shorter than max-line-length
        board-map (apply concat (map (fn [line]
                                  (concat
                                   (char-array line)
                                   (take (- max-line-length (count line)) (repeat \space)))) board-data))
        ]
    {:columns columns
     :rows rows
     :data board-map
     :instructions (map str/join (partition-by #(or (= % \R) (= % \L)) (last data)))}))

(defn index-for-row-and-column [board row column]
  (let [{rows :rows columns :columns} board]
    (+ (* row columns) column)))

(defn row-or-column-out-of-range? [row-or-column rows-or-columns]
  (or (< row-or-column 0) (>= row-or-column rows-or-columns)))

(defn value-at-row-and-column [board row column]
  (let [{rows :rows columns :columns} board]
    (if (or
         (row-or-column-out-of-range? row rows)
         (row-or-column-out-of-range? column columns))
      ;; if anything is out of range, just return \space
      \space
      ;; otherwise grab the value at the index
      (let [{data :data} board]
        (nth data (index-for-row-and-column board row column))))))

(defn turn-right [direction]
  (case direction
    ">" "v"
    "<" "^"
    "^" ">"
    "v" "<"))

(defn reverse-direction [direction]
  (-> direction turn-right turn-right))

(defn direction-for-turn [direction turn]
  (case turn
    "R" (turn-right direction)
    "L" (turn-right (reverse-direction direction))))

(defn horizontal-direction [direction]
  (or (= direction "<") (= direction ">")))

(defn vertical-direction [direction]
  (or (= direction "^") (= direction "v")))

(defn indexes-starting-at-row-column-in-direction [row column direction])

(defn row-start-for-direction [board row direction]
  (let [{rows :rows} board]
   (if (vertical-direction direction)
     (case direction
       "^" 0
       "v" (dec rows))
     row)))

(defn column-start-for-direction [board column direction]
  (let [{columns :columns} board]
    (if (horizontal-direction direction)
      (case direction
        "<" 0
        ">" (dec columns))
      column)))

(defn column-fn-for-direction [direction]
  (case direction
    ">" inc
    "<" dec
    identity))

(defn row-fn-for-direction [direction]
  (case direction
    "^" dec
    "v" inc
    identity))

(defn wrap-row-and-column-part-one [board row column direction]
  ;; wrapping out of range steps according to the part one rules
  (let [wrap-direction (reverse-direction direction)
        {rows :rows columns :columns data :data} board
        ;; work out the appropriate end of the row based on the direction
        ;; opposite to the one we are travelling in (wrap-direction)
        scan-start-row (row-start-for-direction board row wrap-direction)
        scan-start-column (column-start-for-direction board column wrap-direction)
        ;; Let's find out which indexes (column or row) we are going to
        ;; increment/decrement based on the direction we are moving
        fn-column (column-fn-for-direction direction)
        fn-row (row-fn-for-direction direction)]
    ;; then we just need to check values along the direction we are
    ;; travelling until we find a non \space value
    (loop [current-row scan-start-row
           current-column scan-start-column]
      ;; N.B. This explodes if we go out of range, but we shouldn't be cause we
      ;; are moving back towards the cell we started from (row column)
      (let [value (value-at-row-and-column board current-row current-column)]
        (if (not= value \space)
          ;; in part one wrapping around the ends of rows doesn't change the
          ;; direction, but part two will so we add this to the return from this
          ;; function
          [current-row current-column direction]
          (recur (fn-row current-row) (fn-column current-column)))
        )
      )
    )
  )

(defn offset-source-row-and-column-for-direction [[source destination] direction]
  ;; takes a vector source -> destination (which is eventually in our edge map)
  ;; and offsets the row and column in the source based on the direction. See
  ;; mappings-for-edge for an explanation of that.
  [(let [[row column] source]
     (case direction
       ">" [row (dec column)]
       "<" [row (inc column)]
       "^" [(inc row) column]
       "v" [(dec row) column])) destination])

(defn mappings-for-edge [from-row-start
                         from-column-start
                         to-row-start
                         to-column-start
                         ;; offsets
                         from-row-offset
                         from-column-offset
                         to-row-offset
                         to-column-offset
                         ;; directions for from and to
                         from-direction
                         to-direction
                         edge-width]

  ;; My first take on this function returned the mappings from the rows of cells
  ;; along the edge of the cube, but the wrap-row-and-column interface is called
  ;; with the out of bounds cell (so one past the edge in the oppposite of
  ;; direction (to-direction or from-direction depending on which way you are
  ;; going).
  ;;
  ;; So I am going to leave my from-to-mappings and to-from-mappings variables
  ;; and then offset the key (left hand vector) for each row in each of those
  ;; two variables based on the to-direction and from-direction.
  ;;
  ;; "From" and "to" is a bit confusing in this code (because it implies the
  ;; mapping itself), because the mappings are reciprocal (from -> to and also
  ;; to -> from) so "from" was really just the first edge I looked at on my
  ;; little cube mockup

  (let [from-to-mappings (map (fn [index]
                                [[(+ from-row-start (* from-row-offset index))
                                  (+ from-column-start (* from-column-offset index))]
                                 [(+ to-row-start (* to-row-offset index))
                                  (+ to-column-start (* to-column-offset index))
                                  to-direction
                                  ]]) (range 0 edge-width))
        to-from-mappings (map (fn [[from [to-row to-column]]] [[to-row to-column] (conj from from-direction)]) from-to-mappings)
        offset-from-to-mappings (map (fn [mapping] (offset-source-row-and-column-for-direction mapping from-direction)) from-to-mappings)
        offset-to-from-mappings (map (fn [mapping] (offset-source-row-and-column-for-direction mapping to-direction)) to-from-mappings)]
    (apply vector (concat offset-from-to-mappings offset-to-from-mappings)))
  )

(defn cube-edge-maps-real []
  ;; From my mockup of the cube I labeled the 14 outside edges as pairs marked
  ;; ABCDXYZ
  (into {} (concat
            ;; [[[from-row from-col] [to-row to-col to-direction]] ... ]

            ;; EDGE A -
            (mappings-for-edge 0 50 149 0 1 0 -1 0 ">" ">" 50)
            ;; EDGE B -
            (mappings-for-edge 0 50 150 0 0 1 1 0 "v" ">" 50)
            ;; EDGE C -
            (mappings-for-edge 0 149 199 49 0 -1 0 -1 "v" "^" 50)
            ;; EDGE D -
            (mappings-for-edge 149 99 199 49 0 -1 -1 0 "^" "<" 50)
            ;; EDGE X -
            (mappings-for-edge 49 100 50 99 0 1 1 0 "^" "<" 50)
            ;; EDGE Y -
            (mappings-for-edge 0 149 149 99 1 0 -1 0 "<" "<" 50)
            ;; EDGE Z -
            (mappings-for-edge 100 49 99 50 0 -1 -1 0 "v" ">" 50)

            ;; hack to map the zeroth position into the start position (which we
            ;; used in part one to programmatically scan to the first cell
            [[[0 0] [0 50 ">"]]])))


(defn cube-edge-maps-test []
  ;; same for the test cube which has a different topology
  (into {} (concat

            ;; EDGE A
            (mappings-for-edge 0 8 4 3 0 1 0 -1 "v" "v" 4)
            ;; EDGE B
            (mappings-for-edge 0 8 4 4 1 0 0 1 ">" "v" 4)
            ;; EDGE C
            (mappings-for-edge 4 0 11 15 1 0 0 -1 ">" "^" 4)
            ;; EDGE D
            (mappings-for-edge 0 11 11 15 1 0 -1 0 "<" "<" 4)
            ;; EDGE E
            (mappings-for-edge 4 11 8 15 1 0 0 -1 "<" "v" 4)
            ;; EDGE F
            (mappings-for-edge 7 3 11 8 0 -1 0 1 "^" "^" 4)
            ;; EDGE G
            (mappings-for-edge 7 4 11 8 0 1 -1 0 "^" ">" 4)

            [[[0 0] [0 8 ">"]]])))

(def cube-edge-maps (cube-edge-maps-real))

(def debug false)
(defn wrap-row-and-column-part-two [board row column direction]
  ;; wrap out of bands steps according to the cube mapping rules
  (let [wrapped-row-column-and-direction (cube-edge-maps [row column])]
    ;; We can assert this bewcause we manually figured out the mappings along
    ;; the edge of the cube shape (cube-edge-maps)
    (assert wrapped-row-column-and-direction (format "Row and column was %d %d" row column))
    wrapped-row-column-and-direction))

;; let's just make this a global we switch for part two (rather than passing it
;; down or something)
(def wrap-row-and-column wrap-row-and-column-part-two)

(defn maybe-wrap-row-and-column-for-value [board row column direction]
  (let [{rows :rows columns :columns data :data} board
        value (value-at-row-and-column board row column)]
    (if (= value \space)
      ;; if the next spot is blank or it's out of range entirely, then we need
      ;; to wrap around the game board and return the wrapped row and column
      (wrap-row-and-column board row column direction)
      ;; if it doesn't wrap around we are all good
      [row column direction])))

(defn next-step-row-and-column [board row column direction]
  (let [inc-row ((row-fn-for-direction direction) row)
        inc-column ((column-fn-for-direction direction) column)]
    (maybe-wrap-row-and-column-for-value board inc-row inc-column direction)))

(defn next-step [board row column direction]
  (let [{rows :rows columns :columns data :data} board
        [next-row next-column next-direction] (next-step-row-and-column board row column direction)
        value (value-at-row-and-column board next-row next-column)]
    [value next-row next-column next-direction]
    ))

(defn step-reducer [board instruction]
  (let [{direction :current-direction} board]
    (cond
      (.contains ["L", "R"] instruction) (assoc board :current-direction (direction-for-turn direction instruction))
      :else (loop [current-board board
                   remaining-steps (Integer/parseInt instruction)]
              (let [{current-row :current-row current-column :current-column current-direction :current-direction} current-board
                    [next-value next-row next-column next-direction] (next-step current-board current-row current-column current-direction)]
                (if debug
                  (println (format "Step row(%d) column(%d) steps(%d) value(%c) direction(%s)" current-row current-column remaining-steps next-value (current-board :current-direction))))
                (if (or (= next-value \#)
                        (= remaining-steps 0))
                  current-board
                  (recur (assoc current-board :current-row next-row :current-column next-column :current-direction next-direction) (dec remaining-steps))))))))

(defn starting-board-state [board]
  ;; You begin the path in the leftmost open tile of the top row of tiles.
  ;; Initially, you are facing to the right (from the perspective of how the map
  ;; is drawn).

  (let [[starting-row starting-column] (wrap-row-and-column board 0 0 ">")]
    (assoc board
           :current-row starting-row
           :current-column starting-column
           :current-direction ">")))

(defn execute-steps [board]
  (let [{rows :rows columns :columns data :data instructions :instructions} board]
    (reduce step-reducer (starting-board-state board) instructions)))

(defn facing-for-direction [direction]
  (case direction
    ">" 0
    "v" 1
    "<" 2
    "^" 3))

(defn solution [board]
  ;; just calculate the solution given a board
  (let [{current-row :current-row current-column :current-column current-direction :current-direction} board
        ;; going from indexes to numbers (1-n) ... like an animal
        row-number (inc current-row)
        column-number (inc current-column)]
    (+ (* row-number 1000) (* column-number 4) (facing-for-direction current-direction))))

(-> (board-for-data real-data-raw)
    starting-board-state
    execute-steps
    solution)
