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
    "v" "<")
  )

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

(defn wrap-row-and-column [board row column direction]
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
          [current-row current-column]
          (recur (fn-row current-row) (fn-column current-column)))
        )
      )
    )
  )

(defn maybe-wrap-row-and-column-for-value [board row column direction]
  (let [{rows :rows columns :columns data :data} board
        value (value-at-row-and-column board row column)]
    (if (= value \space)
      ;; if the next spot is blank or it's out of range entirely, then we need
      ;; to wrap around the game board and return the wrapped row and column
      (wrap-row-and-column board row column direction)
      ;; if it doesn't wrap around we are all good
      [row column])))

(defn next-step-row-and-column [board row column direction]
  (let [inc-row ((row-fn-for-direction direction) row)
        inc-column ((column-fn-for-direction direction) column)]
    (maybe-wrap-row-and-column-for-value board inc-row inc-column direction)))

(defn next-step [board row column direction]
  (let [{rows :rows columns :columns data :data} board
        [next-row next-column] (next-step-row-and-column board row column direction)
        value (value-at-row-and-column board next-row next-column)]
    [value next-row next-column]
    ))

(defn step-reducer [board instruction]
  (let [{direction :current-direction} board]
   (cond
     (.contains ["L", "R"] instruction) (assoc board :current-direction (direction-for-turn direction instruction))
     :else (loop [current-board board
                  remaining-steps (Integer/parseInt instruction)]
             (let [{current-row :current-row current-column :current-column} current-board
                   [next-value next-row next-column] (next-step current-board current-row current-column direction)]
               (if (or (= next-value \#)
                       (= remaining-steps 0))
                 current-board
                 (recur (assoc board :current-row next-row :current-column next-column) (dec remaining-steps))))))))

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

(defn part-one-solution [board]
  ;; just calculate the solution given a board
  (let [{current-row :current-row current-column :current-column current-direction :current-direction} board
        ;; going from indexes to numbers (1-n) ... like an animal
        row-number (inc current-row)
        column-number (inc current-column)]
    (+ (* row-number 1000) (* column-number 4) (facing-for-direction current-direction))))
