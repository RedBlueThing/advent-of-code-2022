(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def test-data-raw
  ["R 4"
   "U 4"
   "L 3"
   "D 1"
   "R 4"
   "D 1"
   "L 5"
   "R 2"])

(def part-two-test-data-raw
  ["R 5"
   "U 8"
   "L 8"
   "D 3"
   "R 17"
   "D 10"
   "L 25"
   "U 20"
   ])

(def real-data-raw (str/split-lines (slurp "day-nine.txt")))

(defn cast-instruction [[direction times-str]]
  [direction (Integer/parseInt times-str)])

(defn parse-instructions [lines]
  (map (fn [line] (cast-instruction (str/split line #" "))) lines))

(defn neighbours-for-coordinates [x y]
  (let [offsets [-1 0 1]]
    (disj (set (mapcat (fn [x-offset] (map (fn [y-offset] [(+ x x-offset) (+ y y-offset)]) offsets)) offsets)) [x y])))

(defn tail-needs-to-move [head tail]
  (boolean (let [[ head-x head-y ] head
                 [ tail-x tail-y ] tail ]
     (and
      ;; Are they in the same position
      (not (= head tail))
      ;; and tail isn't in the neighbours of head (already adjacent)
      (not (some (conj #{} tail) (neighbours-for-coordinates head-x head-y)))))))

;; of those candidates we want to pick the one the in same x or y as
;; head
(defn pick-tail-position [candidates head]
    (let [[ head-x head-y ] head ]
      (first (sort (fn [[x y] p2]
                     ;; is x y (p1) closer to head than p2
                     (or (= x head-x) (= y head-y))) candidates))))

;; Tail position given a new head position
(defn tail-position [head tail]
  (let [[ head-x head-y ] head
        [ tail-x tail-y ] tail ]
    (if (not (tail-needs-to-move head tail))
      tail
      (-> (neighbours-for-coordinates head-x head-y)
          ;; get the candidates for the tail move
          (set/intersection (neighbours-for-coordinates tail-x tail-y))
          ;; of those candidates, pick the right one
          (pick-tail-position head)))))

;; Some tests for tail movement
;;
;; head moves up
(assert (= (tail-position [0 2] [0 0]) [0 1] ))
;; adjacent
(assert (= (tail-position [0 1] [0 0]) [0 0] ))
;; head is right
(assert (= (tail-position [2 0] [0 0]) [1 0] ))
;; head is up and right
(assert (= (tail-position [1 2] [0 0]) [1 1] ))

;; return a new head position given a direction
(defn move-head [[head-x head-y] direction]
  (case direction
    "R" [(inc head-x) head-y]
    "L" [(dec head-x) head-y]
    "U" [head-x (inc head-y)]
    "D" [head-x (dec head-y)]))

(defn sequence-of-directions-old [direction times]
  (map (fn [index] direction) (range 0 times)))

;; better way of doing this with repeat
(defn sequence-of-directions [direction times]
  (repeat times direction))

(defn move-rope-section [direction current-rope knot]
  (let [
        ;; if the current rope is empty, then this is our first knot. The head
        ;; of the rope.
        is-first-knot? (= (count current-rope) 0)]
    ;; so we either just move the first knot in the direction, or we move the
    ;; knot based on the end of the current-rope (which this reducer is building
    ;; up)
    (conj current-rope (if is-first-knot? (move-head knot direction)
                           (tail-position (last current-rope) knot)))))

(defn move-rope [rope direction]
  (reduce (partial move-rope-section direction) [] rope))

(defn update-tail-set-for-rope [tail-set rope]
  (conj tail-set (last rope)))

(defn simulate-single-direction [current-state direction]
  (let [[tail-set rope] current-state
        new-rope (move-rope rope direction)]
    [(update-tail-set-for-rope tail-set new-rope) new-rope]))

(defn simulate-instruction [current-state instruction]
  (let [[direction times] instruction
        [tail-set rope] current-state]
    (reduce simulate-single-direction current-state (sequence-of-directions direction times))))

(defn simulate-instructions [instructions knots]
  (let [;; our starting positions
        rope (repeat knots [0 0])]
    (reduce simulate-instruction [(update-tail-set-for-rope #{} rope) rope] instructions)))

(def expected-part-one-positions 13)
(assert (= (count (first (simulate-instructions (parse-instructions test-data-raw) 2))) expected-part-one-positions))

(def expected-part-two-positions 36)
(assert (= (count (first (simulate-instructions (parse-instructions part-two-test-data-raw) 10))) expected-part-two-positions))

(count (first (simulate-instructions (parse-instructions real-data-raw) 10)))

