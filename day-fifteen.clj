(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def test-data-raw
  [
   "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
   "Sensor at x=9, y=16: closest beacon is at x=10, y=16"
   "Sensor at x=13, y=2: closest beacon is at x=15, y=3"
   "Sensor at x=12, y=14: closest beacon is at x=10, y=16"
   "Sensor at x=10, y=20: closest beacon is at x=10, y=16"
   "Sensor at x=14, y=17: closest beacon is at x=10, y=16"
   "Sensor at x=8, y=7: closest beacon is at x=2, y=10"
   "Sensor at x=2, y=0: closest beacon is at x=2, y=10"
   "Sensor at x=0, y=11: closest beacon is at x=2, y=10"
   "Sensor at x=20, y=14: closest beacon is at x=25, y=17"
   "Sensor at x=17, y=20: closest beacon is at x=21, y=22"
   "Sensor at x=16, y=7: closest beacon is at x=15, y=3"
   "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
   "Sensor at x=20, y=1: closest beacon is at x=15, y=3"])

(def real-data-raw (str/split-lines (slurp "day-fifteen.txt")))

(defn parse-line [line]
  (map #(Integer/parseInt %) (rest (re-matches #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)" line))))

(defn parse-data [data]
  (map parse-line data))

(parse-data test-data-raw)

(defn manhattan-distance [[x y beacon-x beacon-y]]
  (+ (- (max x beacon-x) (min x beacon-x))
     (- (max y beacon-y) (min y beacon-y))))

(defn start-and-end-coverage-for-sensor-at-row [sensor row]
  (let [[x y beacon-x beacon-y] sensor
        no-beacon-range (manhattan-distance sensor)
        distance-to-row (- (max y row) (min y row))]
    (if (> distance-to-row no-beacon-range) nil
        (let [beacon-spread (- no-beacon-range distance-to-row)
              beacon-spread-start (- x beacon-spread)
              beacon-spread-end (+ x beacon-spread)]
            (if (= beacon-y row)
              (do
                (assert (or (= beacon-x beacon-spread-start) (= beacon-x beacon-spread-end)))
                (if (= beacon-x beacon-spread-start)
                  [(inc beacon-spread-start) beacon-spread-end]
                  [beacon-spread-start (dec beacon-spread-end)])
                )
              [beacon-spread-start beacon-spread-end])))))

(defn coverage-at-row [data row]
  (filter #(not (nil? %)) (map #(start-and-end-coverage-for-sensor-at-row % row) data)))

(defn brute-force-coverage [data row]
  ;; just create a sequence of sets of all the "no beacon" x values
  (apply set/union (map (fn [[start-x end-x]] (set (range start-x (inc end-x)))) (coverage-at-row data row)))
  )

(coverage-at-row (parse-data test-data-raw) 10)
(count (brute-force-coverage (parse-data test-data-raw) 10))

;; part two

(defn calculate-tuning-frequency [x y]
  (+ (* x 4000000) y))

(def test-part-two-max-y 20)
(def part-two-max-y 4000000)
