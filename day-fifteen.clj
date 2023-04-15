(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math.combinatorics :as combo])

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
                (cond
                  ;; if there is only on point covered and it has a beacon, just return nil
                  (= beacon-spread-start beacon-spread-end) nil
                  ;; the beacon is at the start
                  (= beacon-x beacon-spread-start) [(inc beacon-spread-start) beacon-spread-end]
                  ;; the beacon is at the end
                  (= beacon-x beacon-spread-end) [beacon-spread-start (dec beacon-spread-end)]
                  ))
              [beacon-spread-start beacon-spread-end])))))

(defn coverage-at-row [data row]
  (filter #(not (nil? %)) (map #(start-and-end-coverage-for-sensor-at-row % row) data)))

(defn brute-force-coverage [data row]
  ;; just create a sequence of sets of all the "no beacon" x values
  (apply set/union (map (fn [[start-x end-x]] (set (range start-x (inc end-x)))) (coverage-at-row data row))))

(assert (= (count (brute-force-coverage (parse-data test-data-raw) 10)) 26))
;; (count (brute-force-coverage (parse-data real-data-raw) 2000000))

;; part two
;; ---------------------------------------

(defn calculate-tuning-frequency [x y]
  (+ (* x 4000000) y))

(def test-part-two-max 20)
(def part-two-max 4000000)

;; (map (fn [index] (map #(start-and-end-coverage-for-sensor-at-row % 0) (parse-data real-data-raw))) (range 0 20))

(defn cap-value [value min-value max-value]
  (cond
    (< value min-value) min-value
    (> value max-value) max-value
    :else value)
  )

(defn capped-beacons-at-row [data row min-x max-x]
  (set (filter #(not (nil? %)) (map (fn [[x y beacon-x beacon-y]] (if (and (= row beacon-y) (<= beacon-x max-x) (>= beacon-x min-x)) beacon-x nil)) data))))

(defn capped-coverage-at-row [data row min-x max-x]
  (apply conj (map (fn [[start-x end-x]] [(cap-value start-x min-x max-x) (cap-value end-x min-x max-x)])
                   (filter #(not (nil? %)) (map #(start-and-end-coverage-for-sensor-at-row % row) data)))
         (map (fn [x] [x x]) (capped-beacons-at-row data row min-x max-x))))

(coverage-at-row (parse-data test-data-raw) 10)
(capped-coverage-at-row (parse-data test-data-raw) 10 0 test-part-two-max)
(capped-beacons-at-row (parse-data test-data-raw) 10 0 test-part-two-max)

(parse-data real-data-raw)
(capped-coverage-at-row (parse-data real-data-raw) 263737 0 part-two-max)
(capped-beacons-at-row (parse-data real-data-raw) 263737 0 part-two-max)

(defn overlapping-cases [range-one range-two]

  (let [[range-one-start-x range-one-end-x] range-one
        [range-two-start-x range-two-end-x] range-two]
    (cond
      ;; one surrounds two
      (and (>= range-two-start-x range-one-start-x) (<= range-two-end-x range-one-end-x)) [true [[range-one-start-x range-one-end-x]] ]
      ;; end of one and two overlap
      (and (>= range-two-start-x range-one-start-x) (<= range-one-end-x range-two-end-x) (<= range-two-start-x (inc range-one-end-x))) [true [[range-one-start-x range-two-end-x]] ]

      :else
      ;; just return what we were given
      [false [[range-one-start-x range-one-end-x] [range-two-start-x range-two-end-x]] ])))

(defn maybe-merge-ranges [range-one range-two]
  (let [
        [range-one-start-x range-one-end-x] range-one
        [range-two-start-x range-two-end-x] range-two
        [first-overlap-match first-result] (overlapping-cases range-one range-two)
         [second-overlap-match second-result] (overlapping-cases range-two range-one)]

    (cond
      first-overlap-match first-result
      second-overlap-match second-result
      :else
      ;; just return what we were given
      [[range-one-start-x range-one-end-x] [range-two-start-x range-two-end-x]]
      )

    ))

(defn merge-coverage [initial-coverage-data]
  (loop [coverage-data initial-coverage-data]
    (let [combinations (combo/combinations coverage-data 2)
          maybe-merge-results (map #(vector % (maybe-merge-ranges (first %) (second %))) combinations)

          new-ranges (disj (set (map (fn [[original-pair result]]
                                  (if (= (count result) 1)
                                    (first result)
                                    nil))
                                maybe-merge-results)) nil)

          ranges-to-remove (disj (set (mapcat (fn [[original-pair result]]
                                                (if (= (count result) 1)
                                                  original-pair
                                                  nil)
                                                ) maybe-merge-results)) nil)
          consolidated-results (apply vector (set/union (apply disj (set (mapcat (fn [[original-pair result]]
                                                                         (if (= (count result) 1)
                                                                           result
                                                                           original-pair)
                                                                         ) maybe-merge-results)) ranges-to-remove) new-ranges))
          ]
      (if (= (count ranges-to-remove) 0)
        consolidated-results
        (recur consolidated-results)
        )
      )
    )
  )

;; Still pretty slow, but at least merge-coverage means we don't allocate 4,000,000 integers each time we check a row.
;; We can stop as soon as we find a gap (an unmergable range).
(defn solve-part-two [data max-value-for-range]
  (loop [current-row 0]
    (if (= (mod current-row 1000) 0) (println current-row))
    (let [coverage-for-row (merge-coverage (capped-coverage-at-row data current-row 0 max-value-for-range))]
      (if (or (= current-row max-value-for-range)
              (> (count coverage-for-row) 0))
        [current-row coverage-for-row]
        (recur (inc current-row))))))

(solve-part-two (parse-data test-data-raw) test-part-two-max)
(calculate-tuning-frequency (inc 13) 11)

;; (solve-part-two (parse-data real-data-raw) part-two-max)
(def real-data-result [ 2948438 [[2843634 4000000] [0 2843632]] ])
(calculate-tuning-frequency (inc 2843632) 2948438)
