(def test-data-raw ["vJrwpWtwJgWrhcsFMMfFFhFp"
                "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
                "PmmdzqPrVvPwwTWBwg"
                "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
                "ttgJtRGJQctTZtZT"
                "CrZsJsPPZsGzwwsLwLmpwMDw"
                ])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math.combinatorics :as combo])

;; Part one
(def test-data-p1 (map #(map set (split-at (/ (count %) 2) %)) test-data-raw))
(def real-data-p1 (map #(map set (split-at (/ (count %) 2) %)) (str/split-lines (slurp "day-three.txt"))))

(defn common-item [sequence-of-sets]
  (apply set/intersection sequence-of-sets))

(defn priority-for-item [item]
  (if (Character/isUpperCase item)
    (+ (- (int item) (int \A)) 27)
    (+ (- (int item) (int \a)) 1))
  )

(defn score-data [data]
  (reduce + (map #(priority-for-item (first (common-item %))) data)))

(score-data test-data-p1)

;;; Part two
(defn parse-raw-data [raw-data]
  (partition 3 (map set raw-data)))

(def test-data-p2 (parse-raw-data test-data-raw))
(def real-data-p2 (parse-raw-data (str/split-lines (slurp "day-three.txt"))))

(score-data test-data-p2)
