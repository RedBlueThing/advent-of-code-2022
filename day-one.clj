(def test-data [1000 2000 3000 nil 4000 nil 5000 6000 nil 7000 8000 9000 nil 10000])
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])

(def real-data (map #(if (not= %1 "") (Integer/parseInt %1)) (str/split-lines (slurp "day-one.txt"))))

;; Checking for nils .. I think I like the first one
(some nil? (seq [1 2 3 nil]))
(every? identity (seq [1 2 3]))

(defn any-nils-present-in-last-sequence [data]
  (some nil? (last data)))

;; use partition-by to split our input data
(defn split-data [data]
  (filter #(some? (first %)) (partition-by #(= nil %) data)))

(defn sum-calories [data]
  (reduce + data))

;; find the maxium value
(apply max (map sum-calories (split-data real-data)))

;; part two .. just need to sort by calories and take the last three
(sum-calories (take-last 3 (sort (map sum-calories (split-data real-data)))))



