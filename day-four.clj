(def test-data-raw ["2-4,6-8"
                    "2-3,4-5"
                    "5-7,7-9"
                    "2-8,3-7"
                    "6-6,4-6"
                    "2-6,4-8"])

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn split-line [line]
  (str/split line #","))

(defn split-section [section]
  (map #(Integer/parseInt %) (str/split section #"-")))

(defn parse-data [data]
  (map #(map split-section %) (map split-line test-data-raw)))

(def test-data (parse-data test-data-raw))
(def real-data (parse-data (str/split-lines (slurp "day-four.txt"))))

;; Part one

(defn contains? [elf-one-assignments elf-two-assignments]
  (let [elf-one-section-start (first elf-one-assignments)
        elf-one-section-end (last elf-one-assignments)
        elf-two-section-start (first elf-two-assignments)
        elf-two-section-end (last elf-two-assignments)]
    (and (<= elf-one-section-start elf-two-section-start) (>= elf-one-section-end elf-two-section-end))))

(contains? [1 5] [2 3])

(defn fully-contains? [elf-one-assignments elf-two-assignments]
  (or (contains? elf-one-assignments elf-two-assignments)
      (contains? elf-two-assignments elf-one-assignments)))

(fully-contains? [2 3] [1 5])

(reduce + (map #(if (fully-contains? (first %) (second %)) 1 0) test-data))
(reduce + (map #(if (fully-contains? (first %) (second %)) 1 0) real-data))

;; Part two (can just use sets for this one)

(defn set-of-assignments [assignments]
  (let [start (first assignments)
        end (last assignments)]
    (set (range start (inc end)))))

(defn any-overlap? [elf-one-assignments elf-two-assignments]
  (let [elf-one-job-set (set-of-assignments elf-one-assignments)
        elf-two-job-set (set-of-assignments elf-two-assignments)]
    (not (empty? (set/intersection elf-one-job-set elf-two-job-set)))))

(reduce + (map #(if (any-overlap? (first %) (second %)) 1 0) test-data))
(reduce + (map #(if (any-overlap? (first %) (second %)) 1 0) real-data))

;; Part one with sets
(defn either-contains? [elf-one-assignments elf-two-assignments]
  (let [elf-one-job-set (set-of-assignments elf-one-assignments)
        elf-two-job-set (set-of-assignments elf-two-assignments)]
    (or (set/subset? elf-one-job-set elf-two-job-set) (set/subset? elf-two-job-set elf-one-job-set))))

(reduce + (map #(if (either-contains? (first %) (second %)) 1 0) test-data))
(reduce + (map #(if (either-contains? (first %) (second %)) 1 0) real-data))
