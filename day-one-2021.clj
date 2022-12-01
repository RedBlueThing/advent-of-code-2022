(defn square [x]
  (* x x))

(square 10)

(clojure.string/upper-case "clojure")

(def test-data [199 200 208 210 200 207 240 269 260 263])

(defn increase-count [first second] (if (> second first) 1 0))

(defn increase-count-recursion [count previous remaining]
  (if-not remaining count (let [current (first remaining)]
                            (increase-count-recursion (if previous (+ (increase-count previous current) count) 0) current (next remaining)))))

(defn window-indexes [len window-size]
  (filter some? (for [current (range 0 len)] (if (<= (+ current window-size) len) (range current (+ current window-size))))))

(defn sum-indexed [data index-sequence]
  (reduce + (map #(nth data %1) index-sequence))
  )

(defn smooth [data]
  (let [indexes (window-indexes (count data) 3)]
    (map #(sum-indexed data %1) indexes))
  )

(require '[clojure.string :as str])
(def real-data (map #(Integer/parseInt %1) (str/split-lines (slurp "2021-day-one.txt"))))
(increase-count-recursion 0 nil real-data)
(increase-count-recursion 0 nil (smooth real-data))
