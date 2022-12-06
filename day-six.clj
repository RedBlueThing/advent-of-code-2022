(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def test-data-raw "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(def some-more-test-data-raw
  ["bvwbjplbgvbhsrlpgdmjqwftvncz"
   "nppdvjthqldpwncqszvftbrmjlhg"
   "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
   "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"])

(def some-expected-first-markers [5 6 10 11])

(def real-data-raw (slurp "day-six.txt"))

(defn marker? [data index marker-length]
  ;; just look at the last four characters (including index) and see if they are distinct
  (if (< index (dec marker-length))
    false
    (= marker-length (count (set (subs data (- index (dec marker-length)) (inc index)))))))

(defn solve [data marker-length]
  ;; the inc to go from a zero based index to a character number
  (inc (loop [index 0] (if (marker? data index marker-length) index (recur (inc index))))))

(assert (= (map #(solve % 4))) some-expected-first-markers)

;; part one
(solve real-data-raw 4)

;; part two
(solve real-data-raw 14)
