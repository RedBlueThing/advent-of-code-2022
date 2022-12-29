(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def test-data-raw
  [""
   "[1,1,3,1,1]"
   "[1,1,5,1,1]"
   ""
   "[[1],[2,3,4]]"
   "[[1],4]"
   ""
   "[9]"
   "[[8,7,6]]"
   ""
   "[[4,4],4,4]"
   "[[4,4],4,4,4]"
   ""
   "[7,7,7,7]"
   "[7,7,7]"
   ""
   "[]"
   "[3]"
   ""
   "[[[]]]"
   "[[]]"
   ""
   "[1,[2,[3,[4,[5,6,7]]]],8,9]"
   "[1,[2,[3,[4,[5,6,0]]]],8,9]"])

(def real-data-raw (str/split-lines (slurp "day-thirteen.txt")))

(defn parse-data [data] (->>
                         ;; split up our input data by empty lines
                         (partition-by #(= "" %) (map str/trim data))
                         ;; then filter out those empty lines so all we have is
                         ;; each pair of messages
                         (filter #(> (count %) 1))
                         ;; Just read each string as code
                         ((fn [pair] (map #(map read-string %) pair)))
                         vec))

(defn elements-in-order? [first-element second-element]
  ;; if they are both vectors, just call this again with the first element
  (let [result (cond
                 ;; If both elements are vectors, we just pass it down
                 (and (vector? first-element) (vector? second-element))
                 (cond
                   ;; we have elements left inboth vectors, keep going
                   (and (> (count first-element) 0) (> (count second-element) 0))
                   (elements-in-order? (first first-element) (first second-element))
                   ;; there were no elements in both vectors, no result
                   (and (= (count first-element) 0) (= (count second-element) 0))
                   :no-result
                   ;; we ran out of elements in the first one, in order
                   (= (count first-element) 0)
                   :in-order
                   (= (count second-element) 0)
                   :out-of-order)
                 ;; if the first element is a vector (the second can't be)
                 (vector? first-element)
                 (elements-in-order? first-element (vector second-element))
                 ;; or the second element is a vector (the first can't be
                 (vector? second-element)
                 (elements-in-order? (vector first-element) second-element)
                 ;; then they are both integers? (or maybe empty lists)
                 (and (integer? first-element) (integer? second-element))
                 (if (= first-element second-element)
                   :no-result
                   (if (< (or first-element 0) (or second-element 0))
                     :in-order
                     :out-of-order)))]

    ;; if we didn't get a result and there additional elements to look at, then
    ;; keep going.
    (if (and (= result :no-result)
             (vector? first-element)
             (vector? second-element)
             (or (> (count first-element) 0)
                 (> (count first-element) 0)))
      (let [rest-of-first-element (apply vector (rest first-element))
            rest-of-second-element (apply vector (rest second-element))]
        (elements-in-order? rest-of-first-element rest-of-second-element))
      result)))

(defn in-order? [pair]
  (let [first-packet (first pair)
        second-packet (second pair)]
    (= (elements-in-order? first-packet second-packet) :in-order)))

(defn indexed-pairs-in-order [data]
  (filter #(in-order? (second %)) (map-indexed vector data)))

(defn part-one-solution [data]
  (reduce + (map #(inc (first %)) (indexed-pairs-in-order data))))

;; part two
(def divider-packets [[[2]] [[6]]])

(defn combined-data-stream [data divider-packets]
  (apply conj data divider-packets))

(defn element-comparator [first-element second-element]
  (case (elements-in-order? first-element second-element)
    :in-order -1
    :no-result 0
    :out-of-order 1))

(defn sort-data-stream [data]
  (sort element-comparator data))

;; extract all the packets from the pairs of data
(defn extract-packets [data]
  (reduce (fn packet-reducer [result next-pair] (conj result (first next-pair) (second next-pair)))
          []
          data))

(defn is-divider? [packet divider-packets]
  (not (empty? (set/intersection (set (vector packet)) (set divider-packets)))))

;; just need to findt he divider packet indicies (and one to the index)
(defn part-two-solution [raw-data]
  (let [sorted-packets (sort-data-stream (combined-data-stream (extract-packets (parse-data raw-data)) divider-packets))]
    (reduce * (map (fn extract-item-numbers [entry] (inc (first entry))) (filter #(is-divider? (second %) divider-packets) (map-indexed vector sorted-packets))))
    )
  )

