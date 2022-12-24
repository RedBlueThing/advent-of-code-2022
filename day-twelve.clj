(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def test-data-raw
  ["Sabqponm"
   "abcryxxl"
   "accszExk"
   "acctuvwj"
   "abdefghi"])

(def real-data-raw (str/split-lines (slurp "day-twelve.txt")))

(defn costs-for-length [length]
  (vec (repeat length Integer/MAX_VALUE)))

(def expected-test-map (seq [5 8 "SabqponmabcryxxlaccszExkacctuvwjabdefghi" (costs-for-length (* 5 8))]))

(defn read-height-map [data]
  (let [rows  (count data)
        columns (count (first data))]
    [rows columns (apply str data) (costs-for-length (* rows columns))]))

(assert (= expected-test-map (read-height-map test-data-raw)))

(defn height-data [height-map] (last (butlast height-map)))

(defn char-for-index [height-map index]
  (first (subs (height-data height-map) index (inc index))))

(defn cost-for-index [height-map index]
  (nth (last height-map) index))

(defn maybe-map-height [chr]
  ;; We start (S) at height \a and end (E) at height \z
  (or ({\S \a \E \z} chr) chr))

(defn up-neighbour [rows columns index]
  (if (>= index columns) (- index columns)))

(defn down-neighbour [rows columns index]
  (if (< index (- (* rows columns) columns)) (+ index columns)))

(defn right-neighbour [rows columns index]
  (if (and (not= (mod (inc index) columns) 0) (not= (inc index) (* rows columns))) (inc index)))

(defn left-neighbour [rows columns index]
  (if (and (not= (mod index columns) 0) (not= index 0)) (dec index)))

(defn neighbours [height-map index]
  (let [[rows columns, height-data costs] height-map]
    (assert index)
    (set (filter #(not= nil %) (-> (set [])
                                   ;; up
                                   (conj (up-neighbour rows columns index))
                                   ;; down
                                   (conj (down-neighbour rows columns index))
                                   ;; right
                                   (conj (right-neighbour rows columns index))
                                   ;; left
                                   (conj (left-neighbour rows columns index)))))))

;; If the first spot has neightbours that include the second spot, or vice versa
(defn adjacent? [height-map first-index second-index]
  (or (contains? (neighbours height-map first-index) second-index)
      (contains? (neighbours height-map second-index) first-index)))

(defn reachable [height-map first-index second-index]
  ;; weird to be checking locations that aren't next to each other.
  (assert (adjacent? height-map first-index second-index))
  ;; if the second index reachable from the first index
  (let [first-value (maybe-map-height (char-for-index height-map first-index))
        second-value (maybe-map-height (char-for-index height-map second-index))]
    ;; is the first location plus one high above (or equal to) the second location
    (>= (inc (int first-value)) (int second-value))))

(defn reachable-neighbours [height-map index]
  (set (filter #(reachable height-map index %) (neighbours height-map index))))

(defn start-index [height-map]
  (str/last-index-of (height-data height-map) "S"))

(defn end-index [height-map]
  (str/last-index-of (height-data height-map) "E"))

(neighbours (read-height-map test-data-raw) 0)
(reachable-neighbours (read-height-map test-data-raw) 0)

(defn update-cost [height-map index cost]
  (let [[rows columns height-data costs] height-map]
    [rows columns height-data (update costs index (fn [old-cost]
                                                     (if (< cost old-cost) cost old-cost)))]
    ))

(defn update-costs [height-map indexes cost]
  (reduce (fn costs-reducer [height-map index] (update-cost height-map index cost)) height-map indexes ))

(defn next-to-visit [height-map unvisited]
  ;; if we are checking for the next to visit we should have unvisited spots
  (assert (not (empty? unvisited)))
  ;; the next to visit is the unvisited indexes (who are also neighbours) sorted by their costs
  (let [
        sorted-by-cost (sort (map #(vec [(cost-for-index height-map %) %]) unvisited))
        cost (first (first sorted-by-cost))
        index (second (first sorted-by-cost))]
    index))

(defn shortest-paths [height-map start-index]
  (println "Checking " start-index)
  (loop [current start-index
        current-height-map (update-cost height-map current 0)
        ;; we start with unvisited indexs (all except the current, start, index)
         unvisited (disj (set (range 0 (count (last height-map)))) current)]
    (if (or (= current (end-index height-map)) (= (cost-for-index current-height-map current) Integer/MAX_VALUE))
      ;; we found our destination, just return the data (and the costs).
      current-height-map
      ;; we are still looking, update our reachable neighbours costs based on our cost
      (let [
            neighbours (reachable-neighbours current-height-map current)
            height-map-with-updated-costs (update-costs current-height-map neighbours (inc (cost-for-index current-height-map current)))
            new-current (next-to-visit height-map-with-updated-costs unvisited)]
        (recur ;; new current, new height map, new unvisited
         new-current
         height-map-with-updated-costs
         (disj unvisited new-current))))))


(defn shortest-path-to-destination [height-map start-index]
  (cost-for-index (shortest-paths height-map start-index) (end-index height-map)))

(def expected-test-shortest-path 31)
(assert (= expected-test-shortest-path (let [height-map (read-height-map test-data-raw)](shortest-path-to-destination height-map (start-index height-map)))))

(start-index (read-height-map real-data-raw))
(end-index (read-height-map real-data-raw))

;; part two

(defn start-indicies [height-map]
  ;; find all the spots at elevation \a
  (map first (filter #(= (maybe-map-height (second %)) \a) (map-indexed vector (height-data height-map)))))

(defn shortest-path-many-start-indexes-to-destination [height-map start-indices]
  (map #(shortest-path-to-destination height-map %) start-indices))

(apply min (let [height-map (read-height-map test-data-raw)] (shortest-path-many-start-indexes-to-destination height-map (start-indicies height-map))))

;; (apply min (let [height-map (read-height-map real-data-raw)] (shortest-path-many-start-indexes-to-destination height-map (start-indicies height-map))))

