(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def test-data-raw
  [
   "30373"
   "25512"
   "65332"
   "33549"
   "35390"
   ])

(def real-data-raw (str/split-lines (slurp "day-eight.txt")))

(def expected-test-tree-map '(5, 5, "3037325512653323354935390"))

(defn read-tree-map [data]
  [(count (first data)) (count data) (apply str data)])

(defn tree-data [tree-map] (last tree-map))

;; get the height of a particular tree
(defn tree-height [tree-map tree-index]
  (Integer/parseInt (subs (tree-data tree-map) tree-index (inc tree-index))))

(defn heights-for-row [tree-map row]
  (let [[width height trees] tree-map
        row-start-index (* row width)]
    (map #(tree-height tree-map %) (range row-start-index (+ row-start-index width)))))

(defn heights-for-column [tree-map column]
  (let [[width height trees] tree-map]
    (map #(tree-height tree-map %) (map #(+ column (* width %)) (range 0 height)))))

;; if there are no neighbours (outside tree) or the tree is taller than all the
;; neighbours
(defn taller-than-neighbours? [tree neighbours]
  (or (= (count neighbours) 0) (every? #(> tree %) neighbours)))

(defn row-and-column [tree-map tree-index]
  (let [[width height trees] tree-map]
    [(int(/ tree-index height)) (mod tree-index width)]))

;; return north,south,east and west vectors of neighbours.
;;
;; We are updating this function for part two to reverse north and west
;; so they are tree-index centric (this won't affect part-one)
(defn all-neighbours [tree-map tree-index]
  (let [[row column] (row-and-column tree-map tree-index)
        row-heights (split-at column (heights-for-row tree-map row))
        column-heights (split-at row (heights-for-column tree-map column))]
    [
     ;; north
     (reverse (first column-heights))
     ;; south
     (rest (last column-heights))
     ;; east
     (rest (last row-heights))
     ;; west
     (reverse (first row-heights))
     ]
    )
  )

(defn is-visible? [tree-map tree-index]
  (boolean (some true? (map #(taller-than-neighbours? (height tree-map tree-index) %) (all-neighbours tree-map tree-index)))))

(defn count-visible-trees [tree-map]
  (count (filter true? (map #(is-visible? tree-map %) (range 0 (count (tree-data tree-map)))))))

(count-visible-trees (read-tree-map test-data-raw))
(count-visible-trees (read-tree-map real-data-raw))

;; part two - scenic scores
(defn score-neighbours [tree neighbours]
  (let [shorter-neighbours (count (take-while #(< % tree) neighbours))]
    (+ shorter-neighbours (if (= shorter-neighbours (count neighbours)) 0 1)
       )
    ))

(defn scenic-score [tree-map tree-index]
  (apply * (map #(score-neighbours (height tree-map tree-index) %) (all-neighbours tree-map tree-index))))

(defn best-scenic-score [tree-map]
  (last (sort (map #(scenic-score tree-map %) (range 0 (count (tree-data tree-map)))))))

(best-scenic-score (read-tree-map test-data-raw))
(best-scenic-score (read-tree-map real-data-raw))
