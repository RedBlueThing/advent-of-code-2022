(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math.combinatorics :as combo])

(def test-data-raw ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(def real-data-raw (slurp "day-seventeen.txt"))

(defn parse-data [data]
  (map #(if (= % \>) :right :left) (seq data)))

(def cavern-width-units 7)

(def left-edge-starting-x 2) ; 0,1,#
(def bottom-edge-starting-y-offset 4) ; 0,1,2,3,#

(defn next-rock-type [current-rock-type]
  ;; A function that gives me the next rock type given the last one.
  (let [rock-types-in-order [:horizontal-line :cross :reverse-l :vertical-line :block]
        successors (zipmap rock-types-in-order (concat (rest rock-types-in-order) [(first rock-types-in-order)]))]
    (successors current-rock-type)))

(defn next-gas-jet [data offset]
  ;; what is the next direct gas jet given offset from when the simulation
  ;; started.
  (nth (cycle data) offset))

