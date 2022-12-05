(def test-data-raw [
"    [D]    "
"[N] [C]    "
"[Z] [M] [P]"
" 1   2   3 "
nil
"move 1 from 2 to 1"
"move 3 from 1 to 3"
"move 2 from 2 to 1"
"move 1 from 1 to 2"])

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def expected-stack-data {1 '(\Z \N)
                          2 '(\M \C \D)
                          3 '(\P)})

(def expected-procedure-data [[1 2 1][3 1 3][2 2 1][1 1 2]])

(defn parse-box [box] (char (first (subs box 1 2))))

(defn parse-box-row [box-row]
  (map-indexed vector (map second (filter #(= 1 (mod (first %) 4)) (map-indexed vector box-row)))))

;; update dictionary of stacks to boxes
(defn box-map-reducer [stack box]
  (let [[index chr] box] (update-in stack [(inc index)] conj chr)))

(defn parse-stack-data [data]
  (let [stacks (map #(Integer/parseInt %) (filter #(not= % "") (str/split (last data) #" ")))
        ;; let's reverse boxes so we start at the bottom row
        box-rows (map parse-box-row (reverse (butlast data)))
        box-row-map (reduce box-map-reducer {} (partition 2 (flatten box-rows)))]
    (into {} (map (fn [[k v]] [k (reverse (filter #(not= \space %) v))]) box-row-map))))

(defn parse-procedure-data [data]
  (map (fn parse-procedure-line [line] (vec (map #(Integer/parseInt %) (filter #(not (empty? %)) (str/split line #"[^\d]"))))) data))

;; First we need to split the input into the stacks and the rearrangement procedure
(defn parse-data [data]
  (let [split-data (partition-by #(= nil %) data)
        stack-data (first split-data)
        procedure-data (last split-data)]
    {:stacks (parse-stack-data stack-data)
     :procedure (parse-procedure-data procedure-data)}))

(def test-data (parse-data test-data-raw))
(def real-data (parse-data (map #(if (= % "") nil %) (str/split-lines (slurp "day-five.txt")))))

(assert (= expected-stack-data (test-data :stacks)))
(assert (= expected-procedure-data (test-data :procedure)))

(defn apply-procedure-to-stacks [crate-mover stacks procedure-step]
  (let [[number-moved origin-stack-number destination-stack-number] procedure-step
        destination-stack (concat (stacks destination-stack-number) (crate-mover (take number-moved (reverse (stacks origin-stack-number)))))
        origin-stack (drop-last number-moved (stacks origin-stack-number))]

    (into {} (map (fn [[k v]] [k
                               (cond (= k destination-stack-number) destination-stack
                                     (= k origin-stack-number) origin-stack
                                     :else v)])) stacks)))

(defn top-boxes [stacks]
  (str/join (map last (map #(stacks %) (sort (keys stacks))))))

(defn solve [data crate-mover] (top-boxes (reduce (partial apply-procedure-to-stacks crate-mover) (data :stacks) (data :procedure))))

;; part one
(solve test-data identity)
(solve real-data identity)

;; part two
(solve test-data reverse)
(solve real-data reverse)

