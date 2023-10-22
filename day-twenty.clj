(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round]])

(def test-data-raw ["1" "2" "-3" "3" "-2" "0" "4"])
(def real-data-raw (str/split-lines (slurp "day-twenty.txt")))

(def robot-types [:ore-robots :clay-robots :obsidian-robots :geode-robots])
(def resource-types [:ore :clay :obsidian :geode])

(defn parse-data [data]
  (apply vector (map-indexed (fn [idx itm] [idx (Integer/parseInt itm)]) data)))

(defn index-of-pred [pred coll] (ffirst (filter (comp pred second) (map-indexed list coll))))

(defn wrap-index [sequence index]
  ;; indexes need to wrap around the end of the seqence
  (mod index (count sequence)))

(assert (and (= (wrap-index [0 1 2] 4) 1)
      (= (wrap-index [0 1 2] 5) 2)
      (= (wrap-index [0 1 2] -2) 1)
      (= (wrap-index [0 1 2] -1) 2)))

(defn new-index-for-value [sequence index value]
  ;; pass in the value (even though it's in the sequence at index), to make this
  ;; easier to test
  (let [offset-index      (+ index value)
        direction         (if (> value 0) :right :left)
        steps-in-sequence (count sequence)
        steps-before-end  (if (= direction :right) (- (dec steps-in-sequence) index) index)
        ]
    (if (<= (abs value) steps-before-end)
      ;; we don't have to worry about wrapping around the end, so just return the wrapped-index
      (wrap-index sequence offset-index)
      ;; otherwise we need to account for moving from the begining of the array
      ;; to the end (or vice versa) which means the index needs to be
      ;; decremented (or incremented) for each time we wrap around
      (let [
            remaining-steps (- (abs value) steps-before-end)
            amount-to-adjust (round (ceil (/ remaining-steps (dec steps-in-sequence))))
            adjust-fn        ({:left - :right +} direction)]
        (wrap-index sequence (adjust-fn offset-index amount-to-adjust)))
      )
    )
  )

(assert (and (= (new-index-for-value [0 1 2 3] 0 1) 1)
      (= (new-index-for-value [0 1 2 3] 0 2) 2)
      (= (new-index-for-value [0 1 2 3] 0 3) 3)
      (= (new-index-for-value [1 2 -2 -3 0 3 4] 2 -2) 0)
      ;; wrap around the left end once
      (= (new-index-for-value [0 1 2 3] 0 -1) 2)
      ;; wrap around the left end twice
      (= (new-index-for-value [0 1 2 3] 0 -4) 2)
      (= (new-index-for-value [0 1 2 3] 0 -5) 1)
      (= (new-index-for-value [0 1 2 3] 3 1) 1)))

(defn adjust-index-based-on-shifting-index [shifting-index new-index]
  (+ new-index (if (< new-index shifting-index) 0 1)))

(defn shift-to [sequence index new-index]
  (let [
        ;; what is the item at index
        item-to-shift (nth sequence index)
        ;; remove the existing item (which we'll remove after we splice in at
        ;; the new location)
        sequence-post-remove-existing-item (assoc sequence index nil)
        ;; split the sequence to re-insert our item
        [first-part second-part] (split-at (adjust-index-based-on-shifting-index index new-index)  sequence-post-remove-existing-item)
        ;; re-add the item and then combine those sequences, then remove nils
        re-combined (remove nil? (apply concat [first-part (conj second-part item-to-shift)]))
        ]
    ;; append item at index to the first half of the sequence split at new-index
    (apply vector re-combined)))

(def debug false)

(defn shift-at [sequence index]
  (let [;; what is the item at index
        item-to-shift (nth sequence index)
        ;; what is our value (how far do we shift left or right
        value (second item-to-shift)
        ;; determine the new index
        new-index (new-index-for-value sequence index value)]
    (if debug
      (do
        (println (map #(second %) sequence))
        (println (format "%d moves to index %d" value new-index))))
    (shift-to sequence index new-index)))

(defn shift [sequence original-index]
  ;; given the original index, use the first of each item to work out its
  ;; current index
  (shift-at sequence (index-of-pred #(= (first %) original-index) sequence)))

(defn mix [data]
  (reduce (fn [new-data i] (shift new-data i)) data (range 0 (count data))))

(defn solution-part-one [data]
  (let [mixed-data (map #(second %) (mix data))
        index-of-zero (index-of-pred #(= % 0) mixed-data)
        offset-values (map #(nth mixed-data (wrap-index mixed-data (+ % index-of-zero))) [1000, 2000, 3000])]
    (println offset-values)
    (reduce + offset-values)))

(defn solution-part-two [data]
  (let [decryption-key 811589153
        decryption-count 10
        decrypted-data (apply vector (map (fn [[index value]] [index (* value decryption-key)]) data))
        mixed-data (map #(second %) (loop [current-data decrypted-data
                                           i decryption-count]
                                      (if (= i 0)
                                        current-data
                                        (recur (mix current-data) (dec i)))))
        index-of-zero (index-of-pred #(= % 0) mixed-data)
        offset-values (map #(nth mixed-data (wrap-index mixed-data (+ % index-of-zero))) [1000, 2000, 3000])]
    (println mixed-data)
    (println offset-values)
    (reduce + offset-values)))

;; Initial arrangement:
;; 1 2 -3 3 -2 0 4

;; 1 moves between 2 and -3:
;; 2 1 -3 3 -2 0 4

;; 2 moves between -3 and 3:
;; 1 -3 2 3 -2 0 4

;; -3 moves between -2 and 0:
;; 1 2 3 -2 -3 0 4

;; 3 moves between 0 and 4:
;; 1 2 -2 -3 0 3 4

;; -2 moves between 4 and 1:
;; 1 2 -3 0 3 4 -2

;; 0 does not move:
;; 1 2 -3 0 3 4 -2

;; 4 moves between -3 and 0:
;; 1 2 -3 4 0 3 -2

;; -----------------------
;; part two

;; Initial arrangement:
;; 811589153, 1623178306, -2434767459, 2434767459, -1623178306, 0, 3246356612

;; After 1 round of mixing:
;; 0, -2434767459, 3246356612, -1623178306, 2434767459, 1623178306, 811589153

;; After 2 rounds of mixing:
;; 0, 2434767459, 1623178306, 3246356612, -2434767459, -1623178306, 811589153

;; After 3 rounds of mixing:
;; 0, 811589153, 2434767459, 3246356612, 1623178306, -1623178306, -2434767459

;; After 4 rounds of mixing:
;; 0, 1623178306, -2434767459, 811589153, 2434767459, 3246356612, -1623178306

;; After 5 rounds of mixing:
;; 0, 811589153, -1623178306, 1623178306, -2434767459, 3246356612, 2434767459

;; After 6 rounds of mixing:
;; 0, 811589153, -1623178306, 3246356612, -2434767459, 1623178306, 2434767459

;; After 7 rounds of mixing:
;; 0, -2434767459, 2434767459, 1623178306, -1623178306, 811589153, 3246356612

;; After 8 rounds of mixing:
;; 0, 1623178306, 3246356612, 811589153, -2434767459, 2434767459, -1623178306

;; After 9 rounds of mixing:
;; 0, 811589153, 1623178306, -2434767459, 3246356612, 2434767459, -1623178306

;; After 10 rounds of mixing:
;; 0, -2434767459, 1623178306, 3246356612, -1623178306, 2434767459, 811589153
