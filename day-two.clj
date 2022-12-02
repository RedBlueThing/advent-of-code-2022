(def test-data [[ "A" "Y" ] [ "B" "X" ] [ "C" "Z" ]])
(require '[clojure.string :as str])
(def real-data (map #(str/split %1 #" ") (str/split-lines (slurp "day-two.txt"))))

;; A B C are normalized instructions. So we use these when comparing elf and human moves
;; because elves are normal and humans, apparently, are not.
;;
;; It's Christmas you know
(def normalize-human-instructions {"X" "A", "Y" "B", "Z" "C"})
(def denormalize-elf-move (clojure.set/map-invert normalize-human-instructions))

;; A rock
;; B paper
;; C scissors
(defn beats [move-one move-two]
  (case (str/join [move-one move-two])
    ;; rock beats scissors
    "AC" true
    ;; scissors beats paper
    "CB" true
    ;; paper beats rock
    "BA" true
    ;; loser
    false
    )
  )

(defn normalized-win-lose-draw [opponent-move player-move]
  (cond
    (= opponent-move player-move) :draw
    (beats player-move opponent-move) :win
    (beats opponent-move player-move) :lose))

(defn win-lose-draw [elf-will-play human-programmer-should-play]
  (normalized-win-lose-draw elf-will-play (normalize-human-instructions human-programmer-should-play)))

(defn score-for-choice [move]
  ({"X" 1 "Y" 2 "Z" 3} move))

(defn score-choice-and-outcome [choice outcome]
  (+ (score-for-choice choice)
     (case outcome
       :win 6
       :lose 0
       :draw 3)))

(defn score-strategy-round-part-one [round]
  (let [elf-will-play (first round)
        human-programmer-should-play (second round)]
    (score-choice-and-outcome human-programmer-should-play (win-lose-draw elf-will-play human-programmer-should-play))
    ))

(defn total-game-score [fn data]
  (reduce + (map fn data)))

(total-game-score score-strategy-round-part-one test-data)
(total-game-score score-strategy-round-part-one real-data)

;; part two

(defn result-for-instruction [human-programmer-instruction]
  ({"X" :lose "Y" :draw "Z" :win} human-programmer-instruction))

;; rock beats scissors, scissors beats paper, paper beats rock
(def move-to-win-against {"C" "A" "B" "C" "A" "B"})
;; if you want to lose, do the opposite
(def move-to-lose-against (clojure.set/map-invert move-to-win-against) )

(defn unnormalized-move-for-result [opponent-move result]
  ;; we find out what the move should be and then denormalize from a elf move
  ;; back to a human move
  (denormalize-elf-move (case result
     :draw opponent-move
     :win (move-to-win-against opponent-move)
     :lose (move-to-lose-against opponent-move))))

(defn score-strategy-round-part-two [round]
  (let [elf-will-play (first round)
        result-for-round (result-for-instruction (second round))
        human-programmer-must-play (unnormalized-move-for-result elf-will-play result-for-round)]
       (score-choice-and-outcome human-programmer-must-play result-for-round)))

(total-game-score score-strategy-round-part-two test-data)
(total-game-score score-strategy-round-part-two real-data)

