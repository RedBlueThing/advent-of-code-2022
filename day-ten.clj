(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def test-data-raw
  [
"addx 15"
"addx -11"
"addx 6"
"addx -3"
"addx 5"
"addx -1"
"addx -8"
"addx 13"
"addx 4"
"noop"
"addx -1"
"addx 5"
"addx -1"
"addx 5"
"addx -1"
"addx 5"
"addx -1"
"addx 5"
"addx -1"
"addx -35"
"addx 1"
"addx 24"
"addx -19"
"addx 1"
"addx 16"
"addx -11"
"noop"
"noop"
"addx 21"
"addx -15"
"noop"
"noop"
"addx -3"
"addx 9"
"addx 1"
"addx -3"
"addx 8"
"addx 1"
"addx 5"
"noop"
"noop"
"noop"
"noop"
"noop"
"addx -36"
"noop"
"addx 1"
"addx 7"
"noop"
"noop"
"noop"
"addx 2"
"addx 6"
"noop"
"noop"
"noop"
"noop"
"noop"
"addx 1"
"noop"
"noop"
"addx 7"
"addx 1"
"noop"
"addx -13"
"addx 13"
"addx 7"
"noop"
"addx 1"
"addx -33"
"noop"
"noop"
"noop"
"addx 2"
"noop"
"noop"
"noop"
"addx 8"
"noop"
"addx -1"
"addx 2"
"addx 1"
"noop"
"addx 17"
"addx -9"
"addx 1"
"addx 1"
"addx -3"
"addx 11"
"noop"
"noop"
"addx 1"
"noop"
"addx 1"
"noop"
"noop"
"addx -13"
"addx -19"
"addx 1"
"addx 3"
"addx 26"
"addx -30"
"addx 12"
"addx -1"
"addx 3"
"addx 1"
"noop"
"noop"
"noop"
"addx -9"
"addx 18"
"addx 1"
"addx 2"
"noop"
"noop"
"addx 9"
"noop"
"noop"
"noop"
"addx -1"
"addx 2"
"addx -37"
"addx 1"
"addx 3"
"noop"
"addx 15"
"addx -21"
"addx 22"
"addx -6"
"addx 1"
"noop"
"addx 2"
"addx 1"
"noop"
"addx -10"
"noop"
"noop"
"addx 20"
"addx 1"
"addx 2"
"addx 2"
"addx -6"
"addx -11"
"noop"
"noop"
"noop"
])

;; The sum of these signal strengths is 13140.
(def expected-test-signal-strength-sum 13140)

;; Find the signal strength during the 20th, 60th, 100th, 140th, 180th, and
;; 220th cycles. What is the sum of these six signal strengths?

(def real-data-raw (str/split-lines (slurp "day-ten.txt")))

(def important-cycles #{ 20 60 100 140 180 220 })

(defn cpu-clock-cycle-reducer [cpu-state command]
  (let [[ x cycles important-signal-strengths ] cpu-state
        split-command (str/split command #" ")
        op (first split-command)
        updated-important-signal-strengths (if (contains? important-cycles cycles)
                                             (into important-signal-strengths {cycles (* x cycles)}) important-signal-strengths)]
    (case op
      "addx" [ (+ x (Integer/parseInt (second split-command))) (inc cycles) updated-important-signal-strengths ]
      "noop" [ x (inc cycles) updated-important-signal-strengths ]
      ))
  )

(defn post-process-data [data]
  ;; So if addx don't do anything in the first cycle, we are just going to stick
  ;; an noop before it so we count cycles by command
  (mapcat (fn [command] (if (= (first (str/split command #" ")) "addx") ["noop" command] [command])) data))

(defn run-cpu [data]
  (reduce cpu-clock-cycle-reducer [1 1 {}] data))

(reduce + (vals (last (run-cpu (post-process-data test-data-raw)))))

(reduce + (vals (last (run-cpu (post-process-data real-data-raw)))))


