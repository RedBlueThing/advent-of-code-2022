(require '[clojure.string :as str])
(require '[clojure.set :as set])

;; This is a definition of monkey

(def test-data-raw ["Monkey 0:"
                    "Starting items: 79, 98"
                    "Operation: new = old * 19"
                    "Test: divisible by 23"
                    "If true: throw to monkey 2"
                    "If false: throw to monkey 3"
                    ""
                    "Monkey 1:"
                    "Starting items: 54, 65, 75, 74"
                    "Operation: new = old + 6"
                    "Test: divisible by 19"
                    "If true: throw to monkey 2"
                    "If false: throw to monkey 0"
                    ""
                    "Monkey 2:"
                    "Starting items: 79, 60, 97"
                    "Operation: new = old * old"
                    "Test: divisible by 13"
                    "If true: throw to monkey 1"
                    "If false: throw to monkey 3"
                    ""
                    "Monkey 3:"
                    "Starting items: 74"
                    "Operation: new = old + 3"
                    "Test: divisible by 17"
                    "If true: throw to monkey 0"
                    "If false: throw to monkey 1"])

(def real-data-raw (str/split-lines (slurp "day-eleven.txt")))

;; (def real-data (map #(if (not= %1 "") (Integer/parseInt %1)) (str/split-lines (slurp "day-one.txt"))))

(defn parse-monkey-id [line]
  (-> (str/split line #" ")
      second
      (str/split #":")
      first
      Integer/parseInt))

(defn parse-monkey-id [line]
  (-> (re-matches #"Monkey (\d):" line)
      last
      Integer/parseInt))

(defn parse-items [line]
  (map #(Integer/parseInt %) (-> (re-matches #"Starting items: (.*)" line)
                                 last
                                 (str/split #", "))))

(defn parse-operation [line]
  (let [line-elements (str/split line #" ")
        op (case (last (butlast line-elements))
             "/" /
             "*" *
             "+" +
             "-" -)
        maybe-constant (if (= (last line-elements) "old") (fn [value] value) (fn [value] (Integer/parseInt (last line-elements))))]
    (fn [worry-level] (op worry-level (maybe-constant worry-level)))))

(defn last-integer [line]
  (-> (str/split line #" ")
      last
      Integer/parseInt))

(defn parse-test [line]
  (let [divisor (last-integer line)]
    (fn [worry-level] (= (mod worry-level divisor) 0))))

(defn parse-monkey-throw [line]
  (last-integer line))

(defn parse-monkey [data]
  (let [monkey-if-test-true (parse-monkey-throw (nth data 4))
        monkey-if-test-false (parse-monkey-throw (nth data 5))]
    {:monkey-id (parse-monkey-id (nth data 0))
     :items (parse-items (nth data 1))
     :operation (parse-operation (nth data 2))
     :divisor (last-integer (nth data 3))
     :predicate (parse-test (nth data 3))
     :destination (fn [predicate-result] (if predicate-result monkey-if-test-true monkey-if-test-false))
     :inspected 0}))

(defn parse-data [data] (->>
                         ;; split up our input data by empty lines
                         (partition-by #(= "" %) (map str/trim data))
                         ;; then filter out those empty lines so all we have is
                         ;; each chunk of monkey data
                         (filter #(> (count %) 1))
                         ;; then parse each monkey into a dictionary
                         (map parse-monkey)
                         vec))

(parse-data real-data-raw)

(defn adjust-worry-part-one [worry]
  (int (/ worry 3)))

(defn adjust-worry-part-two [worry] worry)


(defn get-divisors [data] (map :divisor data))

;; The product of all the divisors
(defn common-mod [data] (reduce * (get-divisors data)))

(common-mod (parse-data test-data-raw))

(defn simulate-turn [monkey-id data adjust-fn]
  ;; inspect and throw all our items
  (loop [current-data data]
    (let [current-monkey-data (nth current-data monkey-id)
          modulo (common-mod data)]
      (if (empty? (current-monkey-data :items))
        current-data
        (let [current-item-worry (first (current-monkey-data :items))
              new-item-worry (adjust-fn ((current-monkey-data :operation) current-item-worry))
              predicate-result ((current-monkey-data :predicate) new-item-worry)
              new-monkey-id ((current-monkey-data :destination) predicate-result)
              ]
          (recur (-> (update-in current-data [monkey-id :inspected] inc)
                     (update-in [monkey-id :items] (partial drop 1))
                     ;; using the product of all the divisors works because the
                     ;; divisors are all "pairwise coprime"
                     (update-in [new-monkey-id :items] #(conj % (mod new-item-worry modulo))))))))))

(defn simulate-round [data adjust-fn]
  (loop [current-data data
         monkeys (map :monkey-id data)]
    (if (empty? monkeys)
      current-data
      (recur (simulate-turn (first monkeys) current-data adjust-fn) (rest monkeys)))))

(defn n-times [n f] (apply comp (repeat n f)))

(defn simulate-rounds [data rounds adjust-fn]
  (let [monkey-business (reverse (sort (map :inspected ((n-times rounds #(simulate-round % adjust-fn)) data))))]
    (* (first monkey-business) (second monkey-business))))

;; part one
(def expected-test-part-one 10605)
(= (simulate-rounds (parse-data test-data-raw) 20 adjust-worry-part-one) expected-test-part-one)
(simulate-rounds (parse-data real-data-raw) 20 adjust-worry-part-one)

;; part two
(def expected-test-part-two 2713310158)
(= (simulate-rounds (parse-data test-data-raw) 10000 adjust-worry-part-two) expected-test-part-two)
(simulate-rounds (parse-data real-data-raw) 10000 adjust-worry-part-two)

;; test the pairwise coprime-ness of our inputs
(defn gcd [a b]
  (.gcd (biginteger a) (biginteger b)))
(gcd 19 3)

(def divisors [19 3 11 17 5 2 13 7])
(def test-divisors [23 19 13 17])

(ns example.core (:require [clojure.math.combinatorics :as combo]))
(defn test-pairwise-coprime [divisors] (every? #(= % 1) (map #(gcd (first %) (second %)) (filter #(not= (first %) (second %)) (combo/selections divisors 2)))))
(test-pairwise-coprime divisors)
(test-pairwise-coprime test-divisors)
(test-pairwise-coprime [10 100])
(test-pairwise-coprime (get-divisors (parse-data test-data-raw)))
(test-pairwise-coprime (get-divisors (parse-data real-data-raw)))

