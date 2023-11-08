(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def test-data-raw ["1=-0-2"
                    "12111"
                    "2=0="
                    "21"
                    "2=01"
                    "111"
                    "20012"
                    "112"
                    "1=-1="
                    "1-12"
                    "12"
                    "1="
                    "122"])

(def real-data-raw (str/split-lines (slurp "day-twenty-five.txt")))

(def snafu-digit-dictionary {
                             \2 2
                             \1 1
                             \0 0
                             \- -1
                             \= -2
                             })

(defn convert-snafu-digits [string-to-convert]
  (map (fn [character] (snafu-digit-dictionary character)) (char-array string-to-convert)))

(defn place-to-multiple [place base]
  (if (= place 0)
    1
    (pow base place)))

(defn snafu-to-number [string-to-convert]
  (let [digits (convert-snafu-digits string-to-convert)
        digit-count (count digits)]
    (reduce + (reduce (fn digit-reducer [multiplied-digits [place digit]]
                        (conj multiplied-digits (* (place-to-multiple place 5) digit))
                        ) [] (map-indexed (fn [index value] [(- digit-count (inc index)) value]) digits)))))

(defn number-to-snafu [number]
  (if (= number 0) "0" 
      (loop [n number
             result []]
        (let [offset-n (+ n 2)
              remainder (mod offset-n 5)
              quotient (quot offset-n 5)]
          (if (<= n 0)
            (clojure.string/join (reverse result))
            (recur quotient (conj result ([\= \- \0 \1 \2] (int remainder)))))))))

;; Decimal          SNAFU
;; 0              0
;; 1              1
;; 2              2

;; 3             1=
;; 4             1-
;; 5             10
;; 6             11
;; 7             12

;; 8             2=
;; 9             2-
;; 10            20
;; 11            21
;; 12            22

;; 2022         1=11-2
;; 12345        1-0---0
;; 314159265  1121-1110-1=0
