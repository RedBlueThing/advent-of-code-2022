(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round]])

(def test-data-raw ["root: pppw + sjmn"
                    "dbpl: 5"
                    "cczh: sllz + lgvd"
                    "zczc: 2"
                    "ptdq: humn - dvpt"
                    "dvpt: 3"
                    "lfqf: 4"
                    "humn: 5"
                    "ljgn: 2"
                    "sjmn: drzm * dbpl"
                    "sllz: 4"
                    "pppw: cczh / lfqf"
                    "lgvd: ljgn * ptdq"
                    "drzm: hmdt - zczc"
                    "hmdt: 32"])

(def real-data-raw (str/split-lines (slurp "day-twenty-one.txt")))

(defn parse-line [line]
  (let [[variable-name expression] (str/split line #": ")
        expression-parts (str/split expression #" ")
        ]
    (if (= (count expression-parts) 3)
      ;; "drzm: hmdt - zczc"
      (let [[first-param operation second-param] expression-parts]
        [(keyword variable-name) {
                                  :type :expression
                                  :operation (eval (read-string operation))
                                  :first-param (keyword first-param)
                                  :second-param (keyword second-param)
                                  }])
      ;; "hmdt: 32"
      [(keyword variable-name) {
                                :type :value
                                :value (Integer/parseInt expression)
                                }]

      )
    )
  )

(defn parse-data [raw-data]
  (into {} (map parse-line raw-data)))

(defn evaluate-variable [variable data]
  (let [data-for-variable (data variable)
        type-for-variable (data-for-variable :type)]
    (case type-for-variable
      :expression (let [operation (data-for-variable :operation)
                        first-param (data-for-variable :first-param)
                        second-param (data-for-variable :second-param)
                        first-param-value (evaluate-variable first-param data)
                        second-param-value (evaluate-variable second-param data)
                        ]
                    (if (and (integer? first-param-value) (integer? second-param-value))
                      (operation first-param-value second-param-value)
                      data-for-variable))
      :value (data-for-variable :value)
      )
    )
  )

(defn variables-for-part-two [data]
  ;; what are the two values we care about (the variables that :root refers to)
  (map (fn [keyword] ((data :root) keyword)) [:first-param :second-param]))

(defn update-data-for-human [data human-value]
  (assoc data :humn {:type :value :value human-value}))

(def previous-start 3509819803064)

(defn solution-part-two [data]
  ;; brute force? yeah probably not
  (let [[first-param second-param] (variables-for-part-two data)]
    (loop [i previous-start]
      (let [current-data (update-data-for-human data i)
            first-param-value (evaluate-variable first-param current-data)
            second-param-value (evaluate-variable second-param current-data)]
        (if (or
             (> i 100000000000000)
             (= first-param-value second-param-value))
          (println "Found it or not" i)
          (recur (inc i)))))))

(defn solve-all-possible [data]
  (let [[first-param second-param] (variables-for-part-two data)
        current-data (update-data-for-human data {:type :value :value :yell})]
    (reduce (fn [reduce-data variable] (assoc reduce-data variable (let [result (evaluate-variable variable reduce-data)]
                                                                     (if (integer? result)
                                                                       {:type :value, :value result}
                                                                       result
                                                                       ))))
            current-data (keys current-data))))



(def operation-map {
                    (str /) "/"
                    (str +) "+"
                    (str -) "-"
                    (str *) "*"
                    })

(defn render [variable data]
  (let [data-for-variable (data variable)
        type-for-variable (data-for-variable :type)]
    (case type-for-variable
      :expression (let [operation (data-for-variable :operation)
                        first-param (data-for-variable :first-param)
                        second-param (data-for-variable :second-param)
                        first-param-value (render first-param data)
                        second-param-value (render second-param data)
                        ]
                    (format "(%s %s %s)" first-param-value (operation-map (str operation)) second-param-value))
      :value (str (data-for-variable :value))
      )
    )
  )

;; Render gives us this equation to solve:

;; ((702 + (3 x (73621039497353 - (((2 x (372 + (((((52 x (((667 + ((((((2 x (122 + ((((8 + (2 x (((498 + (((2 x (((10 + (32 x (((((673 + (9 x (((((467 + ((((2 x (364 + ((((((((477 + (47 x (65 + ((yell - 273) / 2)))) x 2) - 376) / 8) + 533) / 4) - 699) x 12))) - 625) + 67) / 2)) / 10) - 828) x 2) - 712))) / 7) + 242) / 9) - 724))) / 2) - 23)) - 228) / 3)) x 3) - 150))) / 4) - 275) / 9))) - 116) x 3) + 314) / 2) + 427)) / 4) - 704)) + 424) x 2) - 988) / 4))) - 423) / 3)))) / 2) = 49160133593649

;; I used sympy to do that ------>

;; import sympy as sp
;; yell = sp.symbols('yell')
;; equation = ((702 + (3 * (73621039497353 - (((2 * (372 + (((((52 * (((667 + ((((((2 * (122 + ((((8 + (2 * (((498 + (((2 * (((10 + (32 * (((((673 + (9 * (((((467 + ((((2 * (364 + ((((((((477 + (47 * (65 + ((yell - 273) / 2)))) * 2) - 376) / 8) + 533) / 4) - 699) * 12))) - 625) + 67) / 2)) / 10) - 828) * 2) - 712))) / 7) + 242) / 9) - 724))) / 2) - 23)) - 228) / 3)) * 3) - 150))) / 4) - 275) / 9))) - 116) * 3) + 314) / 2) + 427)) / 4) - 704)) + 424) * 2) - 988) / 4))) - 423) / 3)))) / 2) - 49160133593649
;; solution = sp.solve(equation, yell)
;; print('Solution for :yell:', solution)

