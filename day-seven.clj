(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def test-data-raw
  ["$ cd /"
   "$ ls"
   "dir a"
   "14848514 b.txt"
   "8504156 c.dat"
   "dir d"
   "$ cd a"
   "$ ls"
   "dir e"
   "29116 f"
   "2557 g"
   "62596 h.lst"
   "$ cd e"
   "$ ls"
   "584 i"
   "$ cd .."
   "$ cd .."
   "$ cd d"
   "$ ls"
   "4060174 j"
   "8033020 d.log"
   "5626152 d.ext"
   "7214296 k"])

(def real-data-raw (str/split-lines (slurp "day-seven.txt")))

;; To begin, find all of the directories with a total size of at most 100000,
;; then calculate the sum of their total sizes.
(def expected-part-one 95437)

;; What shape should I make the directory structure?
(def expected-directory-shape {"/" {"a" {"e" {"i" 584} "f" 29116 "g" 2557 "h.lst" 62596}
                                    "b.txt" 14848514
                                    "c.dat" 8504156
                                    "d" {"j" 4060174 "d.log" 8033020 "d.ext" 5626152 "k" 7214296}}})

;; Command processor

;; if the line starts with $ it's a command
(defn is-command? [line]
  (= \$ (first line)))

;; These kinds of commands
;; "$ cd .."
;; "$ cd d"
;; "$ ls"
;;
;; N.B. I don't handle "cd /" because my test data didn't have it, but I guess
;; I could do this here
(defn apply-command-to-path [command path]
  (let [split-command (str/split command #" ")]
    (case (second split-command)
      "ls" path
      "cd" (if (= (last split-command) "..") (pop path) (conj path (last split-command))))))

;; It's either a directory or a file with a size
;; "dir a"
;; "14848514 b.txt"
(defn apply-file-to-data [file data path]
  (let [split-file-name (str/split file #" ")
        new-file-or-directory-name (last split-file-name)
        current-directory-data (get-in data path)]
    (if (= (first split-file-name) "dir")
      (assoc-in data path (or current-directory-data {new-file-or-directory-name {}}))
      (assoc-in data path (assoc current-directory-data new-file-or-directory-name (Integer/parseInt (first split-file-name)))))
    ))

(defn process-command-reducer [command-processor-data line]
  (let [[data path] command-processor-data]
    (if (is-command? line)
      ;; if it's a command just update the path (or do nothing for ls)
      [data (apply-command-to-path line path)]
      ;; if it's not, then update our data with the file
      [(apply-file-to-data line data path) path])))

(defn process-commands [raw-data]
  ;; we are going to ignore the first cd / because it complicates thing
  ;; so we just assert it's there and then drop it with the "rest"
  ;;
  ;; The initial conditions for the reducer also assume this with "/" as the
  ;; only path entry and the current directory data
  (assert (= (first raw-data) "$ cd /"))
  (let [[data path] (reduce process-command-reducer [{"/" {}} ["/"]] (rest raw-data))]
    data))

(process-commands test-data-raw)

(assert (= (process-commands test-data-raw) expected-directory-shape))

;; Solving

;; we need an or func because we use it with apply
(defn or-fn [& vars]
  (some identity vars))

(defn is-file? [data] (integer? data))

(defn size-for-file [data]
  (if (is-file? data)
    data
    (reduce + (map #(size-for-file (data %)) (keys data)))))

(defn find-path [data path]
  (get-in data path))

;; The total size of directory e is 584 because it contains a single file i of
;; size 584 and no other directories.
(assert (= (size-for-file (find-path expected-directory-shape [ "/" "a" "e" ])) 584))

;; The directory a has total size 94853 because it contains files f (size
;; 29116), g (size 2557), and h.lst (size 62596), plus file i indirectly (a
;; contains e which contains i).
(assert (= (size-for-file (find-path expected-directory-shape [ "/" "a" ])) 94853))

;; Directory d has total size 24933642.
(assert (= (size-for-file (find-path expected-directory-shape [ "/" "d" ])) 24933642))

;; As the outermost directory, / contains every file. Its total size is
;; 48381165, the sum of the size of every file.
(assert (= (size-for-file (find-path expected-directory-shape [ "/" ])) 48381165))

(defn all-paths [data path]
  (assert (not (is-file? data)))
  (let [subdirectories (remove #(is-file? (data %)) (keys data))]
    (apply conj
     (map #(conj path %) subdirectories)
     (mapcat (fn [directory-name] (all-paths (data directory-name) (conj path directory-name))) subdirectories))))

;; part one
(defn size-of-all-paths [data] (map #(size-for-file (find-path data %)) (all-paths data [])))
(defn solve-part-one [data] (reduce + (remove #(> % 100000) (size-of-all-paths data))))

;; with the expected
(assert (= (solve-part-one expected-directory-shape)) expected-part-one)
;; with the processed test data
(assert (= (solve-part-one (process-commands test-data-raw))) expected-part-one)
;; real data
(solve-part-one (process-commands real-data-raw))

;; part two
(def total-space 70000000)
(def space-needed 30000000)
(def expected-part-two 24933642)
(defn total-space-currently-used [data] (size-for-file (find-path data [ "/" ])))

(size-of-all-paths (process-commands test-data-raw))
(total-space-currently-used (process-commands test-data-raw))

;; This is pretty un-readable
(defn solve-part-two [data] (first (remove #(> (- (total-space-currently-used data) %) (- total-space space-needed)) (sort (size-of-all-paths data)))))

;; It's time to remember how threading works.
;;
;; So this one (->) if we our functions take a first parameter. This is better,
;; but isn't great because remove needs the data in the last spot.
(defn solve-part-two [data]
  (first (remove #(> (- (total-space-currently-used data) %) (- total-space space-needed)) (-> data
       (size-of-all-paths)
       (sort)
       )) ))

;; Fortunately there is this one (->>) which sticks the data in the last spot
;; (which works for both our single parameter things and remove)
;;
;; Also, isn't it cool that you can omit the parenthesis on the single parameter
;; functions?
(defn solve-part-two [data]
  (->> data
       size-of-all-paths
       sort
       (remove #(> (- (total-space-currently-used data) %) (- total-space space-needed)))
       first)
  )

(let [split-command (str/split command #" ")]
  (case (second split-command)
  "ls" foo
  "cd" blah))
