(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math.combinatorics :as combo])
(require '[java-time.api :as t])
(require '[loom.graph :as g])
(require '[loom.alg :as alg])
(require '[loom.io :as io])

(def test-data-raw
  [
   "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
   "Valve BB has flow rate=13; tunnels lead to valves CC, AA"
   "Valve CC has flow rate=2; tunnels lead to valves DD, BB"
   "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE"
   "Valve EE has flow rate=3; tunnels lead to valves FF, DD"
   "Valve FF has flow rate=0; tunnels lead to valves EE, GG"
   "Valve GG has flow rate=0; tunnels lead to valves FF, HH"
   "Valve HH has flow rate=22; tunnel leads to valve GG"
   "Valve II has flow rate=0; tunnels lead to valves AA, JJ"
   "Valve JJ has flow rate=21; tunnel leads to valve II"])

(def real-data-raw (str/split-lines (slurp "day-sixteen.txt")))

(defn parse-line [line]
  (let [[valve-id flow-rate-string other-valve-ids] (rest (re-matches #"Valve (..) has flow rate=(\d+); tunnels? leads? to valves? (.*)" line))]
    {:id valve-id
     :flow-rate (Integer/parseInt flow-rate-string)
     :other-valve-ids (map str/trim (str/split other-valve-ids #","))
     :cost Integer/MAX_VALUE
     :path []}))

(defn parse-data [data]
  (reduce (fn [current-map valve]
            (conj current-map [ (:id valve) valve ]))
          {}
          (map parse-line data)))

(parse-data test-data-raw)

(defn max-flow-rate [data]
  (apply max (map :flow-rate (vals data))))

(def start-valve-id "AA")

(defn total-flow-for-valve [remaining-minutes valve]
  (* remaining-minutes (:flow-rate valve)))

(defn cost-for-valve [remaining-minutes valve]
  (- Integer/MAX_VALUE (total-flow-for-valve remaining-minutes valve)))

(defn update-cost-flow-and-minutes [valves valve-id cost flow minutes current]
  (if (<= cost ((valves valve-id) :cost))
    (-> valves
        (assoc-in [valve-id :cost] cost)
        (assoc-in [valve-id :flow] flow)
        (assoc-in [valve-id :minutes] minutes)
        ;; if we are updating the cost and the minutes, then set the from current
        (assoc-in [valve-id :path] (conj ((valves current) :path) valve-id))
        )
    valves))

(defn update-costs [valves valve-ids current remaining-minutes]
  (reduce (fn [valves valve-id]
            (update-cost-flow-and-minutes
             valves
             valve-id
             (cost-for-valve remaining-minutes (valves valve-id))
             (total-flow-for-valve remaining-minutes (valves valve-id))
             remaining-minutes current))
          valves valve-ids))

(defn next-to-visit [valves unvisited]
  (-> (filter #(some? ((valves %) :minutes)) unvisited)
      ((fn sorted-ids [unvisited-ids] (sort #(compare (first %1) (first %2)) (map (fn [valve-id] (let [valve (valves valve-id)]
                                                                              [(valve :cost) (valve :id)])) unvisited-ids))))
      ;; the  lowest of the sorted costs
      first
      ;; the id for that entry (created by sorted-ids)
      second
      ))

(defn optimal-valve-path [valves start-valve total-minutes]
  (loop [current start-valve
         current-valves (update-cost-flow-and-minutes valves current 0 0 30 start-valve)
         ;; how many minutes when we are about to work out where to go next
         remaining-minutes (- total-minutes 2)
         ;; we start with unvisited valves
         unvisited (disj (set (keys valves)) current)]
    (if (or (<= remaining-minutes 0) (nil? current))
      ;; we found our destination, just return the data (and the costs).
      current-valves
      ;; we are still looking, update our reachable neighbours costs based on remaining time
      (let [
            valves-with-updated-costs (update-costs current-valves ((current-valves current) :other-valve-ids) current remaining-minutes)
            new-current (next-to-visit valves-with-updated-costs unvisited)]
        (recur ;; new current, new valves, new unvisited
         new-current
         valves-with-updated-costs
         (or (and new-current (- ((valves-with-updated-costs new-current) :minutes) 2)) 0)
         (disj unvisited new-current))))))


(defn additional-flow? [path next-valve valves remaining-minutes]
  (if (some #(= next-valve %) (path :path))
    0
    (* ((valves next-valve) :flow-rate) remaining-minutes)))

(defn propagate-path [path valves remaining-minutes]
  (let [
        latest-valve (valves (last (path :path)))
        second-to-last-valve (last (butlast (path :path)))
        ;; we can filter the other valves to make sure the second to last valve
        ;; isn't one (so we don't run in circles).
        ;; next-valves (filter #(not= second-to-last-valve %) (latest-valve :other-valve-ids))
        next-valves (latest-valve :other-valve-ids)
        ]
    (set (map
          (fn new-path [next-valve]
            {
             :flow (+ (path :flow) (additional-flow? path next-valve valves remaining-minutes))
             ;; :flow 0
             :path (conj (path :path) next-valve)})
          next-valves))
    )
  )

(defn propagate [paths valves remaining-minutes]
  (reduce set/union #{} (map #(propagate-path % valves remaining-minutes) paths)))

(defn brute-force-bfs-valve-path [valves start-valve total-minutes]
  (loop [paths #{ { :flow 0 :path [start-valve] } }
         remaining-minutes (- total-minutes 2)]
    (if (= remaining-minutes 0)
      paths
      (recur
       (propagate paths valves remaining-minutes)
       (- remaining-minutes 2)))))

(defn flow-rate-for-valve [valves valve-id]
  ((valves valve-id) :flow-rate))

(defn edges-for [valves]
  (mapcat (fn edges-for-valve [valve]
         (map
          (fn edge-for-target [target-valve-id] [(valve :id) target-valve-id (flow-rate-for-valve valves target-valve-id) ])
          (valve :other-valve-ids)))
       (vals valves)))

(defn render-valves [valves]
  (let [graph (as-> (g/weighted-digraph) new-graph
                (apply g/add-nodes new-graph (keys valves))
                (apply g/add-edges new-graph (edges-for valves)))]
    ;; Render the graph using Graphviz
    (io/view (update-edge-attributes graph) {:title "Pipes"})))

(defn update-edge-attributes [graph]
  (reduce #(let [weight (g/weight graph %2)]
             (loom.attr/add-attr %1
                                 %2
                                 :label
                                 (str "  [" (cond (vector? weight)
                                                  (clojure.string/join ",\n" weight)
                                                  :else weight) "]")))
          graph
          (g/edges graph)))

;; Call the render-digraph function to generate the graph
(render-valves (parse-data test-data-raw))
(render-valves (parse-data real-data-raw))
;; (brute-force-bfs-valve-path (parse-data test-data-raw) "AA" 30)


;; (sort #(compare (%1 :flow) (%2 :flow)) [{:flow 1} {:flow 2} {:flow 3}])
;; (sort #(compare (%1 :flow) (%2 :flow)) (vector (brute-force-bfs-valve-path (parse-data test-data-raw) "AA" 30)))


