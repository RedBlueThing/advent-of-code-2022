(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math.combinatorics :as combo])
(require '[java-time.api :as t])
(require '[loom.graph :as g])
(require '[loom.alg :as alg])
(require '[loom.io :as io])
(require '[clojure.core.memoize :as memo])
(require '[clojure.data.priority-map :as pm])


(defn optimal-path-with-priority-queue [paths]
  (apply max-key :flow paths))

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
     :other-valve-ids (map str/trim (str/split other-valve-ids #","))}))

(defn parse-data [data]
  (reduce (fn [current-map valve]
            (conj current-map [ (:id valve) valve ]))
          {}
          (map parse-line data)))

(defn max-flow-rate [data]
  (apply max (map :flow-rate (vals data))))

(def start-valve-id "AA")

(defn additional-flow? [valves valve-id remaining-minutes]
  (* ((valves valve-id) :flow-rate) (dec remaining-minutes)))

(defn is-open? [valve-state valve-id]
  (= (valve-state valve-id) :open))

(defn key-for-agent-id [agent-index]
  (keyword (format "agent-%d" agent-index)))

(defn agent-paths-for-agent-count [start-valve agent-count]
  (into {} (map (fn [i] [(key-for-agent-id i) [ start-valve ]]) (range 0 agent-count))))

(defn propagate-path [path valves remaining-minutes agent-count]
  (let [agent-paths (path :agent-paths)]
    (loop [agent-index 0
           valve-state (path :valve-state)
           move-paths []
           ]
      (if (= agent-index agent-count)
        (set move-paths)
        (let [
              agent-path (get agent-paths (key-for-agent-id agent-index))
              latest-valve (valves (last agent-path))
              latest-valve-id (latest-valve :id)
              next-valves (latest-valve :other-valve-ids)
              open-current-valve? (or (is-open? valve-state latest-valve-id) (= (latest-valve :flow-rate) 0))
              new-move-paths (map
                              (fn new-path [next-valve]
                                {
                                 :valve-state valve-state
                                 :flow (path :flow)
                                 :agent-paths (assoc agent-paths (key-for-agent-id agent-index) (conj agent-path next-valve))})
                              next-valves)
              ]
          
          (if (not open-current-valve?)
            (let [new-valve-state (assoc valve-state latest-valve-id :open)]
              (recur (inc agent-index)
                     new-valve-state
                     (conj new-move-paths {:valve-state new-valve-state
                                           :flow (+ (path :flow) (additional-flow? valves latest-valve-id remaining-minutes))
                                           :agent-paths (assoc agent-paths (key-for-agent-id agent-index) (conj agent-path latest-valve-id))})))
            (recur (inc agent-index) valve-state new-move-paths))
          )
        ))))

(defn propagate [paths valves remaining-minutes agent-count]
  (reduce set/union #{} (map #(propagate-path % valves remaining-minutes agent-count) paths)))


(defn brute-force-bfs-valve-path [valves start-valve total-minutes agent-count]
  (loop [paths #{ { :flow 0 :agent-paths (agent-paths-for-agent-count start-valve agent-count) :valve-state {} } }
         remaining-minutes total-minutes]
    (if (= remaining-minutes 0)
      (optimal-path-with-priority-queue paths)
      (recur
       (propagate (set (take 100 (sort #(compare (%2 :flow) (%1 :flow)) paths))) valves remaining-minutes agent-count)
       ;; (propagate paths valves remaining-minutes)
       (- remaining-minutes 1)))))

;; (take 5 (sort #(compare (%2 :flow) (%1 :flow)) (brute-force-bfs-valve-path (parse-data test-data-raw) "AA" 4 1)))
;; (set (take 5 (sort #(compare (%2 :flow) (%1 :flow)) paths)))

(defn flow-rate-for-valve [valves valve-id]
  ((valves valve-id) :flow-rate))

(defn edges-for [valves]
  (mapcat (fn edges-for-valve [valve]
         (map
          (fn edge-for-target [target-valve-id] [(valve :id) target-valve-id (flow-rate-for-valve valves target-valve-id) ])
          (valve :other-valve-ids)))
       (vals valves)))

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

(defn render-valves [valves]
  (let [graph (as-> (g/weighted-digraph) new-graph
                (apply g/add-nodes new-graph (keys valves))
                (apply g/add-edges new-graph (edges-for valves)))]
    ;; Render the graph using Graphviz
    (io/view (update-edge-attributes graph) {:title "Pipes"})))

(defn render-valves [valves]
  (let [graph (as-> (g/weighted-digraph) new-graph
                (apply g/add-nodes new-graph (keys valves))
                (apply g/add-edges new-graph (edges-for valves)))]
    ;; Render the graph using Graphviz
    (io/view (update-edge-attributes graph) {:title "Pipes"})))


;; Call the render-digraph function to generate the graph
(render-valves (parse-data test-data-raw))
(render-valves (parse-data real-data-raw))

;; Part one
(brute-force-bfs-valve-path (parse-data test-data-raw) "AA" 30 1)
