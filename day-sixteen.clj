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
  ["Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
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
            (conj current-map [(:id valve) valve]))
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
  (into {} (map (fn [i] [(key-for-agent-id i) [start-valve]]) (range 0 agent-count))))

(defn get-all-move-paths [path valves])

(defn propagate-scenarios-for-active-agent [scenario valves remaining-minutes agent-index]
  (let [;; Grab the valve state and all the agent pats
        agent-paths (scenario :agent-paths)
        valve-state-and-flow-state (scenario :valve-state)

        ;; grab the current flow which will be basis for any choices to open valves
        flow (valve-state-and-flow-state :flow)
        ;; get the key for this agent and the path we have taken up to this point.
        agent-key (key-for-agent-id agent-index)
        agent-path (get agent-paths agent-key)

        ;; the valve we are currently at
        latest-valve (valves (last agent-path))
        ;; and its id
        latest-valve-id (latest-valve :id)

        ;; what new vavles can we move to?
        next-valves (latest-valve :other-valve-ids)
        ;; is the current valve open already? (if it is we only need to move, not decide to open it)
        open-current-valve? (or (is-open? valve-state-and-flow-state latest-valve-id) (= (latest-valve :flow-rate) 0))
        ;; grab all the scenarios where this agent moves to a new valve
        new-move-scenarios (map
                            (fn new-move-scenario [next-valve]
                              {:valve-state valve-state-and-flow-state
                               :flow flow
                               :agent-paths (assoc agent-paths agent-key (conj agent-path next-valve))})
                            next-valves)]
    ;; if the current valve isn't open, we need to open it and add a new scenario for that.
    (if (not open-current-valve?)
      (let [new-valve-state (assoc (assoc valve-state-and-flow-state latest-valve-id :open) :flow (+ flow (additional-flow? valves latest-valve-id remaining-minutes)))]
        ;; so we just return the new-move-scenarios plus the one with the open valvue
        (conj new-move-scenarios {:valve-state new-valve-state
                                  :flow (new-valve-state :flow)
                                  :agent-paths (assoc agent-paths agent-key (conj agent-path latest-valve-id))}))
      ;; otherwise just the new-move-scenarios unchanged
      new-move-scenarios)))

(defn propagate-scenarios [scenario valves remaining-minutes agent-count]
  ;; for this scenario, which is shaped like:
  ;; { :agent-paths {:agent-0 [], :agent-1 []}, :valve-state { <valve-id> :open, :flow <current-flow-for-this-scenario}}
  ;; where each agent path is the list of valves visited.
  ;;
  ;; We need to in order generate all possible scenarios for each agent in
  ;; order. As we examine each agent we need to propagate all scenarios for
  ;; previously generated scenarios (for prior agents). We will acumlate
  ;; scenarios in the current-scenario-set which we will return

  (loop [agent-index 0
         current-scenario-set #{scenario}]
    (if (= agent-index agent-count)
      ;; we are done, return the current scenarios
      current-scenario-set
      ;; otherwise we want to propagate scenarios for each scenario for the
      ;; current agent-index
      (recur (inc agent-index)
             (reduce set/union #{} (map #(propagate-scenarios-for-active-agent % valves remaining-minutes agent-index) current-scenario-set))))))

(defn propagate [scenario-set valves remaining-minutes agent-count]
  (reduce set/union #{} (map #(propagate-scenarios % valves remaining-minutes agent-count) scenario-set)))

(defn brute-force-bfs-valve-path [valves start-valve total-minutes agent-count]
  (loop [scenario-set #{{:agent-paths (agent-paths-for-agent-count start-valve agent-count) :valve-state {:flow 0}}}
         remaining-minutes total-minutes]
    (if (= remaining-minutes 0)
      (optimal-path-with-priority-queue scenario-set)
      (recur
       ;; top solutions sorted by flow
       (propagate (set (take 2000 (sort #(compare (get-in %2 [:valve-state :flow]) (get-in %1 [:valve-state :flow])) scenario-set))) valves remaining-minutes agent-count)
       ;; brute force
       ;; (propagate paths valves remaining-minutes agent-count)
       (- remaining-minutes 1)))))

;; (take 5 (sort #(compare (%2 :flow) (%1 :flow)) (brute-force-bfs-valve-path (parse-data test-data-raw) "AA" 4 1)))
;; (set (take 5 (sort #(compare (%2 :flow) (%1 :flow)) paths)))

(defn flow-rate-for-valve [valves valve-id]
  ((valves valve-id) :flow-rate))

(defn edges-for [valves]
  (mapcat (fn edges-for-valve [valve]
            (map
             (fn edge-for-target [target-valve-id] [(valve :id) target-valve-id (flow-rate-for-valve valves target-valve-id)])
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


