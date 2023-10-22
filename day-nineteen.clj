(require '[clojure.string :as str])
(require '[clojure.set :as set])

;; Blueprint definitions

(def test-data-raw ["Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian."
"Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."])

(def real-data-raw (str/split-lines (slurp "day-nineteen.txt")))

(defn key-for-parse-index [index]
  ({0 :blueprint-id
    1 :ore-robot-ore-cost
    2 :clay-robot-ore-cost
    3 :obsidian-robot-ore-cost
    4 :obsidian-robot-clay-cost
    5 :geode-robot-ore-cost
    6 :geode-robot-obsidian-cost} index))

(def robot-types [:ore-robots :clay-robots :obsidian-robots :geode-robots])
(def resource-types [:ore :clay :obsidian :geode])

(defn parse-blueprint-definition [line]
  (let [parsed-data (-> (re-matches #"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian." line)
                        rest)]
    (into {} (map-indexed (fn [i value] [ (key-for-parse-index i) (Integer/parseInt value) ]) parsed-data))))

(defn parse-data [data]
  (map parse-blueprint-definition data))

(defn sufficient-resource [resource cost]
  (or (= resource :infinite) (>= resource cost)))

(defn spend-resource [resource cost]
  (if (= resource :infinite)
    :infinite
    (- resource cost)))

(defn maybe-build-geode-robot [blueprint state]
  (let [{robots :robots resources :resources} state
        {ore :ore
         obsidian :obsidian} resources
        {geode-robot-ore-cost :geode-robot-ore-cost
         geode-robot-obsidian-cost :geode-robot-obsidian-cost} blueprint
        {geode-robots :geode-robots} robots]
    (if (and (sufficient-resource ore geode-robot-ore-cost)
             (sufficient-resource obsidian geode-robot-obsidian-cost))
      ;; we have enough for a geode robot, then spend the resources and update the robots
      (-> state
          (assoc-in [:resources :ore] (spend-resource ore geode-robot-ore-cost))
          (assoc-in [:resources :obsidian] (spend-resource obsidian geode-robot-obsidian-cost))
          (assoc-in [:pending-robots :geode-robots] (inc geode-robots)))
      ;; nope, not enough, just return what we have
      state)))

(defn maybe-build-obsidian-robot [blueprint state]
  (let [{robots :robots resources :resources} state
        {ore :ore
         clay :clay} resources
        {obsidian-robot-ore-cost :obsidian-robot-ore-cost
         obsidian-robot-clay-cost :obsidian-robot-clay-cost} blueprint
        {obsidian-robots :obsidian-robots} robots]
    (if (and (sufficient-resource ore obsidian-robot-ore-cost)
             (sufficient-resource clay obsidian-robot-clay-cost))
      ;; we have enough for a obsidian robot, then spend the resources and update the robots
      (-> state
          (assoc-in [:resources :clay] (spend-resource clay obsidian-robot-clay-cost))
          (assoc-in [:resources :ore] (spend-resource ore obsidian-robot-ore-cost))
          (assoc-in [:pending-robots :obsidian-robots] (inc obsidian-robots)))
      ;; nope, not enough, just return what we have
      state)))

(defn maybe-build-clay-robot [blueprint state]
  (let [{robots :robots resources :resources} state
        {ore :ore} resources
        {clay-robot-ore-cost :clay-robot-ore-cost} blueprint
        {clay-robots :clay-robots} robots]
    (if (sufficient-resource ore clay-robot-ore-cost)
      ;; we have enough for a clay robot, then spend the resources and update the robots
      (-> state
          (assoc-in [:resources :ore] (spend-resource ore clay-robot-ore-cost))
          (assoc-in [:pending-robots :clay-robots] (inc clay-robots)))
      ;; nope, not enough, just return what we have
      state)))

(defn maybe-build-ore-robot [blueprint state]
  (let [{robots :robots resources :resources} state
        {ore :ore} resources
        {ore-robot-ore-cost :ore-robot-ore-cost} blueprint
        {ore-robots :ore-robots} robots]
    (if (sufficient-resource ore ore-robot-ore-cost)
      ;; we have enough for an ore robot, then spend the resources and update the robots
      (-> state
          (assoc-in [:resources :ore] (spend-resource ore ore-robot-ore-cost))
          (assoc-in [:pending-robots :ore-robots] (inc ore-robots)))
      ;; nope, not enough, just return what we have
      state)))

(defn increment-resource [current robots max-cost remaining-rounds]
  (if (= current :infinite)
    :infinite
    (let [new-resource-amount (+ current robots)]
      ;; if the new resource amount is more than the remaining rounds multiplied
      ;; by the maximum cost to build any robot using this resource then this
      ;; resource may as well be infinite for the purposes of this simulation.
      (if (> new-resource-amount (* remaining-rounds max-cost))
        :infinite
        new-resource-amount))))

(defn update-resources [round rounds blueprint state]
  ;; All we need to do is add resources for each robot we have
  (let [{robots :robots pending-robots :pending-robots resources :resources} state
        {ore-robots :ore-robots
         clay-robots :clay-robots
         obsidian-robots :obsidian-robots
         geode-robots :geode-robots} robots
        {ore-robot-ore-cost :ore-robot-ore-cost
         clay-robot-ore-cost :clay-robot-ore-cost
         obsidian-robot-ore-cost :obsidian-robot-ore-cost
         obsidian-robot-clay-cost :obsidian-robot-clay-cost
         geode-robot-ore-cost :geode-robot-ore-cost
         geode-robot-obsidian-cost :geode-robot-obsidian-cost} blueprint
        {ore :ore
         clay :clay
         obsidian :obsidian
         geode :geode} resources
        max-ore-cost (max geode-robot-ore-cost obsidian-robot-ore-cost clay-robot-ore-cost ore-robot-ore-cost)
        max-obsidian-cost geode-robot-obsidian-cost
        max-clay-cost obsidian-robot-clay-cost
        remaining-rounds (- rounds round)]
    {
     :robots robots
     :pending-robots pending-robots
     :resources {:ore (increment-resource ore ore-robots max-ore-cost remaining-rounds)
                 :clay (increment-resource clay clay-robots max-clay-cost remaining-rounds)
                 :obsidian (increment-resource obsidian obsidian-robots max-obsidian-cost remaining-rounds)
                 :geode (+ geode geode-robots)} }))

(defn maybe-build-robots [blueprint state ]
  ;; A flawed heuristic approach that eventually finds geodes, but isn't optimal
  (let [
        {robots :robots pending-robots :pending-robots resources :resources} state
        {ore-robots :ore-robots
         clay-robots :clay-robots
         obsidian-robots :obsidian-robots
         geode-robots :geode-robots} robots
        {geode-robot-ore-cost :geode-robot-ore-cost
         geode-robot-obsidian-cost :geode-robot-obsidian-cost
         obsidian-robot-ore-cost :obsidian-robot-ore-cost
         obsidian-robot-clay-cost :obsidian-robot-clay-cost} blueprint
        clay-to-ore-cost-ratio (/ obsidian-robot-clay-cost obsidian-robot-ore-cost)
        obsidian-to-ore-cost-ratio (/ geode-robot-obsidian-cost geode-robot-ore-cost)
        clay-to-ore-robot-ratio (/ clay-robots ore-robots)
        obsidian-to-ore-robot-ratio (/ obsidian-robots ore-robots)
        ]

    ;; if don't have enough clay robots, always build clay robots
    (if (< clay-to-ore-robot-ratio clay-to-ore-cost-ratio)
      (maybe-build-clay-robot blueprint state)
      ;; similary if we don't have enough obsidian robots, build those
      (if (< obsidian-to-ore-robot-ratio obsidian-to-ore-cost-ratio)
        (maybe-build-obsidian-robot blueprint state)
        (maybe-build-geode-robot blueprint state)))))

(defn finish-building-robots [blueprint state]
  ;; the pending-robots from maybe-build-robots becomes robots
  (let [{pending-robots :pending-robots robots :robots resources :resources} state]
    {:robots (merge robots pending-robots) :resources resources}))

(defn simulate-round [round rounds blueprint state build-fn]
  ;; Simulate round building a particular robot type
  (let [{pre-build-robots :robots resources :resources} state]
    (->> state
         (build-fn blueprint)
         (update-resources round rounds blueprint)
         (finish-building-robots blueprint))))

(def starting-robots {:ore-robots 1
                      :clay-robots 0
                      :obsidian-robots 0
                      :geode-robots 0})

(def starting-resources {:ore 0
                         :clay 0
                         :obsidian 0
                         :geode 0})

(def starting-state {:robots starting-robots :resources starting-resources})


(def debug false)

(defn >=resource? [resource other-resource]
  ;; is the resource greater than or equal to other-resource?

  ;; first is the other-resource infinite?
  (if (= other-resource :infinite)
    ;; only true if the resource is also infinite
    (= resource :infinite)
    ;; otherwise is the other-resource >= resource or is other-resource infinite?
    (or (= resource :infinite) (>= resource other-resource))))

(defn >=resources? [state other-state]
  ;; True if state has >= resources than other-state (and the same robots)
  (let [resources (:resources state)
        other-resources (:resources other-state)]
    (and (not= other-state state)
         (= (:robots other-state) (:robots state))
         (>=resource? (:ore resources) (:ore other-resources))
         (>=resource? (:clay resources) (:clay other-resources))
         (>=resource? (:obsidian resources) (:obsidian other-resources))
         (>=resource? (:geode resources) (:geode other-resources)))))

(assert (>=resources? {:robots {} :resources {:ore 19, :clay 12, :obsidian 16, :geode 0}}
                      {:robots {} :resources {:ore 18, :clay 12, :obsidian 16, :geode 0}}))
(assert (>=resources? {:robots {} :resources {:ore :infinite, :clay 12, :obsidian 16, :geode 0}}
                      {:robots {} :resources {:ore 18, :clay 12, :obsidian 16, :geode 0}}))
(assert (>=resources? {:robots {} :resources {:ore :infinite, :clay 15, :obsidian 16, :geode 0}}
                      {:robots {} :resources {:ore :infinite, :clay 13, :obsidian 16, :geode 0}}))

(defn maybe-prune-lagging-states [state round rounds pruning-data]
  (let [other-states-with-matching-robots ((pruning-data :matching-robot-dictionary) (state :robots))
        other-states-with-matching-robots-and-more-resources (filter (fn [other-state] (>=resources? other-state state)) other-states-with-matching-robots)]
    ;; if other state exists with the same robots and more resources then this state can be pruned
    (if (not-empty other-states-with-matching-robots-and-more-resources)
      nil
      state)))

(defn maybe-prune-losing-by-geode-robot-states [state round rounds pruning-data]
  (if (or
       (nil? state)
       (> (-> state :robots :geode-robots) 0))
    ;; if this state has geode-robots, then it's viable
    state
    ;; if it doesn't, let's check for any other states that do. If one exists, then we'll drop this state
    (let [other-states-with-geode-robots (pruning-data :states-with-geode-robots)]
      (if (not-empty other-states-with-geode-robots)
        nil
        state))))

(defn maybe-prune-geode-robots-every-round-states [state round rounds pruning-data]
  ;; Once we have infinite ore and obsidian we can make a geode robot every
  ;; round so the state with the most geodes always wins
  (if (nil? state)
    ;; If this one is nil it's already been pruned
    state
    ;; do we have infinite ore and obsidian
    (let [best-geode-robots-for-infinite-resource-states (pruning-data :best-geode-robots-for-infinite-resource-states)
          other-states-with-geode-robots (pruning-data :states-with-geode-robots)
          {robots :robots resources :resources} state
          geode-robots (robots :geode-robots)
          {
           ;; the resources that we use to build geodes
           ore :ore
           obsidian :obsidian} resources]
      (if (and (= ore :infinite)
               (= obsidian :infinite))
        (if (> geode-robots best-geode-robots-for-infinite-resource-states)
          state
          nil)
        state))
    )
  )

(defn maybe-prune-state [round rounds state pruning-data]
  (-> state
      (maybe-prune-lagging-states round rounds pruning-data)
      (maybe-prune-losing-by-geode-robot-states round rounds pruning-data)
      (maybe-prune-geode-robots-every-round-states round rounds pruning-data)))

(defn maybe-prune-states [round rounds states]
  ;; Prune the set of remaing states to trim down the problem space
  (let [states-with-geode-robots (filter (fn [other-state] (> (-> other-state :robots :geode-robots) 0)) states)
        geode-robots-for-states-with-infinite-resources (remove nil? (map (fn [other-state]
                                                                      (let [{robots :robots resources :resources} other-state
                                                                            geode-robots (robots :geode-robots)
                                                                            {
                                                                             ;; the resources that we use to build geodes
                                                                             ore :ore
                                                                             obsidian :obsidian} resources]
                                                                        (if (and (= ore :infinite)
                                                                                 (= obsidian :infinite))
                                                                          geode-robots
                                                                          nil))) states))
        best-geode-robots-for-infinite-resource-states (if (empty? geode-robots-for-states-with-infinite-resources)
                                                       nil
                                                       (apply max geode-robots-for-states-with-infinite-resources))
        matching-robot-dictionary (reduce (fn matching-robot-reducer [dict next-state] (let [robots (next-state :robots)
                                                                                             current-states (or (dict robots) [])]
                                                                                         (assoc dict robots (conj current-states next-state)))) {} states)
        pruning-data {:states-with-geode-robots states-with-geode-robots
                      :best-geode-robots-for-infinite-resource-states best-geode-robots-for-infinite-resource-states
                      :matching-robot-dictionary matching-robot-dictionary
                      }]
      (set (remove nil? (map (fn [state] (maybe-prune-state round rounds state pruning-data)) states)))))

(defn build-function-for-resource [resource build-function]
  (if (= resource :infinite)
    nil
    build-function))


(def build-functions [maybe-build-clay-robot
                      maybe-build-ore-robot
                      maybe-build-obsidian-robot
                      maybe-build-geode-robot])

(def ore-type-for-function {maybe-build-clay-robot :clay
                            maybe-build-ore-robot :ore
                            maybe-build-obsidian-robot :obsidian
                            maybe-build-geode-robot :geode
                            })

(defn filter-build-functions-for-state [state blueprint build-functions]
  ;; if we have ore of a particular type than the remaining rounds then we can remove that robot type.
  (let [resources (:resources state)]
    (remove nil? (map (fn [build-function] (build-function-for-resource (resources (ore-type-for-function build-function)) build-function)) build-functions))))

(defn simulate-rounds [blueprint starting-state rounds]
  ;; Brute force all the things
  (println "Blueprint" (blueprint :blueprint-id))
  (loop [i 0
         states (set [starting-state])]
    (println "Rounds" (str i))
    (if (= i rounds)
      states
      (let [new-states (apply set/union (map (fn set-of-new-states-for-functions [previous-state] (set (map (fn[build-fn] (simulate-round i rounds blueprint previous-state build-fn)) (filter-build-functions-for-state previous-state blueprint build-functions)))) states))
            pruned-new-states (maybe-prune-states i rounds new-states)]
        (if debug
          (doseq [state pruned-new-states]
            (println state)))
        (recur (inc i) pruned-new-states)))))

(defn compare-geode-states-> [first-state second-state]
  (let [
        {first-resources :resources} first-state
        {second-resources :resources} second-state
        {first-geode :geode} first-resources
        {second-geode :geode} second-resources]
    (> first-geode second-geode)))

(defn state-with-most-geodes [states]
  (let [sorted-states (apply (partial sorted-set-by compare-geode-states->) states)]
    (first sorted-states)))

(assert (= (-> (state-with-most-geodes #{{:resources {:ore 5, :clay 0, :obsidian 0, :geode 0}} {:resources {:ore 7, :clay 0, :obsidian 0, :geode 5}} {:resources {:ore 7, :clay 0, :obsidian 0, :geode 6}} {:resources {:ore 7, :clay 0, :obsidian 0, :geode 9}}}) :resources :geode) 9))

(defn quality-level [blueprint state]
  (let [{resources :resources} state
        {geode :geode} resources]
    (* (:blueprint-id blueprint) geode)))

(defn solution-part-one [data rounds]
  (let [quality-levels (into {} (map (fn [blueprint] { blueprint (quality-level blueprint (state-with-most-geodes (simulate-rounds blueprint starting-state rounds))) }) data))]
    (reduce + (vals quality-levels))))

(defn solution-part-two [data rounds]
  (let [states-with-most-geodes (into {} (map (fn [blueprint] { blueprint (state-with-most-geodes (simulate-rounds blueprint starting-state rounds)) }) (take 3 data)))]
    (println states-with-most-geodes)
    (apply * (map (fn[state] (-> state :resources :geode)) (vals states-with-most-geodes)))))

