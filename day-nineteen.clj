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
    6 :geode-robot-obisidian-cost} index))

(def robot-types [:ore-robots :clay-robots :obsidian-robots :geode-robots])
(def resource-types [:ore :clay :obsidian :geode])

(defn parse-blueprint-definition [line]
  (let [parsed-data (-> (re-matches #"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian." line)
                        rest)]
    (into {} (map-indexed (fn [i value] [ (key-for-parse-index i) (Integer/parseInt value) ]) parsed-data))))

(defn parse-data [data]
  (map parse-blueprint-definition data))

(defn maybe-build-geode-robot [blueprint state]
  (let [{robots :robots resources :resources} state
        {ore :ore
         obsidian :obsidian} resources
        {geode-robot-ore-cost :geode-robot-ore-cost
         geode-robot-obisidian-cost :geode-robot-obisidian-cost} blueprint
        {geode-robots :geode-robots} robots]
      (if (and (>= ore geode-robot-ore-cost)
             (>= obsidian geode-robot-obisidian-cost))
        ;; we have enough for a geode robot, then spend the resources and update the robots
        (-> state
            (assoc-in [:resources :ore] (- ore geode-robot-ore-cost))
            (assoc-in [:resources :obsidian] (- obsidian geode-robot-obisidian-cost))
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
    (if (and (>= ore obsidian-robot-ore-cost)
             (>= clay obsidian-robot-clay-cost))
      ;; we have enough for a obsidian robot, then spend the resources and update the robots
      (-> state
          (assoc-in [:resources :clay] (- clay obsidian-robot-clay-cost))
          (assoc-in [:resources :ore] (- ore obsidian-robot-ore-cost))
          (assoc-in [:pending-robots :obsidian-robots] (inc obsidian-robots)))
      ;; nope, not enough, just return what we have
      state)))

(defn maybe-build-clay-robot [blueprint state]
  (let [{robots :robots resources :resources} state
        {ore :ore} resources
        {clay-robot-ore-cost :clay-robot-ore-cost} blueprint
        {clay-robots :clay-robots} robots]
    (if (>= ore clay-robot-ore-cost)
      ;; we have enough for a clay robot, then spend the resources and update the robots
      (-> state
          (assoc-in [:resources :ore] (- ore clay-robot-ore-cost))
          (assoc-in [:pending-robots :clay-robots] (inc clay-robots)))
      ;; nope, not enough, just return what we have
      state)))

(defn maybe-build-ore-robot [blueprint state]
  (let [{robots :robots resources :resources} state
        {ore :ore} resources
        {ore-robot-ore-cost :ore-robot-ore-cost} blueprint
        {ore-robots :ore-robots} robots]
    (if (>= ore ore-robot-ore-cost)
      ;; we have enough for an ore robot, then spend the resources and update the robots
      (-> state
          (assoc-in [:resources :ore] (- ore ore-robot-ore-cost))
          (assoc-in [:pending-robots :ore-robots] (inc ore-robots)))
      ;; nope, not enough, just return what we have
      state)))

(defn update-resources [state]
  ;; All we need to do is add resources for each robot we have
  (let [{robots :robots pending-robots :pending-robots resources :resources} state
        {ore-robots :ore-robots
         clay-robots :clay-robots
         obsidian-robots :obsidian-robots
         geode-robots :geode-robots} robots
        {ore :ore
         clay :clay
         obsidian :obsidian
         geode :geode} resources]
    {
     :robots robots
     :pending-robots pending-robots
     :resources {:ore (+ ore ore-robots)
                 :clay (+ clay clay-robots)
                 :obsidian (+ obsidian obsidian-robots)
                 :geode (+ geode geode-robots)} }))

(defn maybe-build-robots [blueprint state]
  ;; A flawed heuristic approach that eventually finds geodes, but isn't optimal
  (let [
        {robots :robots pending-robots :pending-robots resources :resources} state
        {ore-robots :ore-robots
         clay-robots :clay-robots
         obsidian-robots :obsidian-robots
         geode-robots :geode-robots} robots
        {geode-robot-ore-cost :geode-robot-ore-cost
         geode-robot-obisidian-cost :geode-robot-obisidian-cost
         obsidian-robot-ore-cost :obsidian-robot-ore-cost
         obsidian-robot-clay-cost :obsidian-robot-clay-cost} blueprint
        clay-to-ore-cost-ratio (/ obsidian-robot-clay-cost obsidian-robot-ore-cost)
        obsidian-to-ore-cost-ratio (/ geode-robot-obisidian-cost geode-robot-ore-cost)
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

(defn simulate-round [blueprint state]
  (let [{pre-build-robots :robots resources :resources} state]
    (->> state
         (maybe-build-robots blueprint)
         update-resources
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


(defn simulate-rounds [blueprint starting-state rounds]
  (loop [i 0
         state starting-state]
    (if (= i rounds)
      state
      (recur (inc i) (simulate-round blueprint state)))))

(defn quality-level [blueprint state]
  (let [{resources :resources} state
        {geode :geode} resources]
    (* (:blueprint-id blueprint) geode)))

;; == Minute 1 ==
;; 1 ore-collecting robot collects 1 ore; you now have 1 ore.

;; == Minute 2 ==
;; 1 ore-collecting robot collects 1 ore; you now have 2 ore.

;; == Minute 3 ==
;; Spend 2 ore to start building a clay-collecting robot.
;; 1 ore-collecting robot collects 1 ore; you now have 1 ore.
;; The new clay-collecting robot is ready; you now have 1 of them.

;; == Minute 4 ==
;; 1 ore-collecting robot collects 1 ore; you now have 2 ore.
;; 1 clay-collecting robot collects 1 clay; you now have 1 clay.

;; == Minute 5 ==
;; Spend 2 ore to start building a clay-collecting robot.
;; 1 ore-collecting robot collects 1 ore; you now have 1 ore.
;; 1 clay-collecting robot collects 1 clay; you now have 2 clay.
;; The new clay-collecting robot is ready; you now have 2 of them.

;; == Minute 6 ==
;; 1 ore-collecting robot collects 1 ore; you now have 2 ore.
;; 2 clay-collecting robots collect 2 clay; you now have 4 clay.

;; == Minute 7 ==
;; Spend 2 ore to start building a clay-collecting robot.
;; 1 ore-collecting robot collects 1 ore; you now have 1 ore.
;; 2 clay-collecting robots collect 2 clay; you now have 6 clay.
;; The new clay-collecting robot is ready; you now have 3 of them.

;; == Minute 8 ==
;; 1 ore-collecting robot collects 1 ore; you now have 2 ore.
;; 3 clay-collecting robots collect 3 clay; you now have 9 clay.

;; == Minute 9 ==
;; 1 ore-collecting robot collects 1 ore; you now have 3 ore.
;; 3 clay-collecting robots collect 3 clay; you now have 12 clay.

;; == Minute 10 ==
;; 1 ore-collecting robot collects 1 ore; you now have 4 ore.
;; 3 clay-collecting robots collect 3 clay; you now have 15 clay.

;; == Minute 11 ==
;; Spend 3 ore and 14 clay to start building an obsidian-collecting robot.
;; 1 ore-collecting robot collects 1 ore; you now have 2 ore.
;; 3 clay-collecting robots collect 3 clay; you now have 4 clay.
;; The new obsidian-collecting robot is ready; you now have 1 of them.

;; == Minute 12 ==
;; Spend 2 ore to start building a clay-collecting robot.
;; 1 ore-collecting robot collects 1 ore; you now have 1 ore.
;; 3 clay-collecting robots collect 3 clay; you now have 7 clay.
;; 1 obsidian-collecting robot collects 1 obsidian; you now have 1 obsidian.
;; The new clay-collecting robot is ready; you now have 4 of them.

;; == Minute 13 ==
;; 1 ore-collecting robot collects 1 ore; you now have 2 ore.
;; 4 clay-collecting robots collect 4 clay; you now have 11 clay.
;; 1 obsidian-collecting robot collects 1 obsidian; you now have 2 obsidian.

;; == Minute 14 ==
;; 1 ore-collecting robot collects 1 ore; you now have 3 ore.
;; 4 clay-collecting robots collect 4 clay; you now have 15 clay.
;; 1 obsidian-collecting robot collects 1 obsidian; you now have 3 obsidian.

;; == Minute 15 ==
;; Spend 3 ore and 14 clay to start building an obsidian-collecting robot.
;; 1 ore-collecting robot collects 1 ore; you now have 1 ore.
;; 4 clay-collecting robots collect 4 clay; you now have 5 clay.
;; 1 obsidian-collecting robot collects 1 obsidian; you now have 4 obsidian.
;; The new obsidian-collecting robot is ready; you now have 2 of them.

;; == Minute 16 ==
;; 1 ore-collecting robot collects 1 ore; you now have 2 ore.
;; 4 clay-collecting robots collect 4 clay; you now have 9 clay.
;; 2 obsidian-collecting robots collect 2 obsidian; you now have 6 obsidian.

;; == Minute 17 ==
;; 1 ore-collecting robot collects 1 ore; you now have 3 ore.
;; 4 clay-collecting robots collect 4 clay; you now have 13 clay.
;; 2 obsidian-collecting robots collect 2 obsidian; you now have 8 obsidian.

;; == Minute 18 ==
;; Spend 2 ore and 7 obsidian to start building a geode-cracking robot.
;; 1 ore-collecting robot collects 1 ore; you now have 2 ore.
;; 4 clay-collecting robots collect 4 clay; you now have 17 clay.
;; 2 obsidian-collecting robots collect 2 obsidian; you now have 3 obsidian.
;; The new geode-cracking robot is ready; you now have 1 of them.

;; == Minute 19 ==
;; 1 ore-collecting robot collects 1 ore; you now have 3 ore.
;; 4 clay-collecting robots collect 4 clay; you now have 21 clay.
;; 2 obsidian-collecting robots collect 2 obsidian; you now have 5 obsidian.
;; 1 geode-cracking robot cracks 1 geode; you now have 1 open geode.

;; == Minute 20 ==
;; 1 ore-collecting robot collects 1 ore; you now have 4 ore.
;; 4 clay-collecting robots collect 4 clay; you now have 25 clay.
;; 2 obsidian-collecting robots collect 2 obsidian; you now have 7 obsidian.
;; 1 geode-cracking robot cracks 1 geode; you now have 2 open geodes.

;; == Minute 21 ==
;; Spend 2 ore and 7 obsidian to start building a geode-cracking robot.
;; 1 ore-collecting robot collects 1 ore; you now have 3 ore.
;; 4 clay-collecting robots collect 4 clay; you now have 29 clay.
;; 2 obsidian-collecting robots collect 2 obsidian; you now have 2 obsidian.
;; 1 geode-cracking robot cracks 1 geode; you now have 3 open geodes.
;; The new geode-cracking robot is ready; you now have 2 of them.

;; == Minute 22 ==
;; 1 ore-collecting robot collects 1 ore; you now have 4 ore.
;; 4 clay-collecting robots collect 4 clay; you now have 33 clay.
;; 2 obsidian-collecting robots collect 2 obsidian; you now have 4 obsidian.
;; 2 geode-cracking robots crack 2 geodes; you now have 5 open geodes.

;; == Minute 23 ==
;; 1 ore-collecting robot collects 1 ore; you now have 5 ore.
;; 4 clay-collecting robots collect 4 clay; you now have 37 clay.
;; 2 obsidian-collecting robots collect 2 obsidian; you now have 6 obsidian.
;; 2 geode-cracking robots crack 2 geodes; you now have 7 open geodes.

;; == Minute 24 ==
;; 1 ore-collecting robot collects 1 ore; you now have 6 ore.
;; 4 clay-collecting robots collect 4 clay; you now have 41 clay.
;; 2 obsidian-collecting robots collect 2 obsidian; you now have 8 obsidian.
;; 2 geode-cracking robots crack 2 geodes; you now have 9 open geodes.
