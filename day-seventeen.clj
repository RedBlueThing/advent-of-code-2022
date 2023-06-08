(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math.combinatorics :as combo])

(def test-data-raw ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(def real-data-raw (slurp "day-seventeen.txt"))

(defn parse-data [data]
  (remove nil? (map #(case % \> :right \< :left nil) (seq data))))

;; Some globals that I am expecting might change for part two
(def cavern-width-units 7)
(def left-edge-starting-x 2) ; 0,1,#
(def bottom-edge-starting-y-offset 3) ; 0,1,2,#

(defn next-rock-type [current-rock-type]
  ;; A function that gives me the next rock type given the last one.
  (let [rock-types-in-order [:horizontal-line :cross :corner :vertical-line :block]
        successors (zipmap rock-types-in-order (concat (rest rock-types-in-order) [(first rock-types-in-order)]))]
    (successors current-rock-type)))

(defn next-gas-jet [data offset]
  ;; what is the next direction gas jet given offset from when the simulation
  ;; started.
  (nth (cycle data) offset))

(defn bottom-left-to-top-right-game-board-for-rock-type [rock-type]
  ({:horizontal-line [[\# \# \# \#]]
    :cross [[nil \@ nil] [\@ \@ \@] [nil \@ nil]]
    :corner [[\% \% \%] [nil nil \%] [nil nil \%]]
    :vertical-line [[\$] [\$] [\$] [\$]]
    :block [[\& \&] [\& \&]]} rock-type))

(defn sprite-key-for-row-column [row-index cell-index]
  (format "%d,%d" row-index cell-index))

(defn starting-sprite-for-rock-type-game-board [rock-type-game-board bottom-edge-y-offset]
  (into {}
        (filter (comp not nil? second) (mapcat (fn [x] x) (for [[row-index row] (map-indexed vector rock-type-game-board)]
                                       (for [[cell-index cell] (map-indexed vector row)]
                                         [(sprite-key-for-row-column (+ bottom-edge-y-offset row-index) (+ left-edge-starting-x cell-index)) cell]))))))

(defn height-for-rock-type [rock-type]
  (count (bottom-left-to-top-right-game-board-for-rock-type rock-type)))

(defn create-bottom-left-to-top-right-game-board [rows columns]
  {:rows rows
   :columns columns
   :data (repeat rows (repeat columns nil))
   :sprite nil})

(defn row-index-of-top-non-nil [game-board]
  (let [rows (game-board :rows)
        columns (game-board :columns)
        data (game-board :data)]
    (loop [i (dec rows)]
      (if (>= i 0)
        (let [row (nth data i)]
          (if (some #(not= % nil) row)
            i
            (recur (dec i))))
        -1))))

(defn update-game-board-for-new-rock [game-board rock-type]
  ;; Returns a new game-board with enough room above the top shape. From top to
  ;; bottom look for a row with rocks in it.
  (let [rows (:rows game-board)
        columns (:columns game-board)
        data (:data game-board)
        top-index (row-index-of-top-non-nil game-board)
        sprite (starting-sprite-for-rock-type-game-board (bottom-left-to-top-right-game-board-for-rock-type rock-type) (+ (inc bottom-edge-starting-y-offset) top-index))
        new-rows (- (+ bottom-edge-starting-y-offset (height-for-rock-type rock-type)) (- rows (inc top-index)))]
    {:rows (+ rows (if (> new-rows 0) new-rows 0))
     :columns columns
     :data (if (> new-rows 0) (concat data (repeat new-rows (repeat columns nil))) data)
     :sprite sprite}))

(def gutter 0)
(def tile-size 30)

(defn render-size [game-board]
  (let [rows (:rows game-board)
        columns (:columns game-board)]
    [(* columns (+ tile-size (* gutter 2))) (* rows (+ tile-size (* gutter 2)))]))



(defn render-sprite-data [data sprite clear?]
  ;; Just render the current sprite at it's location
  (map (fn [[row-index row]] (map (fn [[cell-index cell]]
                                    (let [sprite-value-at (sprite (sprite-key-for-row-column row-index cell-index))]
                                      (if sprite-value-at (if clear? nil sprite-value-at) cell)))
                                  (map-indexed vector row))) (map-indexed vector data)))

(defn render-sprite [game-board clear?]
  ;; Just render the current sprite at it's location
  (let [rows (:rows game-board)
        columns (:columns game-board)
        data (:data game-board)
        sprite (:sprite game-board)]
    {:rows rows
     :columns columns
     :data (render-sprite-data data sprite clear?)
     :sprite sprite}))

(defn new-coordinate-for-direction [[y x] direction]
  (case direction
    :left [y (dec x)]
    :right [y (inc x)]
    :down [(dec y) x]))

(defn sprite-location-viable? [sprite game-board]
  ;; Check if the sprite is still on the board and hasn't overlapped with
  ;; another rock.
  (let [
        rows (:rows game-board)
        columns (:columns game-board)
        data (:data game-board)
        sprite-coordinates (map (fn [[ cell-key value ]] (map #(Integer/parseInt %) (str/split cell-key #","))) sprite)]

    ;; make sure all the coordinates in the sprite are in range and refer to a
    ;; cell that doesn't have part of a rock in it.
    (every? (fn viable? [[y x]] (and (>= x 0)
                                     (>= y 0)
                                     (< x columns)
                                     (< y rows)
                                     (nil? (nth (nth data y) x)))) sprite-coordinates)
    )
  )

(defn new-location-for-sprite [sprite direction]
  (into {} (map (fn [[ cell-key value ]] [(apply sprite-key-for-row-column (new-coordinate-for-direction (map #(Integer/parseInt %) (str/split cell-key #",")) direction)) value]) sprite)))

(defn move-sprite [game-board direction]
  ;; just update the sprite with new coordinates
  (let [rows (:rows game-board)
        columns (:columns game-board)
        data (:data game-board)
        sprite (:sprite game-board)
        new-sprite (new-location-for-sprite sprite direction)
        ]
    {:rows rows
     :columns columns
     :data data
     :sprite new-sprite}))

(defn next-direction [last-direction gas-jet-data]
  (let [offset (:offset gas-jet-data)
        data (:data gas-jet-data)]
    (if (= last-direction :down)
      {:direction (next-gas-jet data offset) :gas-jet-data {:offset (inc offset) :data data} }
      {:direction :down :gas-jet-data gas-jet-data })))

(defn simulate-rock-fall [rock-count gas-jet-data]
  (loop [dropped-rocks 0
         current-rock-type :horizontal-line
         game-board (update-game-board-for-new-rock (create-bottom-left-to-top-right-game-board (inc bottom-edge-starting-y-offset) cavern-width-units) current-rock-type)
         current-gas-jet-data {:offset 0 :data gas-jet-data}]
    (if (= (mod dropped-rocks 100000) 0) (println dropped-rocks))
    (if (= dropped-rocks rock-count)
      ;; we have dropped the right number of rocks, so just return the game game-board
      game-board
      ;; we need to simulate a single rock drop (by iterating through all the moves until it stops)
      (let [current-drop-result (loop [updating-game-board game-board
                   current-drop-gas-jet-data current-gas-jet-data
                                       last-direction :down]
              (let [
                    ;; need to get new rows and columns
                    rows (:rows updating-game-board)
                    columns (:columns updating-game-board)
                    data (:data updating-game-board)
                    ;; seems like I could have done this with a global atom for the
                    ;; offset (in next-gas-jet), but that would have meant reseting it
                    ;; each time I did another run, which seemed undesirable.
                    ;;
                    ;; So instead I jump through some hoops keeping my offset with the
                    ;; gas-jet-data which is incremented from next-direction
                    next-direction-response (next-direction last-direction current-drop-gas-jet-data)
                    new-drop-gas-jet-data (:gas-jet-data next-direction-response)
                    direction (:direction next-direction-response)
                    ;; Now we have the direction we can move the sprite (or not)
                    sprite (:sprite updating-game-board)
                    new-sprite (new-location-for-sprite sprite direction)
                    new-sprite-location-viable? (sprite-location-viable? new-sprite updating-game-board)
                    ;; we are done if the sprite is moving down and it's new location
                    ;; isn't viable because it overlaps with an existing rock or has
                    ;; moved below the world
                    done? (and (= direction :down) (not new-sprite-location-viable?))
                    ]
                (if done?
                  ;; If we are done with this rock falling, we render the rock
                  ;; onto the game board (based on it's sprite data) and return
                  ;; it.
                  { :new-game-board (assoc updating-game-board :data (render-sprite-data data sprite false)) :new-gas-jet-data new-drop-gas-jet-data}
                  ;; we just update the game board with the new sprite (we
                  ;; render at the end of the loop now so we don't fail to be
                  ;; viable because the game board contains the current sprite)
                  (recur (assoc updating-game-board :sprite (if new-sprite-location-viable? new-sprite sprite)) new-drop-gas-jet-data direction)
                  )
                )
              )
            new-game-board (:new-game-board current-drop-result)
            new-gas-jet-data (:new-gas-jet-data current-drop-result)
            new-rock-type (next-rock-type current-rock-type)
            ]
         (recur (inc dropped-rocks) new-rock-type (update-game-board-for-new-rock new-game-board new-rock-type) new-gas-jet-data)
        )
      )
    )
  )

(defn randomize-game-board [game-board]
  ;; Just for testing
  (let [rows (:rows game-board)
        columns (:columns game-board)
        data (:data game-board)
        sprite (:sprite game-board)]
    {:rows rows
     :columns columns
     :data (map (fn [row] (map (fn [cell] (rand-nth [\# \@ \% \$ \&])) row)) data)
     :sprite sprite}))

(require '[clojure.java.io :as io])
(import '[java.awt Color Graphics2D Rectangle]
        '[java.awt.image BufferedImage]
        '[javax.imageio ImageIO]
        '[javax.swing ImageIcon JLabel JFrame])

(defn color-for-cell [cell]
  (or ({\# Color/RED
     \@ Color/BLUE
     \% Color/GREEN
     \$ Color/MAGENTA
     \& Color/YELLOW} cell) Color/GRAY))

(defn draw-game-board-squares [game-board graphics]
  (doseq [[row-index row] (map-indexed vector (reverse (:data game-board)))]
    (doseq [[cell-index cell] (map-indexed vector row)]
      (let [cell-left (* cell-index tile-size)
            cell-top (* row-index tile-size)
            color (color-for-cell cell)]
        (do (.setColor graphics color)
            (.fillRect graphics cell-left cell-top tile-size tile-size)
            (.setColor graphics (.darker color))
            (.fillRect graphics (+ cell-left 2) (+ cell-top 2) (- tile-size 4) (- tile-size 4)))))))

(def game-board-file "tetris.png")

(defn render-game-board [game-board fn]
  (let [filename fn
        [width height] (render-size game-board)
        image (BufferedImage. width height BufferedImage/TYPE_INT_RGB)]
    (draw-game-board-squares game-board (.createGraphics image))
    (ImageIO/write image "png" (io/file filename))
    (doto (JFrame. "Image")
      (.add (doto (JLabel.)
              (.setIcon (ImageIcon. (str (io/file filename))))))
      (.pack)
      (.setVisible true))))

;; some test code to render a random board
;; (render-game-board (randomize-game-board (create-bottom-left-to-top-right-game-board (inc bottom-edge-starting-y-offset) cavern-width-units)) game-board-file)
;; render a game after a single rock has dropped
;; (render-game-board (simulate-rock-fall 1 (parse-data test-data-raw)) game-board-file)
;; Render the test data example
;; (render-game-board (let [game-board (simulate-rock-fall 10 (parse-data test-data-raw)) sprite (:sprite game-board)](assoc game-board :data (render-sprite-data (:data game-board) sprite false))) game-board-file)

;; part one
;; (render-game-board (simulate-rock-fall 2022 (parse-data real-data-raw)) game-board-file)
(def test-data-result-part-one (inc (row-index-of-top-non-nil (simulate-rock-fall 2022 (parse-data test-data-raw)))))

;; (inc (row-index-of-top-non-nil (simulate-rock-fall 2022 (parse-data real-data-raw))))

(require '[clojure.java.io :as io]
         '[clojure.java.shell :refer [sh]])

(defn open-image [filename]
  (sh "open" "-a" "Preview" filename))

;; Usage
(open-image game-board-file)
