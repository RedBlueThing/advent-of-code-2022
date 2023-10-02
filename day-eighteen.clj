(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[thi.ng.geom.gl.glmesh :as glm])
(require '[thi.ng.color.core :as col])
(require '[thi.ng.geom.gl.camera :as cam])
(require '[thi.ng.geom.aabb :as a])
(require '[thi.ng.geom.gl.shaders.basic :as basic])
(require '[thi.ng.geom.gl.shaders :as sh])
(require '[thi.ng.geom.attribs :as attr])
(require '[thi.ng.math.core :as m])
(require '[thi.ng.geom.core :as g])
(require '[thi.ng.geom.circle :as c])
(require '[thi.ng.geom.svg.core :as svg])
(require '[thi.ng.geom.svg.adapter :as adapt])
(require '[thi.ng.geom.svg.shaders :as shader])
(require '[thi.ng.geom.svg.renderer :as render])
(require '[clojure.java.io :as io])
(require '[thi.ng.geom.matrix :as mat])
(require '[thi.ng.geom.mesh.io :as mio])
(require '[thi.ng.geom.gmesh :as gmesh])
(require '[thi.ng.geom.basicmesh :as basicmesh])
(require '[thi.ng.geom.vector :as v :refer [vec2 vec3]])
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])

;; we can treat the inputs as an array of 3D coordinates of the bottom left
;; corner of each cube.

(def label-to-index {:x 0 :y 1 :z 2})

(def test-data-raw ["2,2,2" "1,2,2" "3,2,2" "2,1,2" "2,3,2" "2,2,1" "2,2,3" "2,2,4" "2,2,6" "1,2,5" "3,2,5" "2,1,5" "2,3,5"])

(def real-data-raw (str/split-lines (slurp "day-eighteen.txt")))

(defn data-for-raw [raw]
  (map (fn parse-line [raw-line] (map #(Integer/parseInt %) (str/split raw-line #","))) raw))

;; ---------------------------------------------------

(defn project-point [point axis]
  ;; project along an axis by just zeroing that axis value
  (map-indexed (fn [index value] (if (= (label-to-index axis) index) 0 value)) point))

(defn projection-set [data axis]
  ;; Return a set of coordinates projected along the provided axis
  (set (map (fn [point] (project-point point axis)) data)))

(defn part-one-surface-area-convex-blob [data]
  ;; This naive attempt assumes that the blob is a convex shape and will always
  ;; undershoot the actual surface area.
  (reduce +
          [(* 2 (count (projection-set data :x)))
           (* 2 (count (projection-set data :y)))
           (* 2 (count (projection-set data :z)))]))

;; ---------------------------------------------------

(defn sides-ignoring-covered [data]
  ;; if we ignore sides being covered aby other cubes it's just the number of
  ;; cubes times six.
  (* (count data) 6))

(defn cube-sides-set [cube]
  ;; We would like a set of coordinates for the center of each face of the cube.
  ;; We are going to use those coordinates as unique identifiers for each face.
  (let [[ x y z ] cube]
    (set (map seq [
                   [(+ x 1/2) (+ y 1/2) z]
                   [(+ x 1/2) (+ y 1/2) (+ z 1)]
                   [(+ x 1/2) (+ y 1) (+ z 1/2)]
                   [(+ x 1/2) y (+ z 1/2)]
                   [x (+ y 1/2) (+ z 1/2)]
                   [(+ x 1) (+ y 1/2) (+ z 1/2)]
                   ]))))

(defn covered-sides [cube-one cube-two]
  ;; We are going to identify overlapping sides with the center point of that
  ;; side (x,y,z).
  (set/intersection (cube-sides-set cube-one) (cube-sides-set cube-two)))

(defn covered-sides-for-data [data]
  (reduce set/union (map (fn [cube-pair] (apply covered-sides cube-pair)) (combo/combinations data 2))))

(defn part-one-total-uncovered-sides [data]
  (let [
        ;; first get the number of sides for all the cubes
        total-sides (sides-ignoring-covered data)
        ;; the number of unique sides that are covered
        total-covered-sides (count (covered-sides-for-data data))
        ]
    ;; so our answer is the total number of sides for all the cubes minus the
    ;; covered sides times two (because each covered side has been counted for
    ;; each of the two adjoining cubes
    (- total-sides (* total-covered-sides 2))))

(assert (= (count (covered-sides '(1 1 1) '(2 1 1))) 1))
(assert (= (count (covered-sides '(2 2 2) '(2 1 1))) 0))
(assert (= (count (covered-sides '(1 1 1) '(2 1 0))) 0))

;; ---------------------------------------------------

;; Part Two - What if we created a set of cubes within the bounds of the
;; blob (that aren't part of the blob) and then for each of those compare them
;; to all the cubes in the blob and count the number that have six adjoining
;; blob cubes.
;;
;; Then we subtract their surface area from the part one solution?
(defn find-single-cube-bubble-enclosed-empty-space [data]
  (into {} (filter (fn only-enclosed-space-bordering-six-cubes [[k v]] (= v 6))
                   (reduce (fn [cube-dictionary [empty-space-cube adjoining-face-set]]
                             (assoc cube-dictionary empty-space-cube (+ (count adjoining-face-set) (or (cube-dictionary empty-space-cube) 0))))
                           {}
                           (for [empty-space-cube (empty-space-for-data data)
                                 blob-cube data]
                             (seq [empty-space-cube (covered-sides empty-space-cube blob-cube) ]))))))

;; So the problem with this solution is that it assumes single cubes inside the
;; blob. Which works fine with the test data because there is only a single cube
;; air bubble in the test data. The real data has much larger variously shaped
;; air bubbles, so we need to simulate the steam/water flow from the outside
;; around the blob.
(defn part-two-solution-flawed [data]
  (- (part-one-total-uncovered-sides data) (* (count (find-single-cube-bubble-enclosed-empty-space data)) 6)))

(defn axis-value-for-cube [axis-label cube] (nth cube (label-to-index axis-label)))

(defn range-for-data [data border]
  (let [x-axis-values (map (partial axis-value-for-cube :x) data)
        y-axis-values (map (partial axis-value-for-cube :y) data)
        z-axis-values (map (partial axis-value-for-cube :z) data)
        ;; we are going to extend the range outside the data (by border) so the
        ;; water flow algorithm can get around the sides of the blog
        min-x-value (- (apply min x-axis-values) border)
        min-y-value (- (apply min y-axis-values) border)
        min-z-value (- (apply min z-axis-values) border)
        max-x-value (+ (apply max x-axis-values) border)
        max-y-value (+ (apply max y-axis-values) border)
        max-z-value (+ (apply max z-axis-values) border)]
    {:min-x-value min-x-value
    :min-y-value min-y-value
    :min-z-value min-z-value
    :max-x-value max-x-value
    :max-y-value max-y-value
    :max-z-value max-z-value}))


(defn corner-for-data [data]
  (let [cube-range (range-for-data data 0)
        min-x-value (cube-range :min-x-value)
        min-y-value (cube-range :min-y-value)
        min-z-value (cube-range :min-z-value)] (seq [ min-x-value min-y-value min-z-value ])))


(defn empty-space-for-data [data]
  (let [
        cube-range (range-for-data data 1)
        min-x-value (cube-range :min-x-value)
        min-y-value (cube-range :min-y-value)
        min-z-value (cube-range :min-z-value)
        max-x-value (cube-range :max-x-value)
        max-y-value (cube-range :max-y-value)
        max-z-value (cube-range :max-z-value)
        all-cube-data (set (for [x (range min-x-value (inc max-x-value))
                                 y (range min-y-value (inc max-y-value))
                                 z (range min-z-value (inc max-z-value))]
                             (seq [ x y z ])))]
    (set/difference all-cube-data data)))



(defn neighbours-for-cube [[x y z]]
  ;; Where a cube is defined by the bottom/left/front corner of the cube
  (set (map seq [
                 [(inc x) y z]
                 [(dec x) y z]
                 [x (inc y) z]
                 [x (dec y) z]
                 [x y (inc z)]
                 [x y (dec z)]
                 ])))

(defn trim-out-of-range [cubes-to-check cube-range]
  (let [min-x-value (cube-range :min-x-value)
        min-y-value (cube-range :min-y-value)
        min-z-value (cube-range :min-z-value)
        max-x-value (cube-range :max-x-value)
        max-y-value (cube-range :max-y-value)
        max-z-value (cube-range :max-z-value)
        cubes-in-range-set (set (filter (fn [[x y z]] ;;check
                                      (and (<= x max-x-value)
                                           (>= x min-x-value)
                                           (<= y max-y-value)
                                           (>= y min-y-value)
                                           (<= z max-z-value)
                                           (>= z min-z-value)))
                                    cubes-to-check))
        ]
    ;; return the cubes still in range
    cubes-in-range-set))


(defn recurse-trim-empty-space-set [empty-space-set removed-from-empty-space-set current-empty-cube blob-set cube-range]
  ;; Given the current remaining empty space cubes to check, the range of the
  ;; blob, the current cubes in the blob and a cube in the empty space to check
  ;; return a new empty space set with that cube and any of it's outside blob
  ;; neighbours removed
  (let [neighbours-to-check (neighbours-for-cube current-empty-cube)
        ;; remaining neighbours to check is everything that isn't out of
        ;; range (or in the blob)
        remaining-in-range-neighbours-to-check (trim-out-of-range neighbours-to-check cube-range)
        ;; now check if any those in range neighbours are in the blob
        remaining-in-range-and-outside-blob-neighbours-to-check (set/difference remaining-in-range-neighbours-to-check blob-set)
        ;; just remove the current-cube from the empty space set to get the new trimmed empty space set
        new-trimmed-empty-space-set (disj empty-space-set current-empty-cube)
        ;; because we removed the current cube from the empty space set we can add it to the shared atom
        foo (swap! removed-from-empty-space-set conj current-empty-cube)
        ;; now we can compare our
        ;; remaining-in-range-and-outside-blob-neighbours-to-check cubes to ones
        ;; already removed from the empty space set to come up with the
        ;; remaining neighbours to check
        remaining-neighbours-to-check (set/difference remaining-in-range-and-outside-blob-neighbours-to-check @removed-from-empty-space-set)]

    ;; if there are no remaining n
    (if (empty? remaining-neighbours-to-check)
      ;; n.b. this is just the empty space set with the cubes from this
      ;; recursive branch removed, but we are going to intersect this with all
      ;; the other empty-space-sets from all the other recursive branches so we
      ;; are left with just the empty space set cubes that weren't reached by
      ;; looking at neighbours.
      new-trimmed-empty-space-set
      ;; We want the intersection of all the sets of :empty-space-set from the neighbours to check (and all their neighbours)
      (apply set/intersection (map (fn [current-empty-cube-recurse] (recurse-trim-empty-space-set new-trimmed-empty-space-set removed-from-empty-space-set current-empty-cube-recurse blob-set cube-range)) remaining-neighbours-to-check)))))

;; This works perfectly except for the minor problem that it overflows the stack
;; on the real large data set. Which in hindsight makes perfect sense.
;;
;; So I just need to re-write this as a queue or stack based solution and we
;; are all good.
(defn trim-empty-space-set-explod-on-large-data [empty-space-set current-empty-cube blob-set cube-range]
  ;; We are going to recursively trim the empty space set as we iterate through
  ;; adjacent cubes, and we are going to use a global atom to record the cubes
  ;; removed from the empty-space-set so we know when to stop.
  (let [removed-from-empty-space-set (atom #{})]
    (recurse-trim-empty-space-set empty-space-set removed-from-empty-space-set current-empty-cube blob-set cube-range)))


;; so we will start at the edge of the empty space and recursively check
;; neighbours until we can only find blob cubes or cubes out of range.
(defn solution-part-two [data]
  (let [cube-range (range-for-data data 1)
        empty-space-set (empty-space-for-data data)
        blob-set (set data)
        ;; get the part one solution so we have something to subtract
        total-uncovered-sides (part-one-total-uncovered-sides data)
        ;; get the number of cubes that we can't reach from the outside
        ;; by starting at x,y,z simulate water flow outside the blob
        set-of-unreachable-cubes (trim-empty-space-set-explod-on-large-data empty-space-set (corner-for-data empty-space-set) blob-set cube-range)
        ;; the uncovered sides for the unreachable cubes
        unreachable-total-uncovered-sides (part-one-total-uncovered-sides set-of-unreachable-cubes)
        ]
    ;; The answer is the total uncovered sides of the blobs minus the total
    ;; uncovered sides of the unreachable cubes.
    ;; (println (corner-for-data empty-space-set))
    ;; (println cube-range)
    ;; (println empty-space-set)
    ;; (println set-of-unreachable-cubes)
    ;; (println total-uncovered-sides)
    ;; (println unreachable-total-uncovered-sides)
    (- total-uncovered-sides unreachable-total-uncovered-sides)))

;; Just playing around with visualisation using thi.ng
;; ---------------------------------------------------

(defn cube-vertices-for-bottom-left [x y z]
  [[x y z]
   [x (inc y) z]
   [(inc x) (inc y) z]
   [(inc x) y z]
   [x y (inc z)]
   [x (inc y) (inc z)]
   [(inc x) (inc y) (inc z)]
   [(inc x) y (inc z)]])

(defn verticies-for-data [data]
  (apply concat (map (fn [cube-data ] (apply cube-vertices-for-bottom-left cube-data)) data)))

(defn faces-for-data [data]
  ;; Assuming we have vertices corresponding to the data, create indexes for
  ;; faces
  (map-indexed (fn [index item] (let [bottom-left-index (if (= index 0) 0 (inc (* index 8)))] [[bottom-left-index (+ bottom-left-index 1) (+ bottom-left-index 2) (+ bottom-left-index 3)]
                                                                                               ])) data)
  )

(defn data-mesh [data]
  (let [vertices [[0 0 0][0 1 0][1 1 0][1 0 0]]
        faces [[0 1 2 3 0]]]
    (-> (gmesh/gmesh)
        (assoc :vertices (set vertices))
        (assoc :faces (apply vector (map (fn make-face [face-vertex-indicies] (thi.ng.geom.meshface.MeshFace. (apply vector (map #(apply vec3 (nth vertices %)) face-vertex-indicies)) nil)) faces)))
        )))

(def width  640)
(def height 480)
(def model  (-> (mat/matrix44) (g/rotate-x m/HALF_PI) (g/rotate-z m/SIXTH_PI)))
(def view   (apply mat/look-at (mat/look-at-vectors 0 0 2 0 0 0)))
(def proj   (mat/perspective 60 (/ width height) 0.1 2))
(def mvp    (->> model (m/* view) (m/* proj)))
(def col-tx (g/rotate-x (mat/matrix44) (- m/HALF_PI)))

(def shader
  (shader/shader
   {:fill     (shader/phong
               {:model     model
                :view      view
                :light-pos [0 0 -1]
                :light-col [1 1 1]
                :diffuse   (shader/normal-rgb col-tx)
                :ambient   [0.1 0.1 0.2]
                :specular  1.0
                :shininess 6.0})
    :uniforms {:stroke "black" :stroke-width 0.25}
    :flags    {:solid false}}))

(def stl-mesh
  (with-open [in (io/input-stream "./suzanne.stl")]
    (-> in
        (mio/wrapped-input-stream)
        (mio/read-stl)
        (g/center)
        (g/scale 0.85))))

(def cube-mesh
  (let [vertices [[0 0 0][0 1 0][1 1 0][1 0 0]]
        faces [[0 1 2 3 0]]]
       (-> (gmesh/gmesh)
           (assoc :vertices (set vertices))
           (assoc :faces (apply vector (map (fn make-face [face-vertex-indicies] (thi.ng.geom.meshface.MeshFace. (apply vector (map #(apply vec3 (nth vertices %)) face-vertex-indicies)) nil)) faces)))
           )))

;; (map #(thi.ng.geom.meshface.MeshFace. (apply vector (map #(apply vec3 %))) nil) face-vertex-indicies)

(defn render-svg
  [path mesh mvp width height]
  (let [screen (mat/viewport-matrix width height)]
    (->> (svg/svg
          {:width width :height height}
          (render/mesh mesh mvp screen shader))
         (svg/serialize)
         (spit path))))

(render-svg "./svg-stl-mesh.svg" stl-mesh mvp width height)
(render-svg "./svg-cube-mesh.svg" cube-mesh mvp width height)

