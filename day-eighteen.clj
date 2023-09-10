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
                   ])))
  )

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

;; Just playing around with visualisation using thi.ng
;; ---------------------------------------------------

(defn cube-vertices-for=bottom-left [x y z]
  [[x y z]
   [x (inc y) z]
   [(inc x) (inc y) z]
   [(inc x) y z]
   [x y (inc z)]
   [x (inc y) (inc z)]
   [(inc x) (inc y) (inc z)]
   [(inc x) y (inc z)]])

(defn verticies-for-data [data]
  (apply concat (map (fn [cube-data ] (apply cube-vertices-for=bottom-left cube-data)) data)))

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
