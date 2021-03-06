(ns unrest.cavern
  (:use unrest.core
        unrest.blocks
        unrest.srand
        unrest.util))

(set! *warn-on-reflection* true)

(def ^:dynamic *min-spiral-radius* 8)
(def ^:dynamic *max-spiral-radius* 1/2)
(def ^:dynamic *base-width* 3)
(def ^:dynamic *width-mul* 6)
(def ^:dynamic *num-caves* 15)
(def ^:dynamic *turn-rate* 1)
(def ^:dynamic *edge-distance-fudge-factor* 5/12)

(defn in-cave?-fn
  "Takes a cave-params hash (see random-cave for an example) or a seq
  of cave-params hashes, and returns a fn of x y z that returns true
  if the coordinates are within the walls of a cave.  If cave-params
  is a single cave, the function returned also takes a single y
  argument and returns a vector [cx cz td] with the x and z
  coordinates of the center of the cave at that y, and the theta
  divisor for the cave, used in computing twists."
  ([cave-params]
     (if (or (vector? cave-params)
             (seq? cave-params))
       (if (seq cave-params)
         (let [fns (map in-cave?-fn cave-params)]
           (fn
             ([x y z]
                (loop [in-cave? (first fns)
                       rest-fns (rest fns)]
                  (cond (in-cave? x y z) true
                        (seq rest-fns) (recur (first rest-fns)
                                              (rest rest-fns))
                        :else false)))
             ([y]
                (loop [in-cave? (first fns)
                       rest-fns (rest fns)
                       params (first cave-params)
                       rest-params (rest cave-params)]
                  (cond (and (<= y (:max-y params))
                             (>= y (:min-y params)))
                          (in-cave? y)
                        (not (seq rest-fns))
                          (throw (RuntimeException.
                                  (str "Ran out of cave in in-cave? for"
                                       " y=" y)))
                        :else
                          (recur (first rest-fns)
                                 (rest rest-fns)
                                 (first rest-params)
                                 (rest rest-params))))))))
       ;; not (or (vector? ...) ...) -- i.e., cave-params is a hash:
       (if-let [layers (:layered-cave-params cave-params)]
         (let [fns (mapv in-cave?-fn layers)]
           (fn
             ([x y z]
                (if-let [f (fns (* 2 y))]
                  (f x y z)
                  (throw (Exception. (str "in-cave? " x " " y " " z
                                          " called, fns is: " fns
                                          " with count " (count fns))))))))
         ;; not layered -- just one cave:
         (let [x0 (:x0 cave-params)
               z0 (:z0 cave-params)
               min-y (:min-y cave-params)
               max-y (:max-y cave-params)
               r (:radius cave-params)
               turn-rate (or (:turn-rate cave-params) 1)
               theta-divisor (* turn-rate (* 0.25 r))
               theta-offset (or (:theta-offset cave-params) 0)
               div-64-r (/ 64 r)
               width-rate (or (:width-rate cave-params) 5.0)
               width-offset (or (:width-offset cave-params) 0)
               base-width (or (:base-width cave-params) 3)
               width-mul (or (:width-mul cave-params) 6)]
           (fn
             ([x y z]
                (if (or (> y max-y)
                        (< y min-y))
                  false
                  (let [x (- x x0)
                        z (- z z0)
                        theta (+ theta-offset (/ y theta-divisor))
                        cave-x (* r (Math/sin theta))
                        cave-z (* r (Math/cos theta))
                        x-dist (- x cave-x)
                        z-dist (- z cave-z)
                        width (* width-mul
                                 (+ base-width
                                    (Math/sin (+ width-offset
                                                 (/ y width-rate)))))
                        dist-squared (+ (* x-dist x-dist)
                                        (* z-dist z-dist))
                        dist-skew (+ (* cave-x x-dist div-64-r)
                                     (* cave-z z-dist div-64-r))
                        dist-squared (+ dist-squared
                                        (Math/abs dist-skew))]
                    (< dist-squared (* width width)))))
             ([y]
                (let [theta (+ theta-offset (/ y theta-divisor))
                      cave-x (* r (Math/sin theta))
                      cave-z (* r (Math/cos theta))]
                  {:cave-x (+ cave-x x0)
                   :cave-z (+ cave-z z0)
                   :theta-divisor theta-divisor}))))))))

(defn filter-cave-params-by-y
  ([y cave-params]
     (filter #(and (>= y (:min-y %))
                   (<= y (:max-y %)))
             cave-params)))

(defn optimize-cave-params
  "Takes cave-params as nested seqs; returns cave-params as a hash in
  {:layered-cave-params (cp-seq cp-seq ...) form}"
  ([cave-params y-max]
     (let [flat-cp (flatten cave-params)
           cp-seqs (map filter-cave-params-by-y
                        (range 0 y-max 1/2)
                        (repeat flat-cp))]
       {:layered-cave-params cp-seqs})))

(defn inverse-cave-generator
  ([cave-params seed]
     (let [in-cave? (in-cave?-fn cave-params)]
       (fn [x y z]
         (cond (in-cave? x y z)
                 (if (> 0.01 (srand 1 seed x y z))
                   (mc-block :glowstone)
                   (mc-block :stone))
               :else (mc-block :air))))))

(defn simple-cave-generator
  ([cave-params seed]
     (let [in-cave? (in-cave?-fn cave-params)]
       (fn [x y z]
         (cond (in-cave? x y z)
                 (mc-block :air)
               :else (if (> 0.01 (srand 1 seed x y z))
                       (mc-block :glowstone)
                       (mc-block :stone)))))))

(defn dark-cave-generator
  ([cave-params]
     (let [in-cave? (in-cave?-fn cave-params)]
       (fn [x y z]
         (cond (in-cave? x y z) (mc-block :air)
               :else (mc-block :stone))))))

(defn distance-2d-sloped
  ([x0 z0 x1 z1 x-slope z-slope]
     (let [x-dist (* (Math/abs (- (double x0) (double x1)))
                     x-slope)
           z-dist (* (Math/abs (- (double z0) (double z1)))
                     z-slope)]
       (Math/sqrt (+ (* x-dist x-dist)
                     (* z-dist z-dist))))))

(defn epic-cave-generator
  ([cave-params x-max y-max z-max x-start z-start]
     (let [in-cave? (in-cave?-fn cave-params)
           x-bound (dec x-max)
           z-bound (dec z-max)
           ground-cap-x-slope (double (/ 40 x-max))
           ground-cap-z-slope (double (/ 40 z-max))]
       (fn [x y z]
         (let [sloping-cap (distance-2d-sloped
                                    x z x-start z-start
                                    ground-cap-x-slope
                                    ground-cap-z-slope)]
           (cond (or (zero? x) (zero? z) (= x-bound x) (= z-bound z))
                   :bedrock
                 (or (#{1 (dec x-bound)} x)
                     (#{1 (dec z-bound)} z))
                   :ground
                 (and (> y (- y-max 3))
                      (> y (+ y-max -3
                              (distance-2d-sloped
                               x z x-start z-start 2/31 2/31))))
                   :air
                 (and (> y (- y-max 7))
                      (> y (- (+ y-max 10) sloping-cap)))
                   :ground
                 (and (> y (- y-max 8))
                      (> y (- (+ y-max 9) sloping-cap))
                      (in-cave? x y z)
                      (not (in-cave? x (dec y) z)))
                   :ground
                 (and (> y (- y-max 10))
                      (> y (- (+ y-max 7) sloping-cap))
                      (< y (dec y-max))
                      (in-cave? x y z)
                      (not (in-cave? x (dec y) z))
                      (not (in-cave? x (inc y) z)))
                   :ground
                 (in-cave? x y z)
                   :air
                 :else
                   :ground))))))

(defn count-spawners
  ([zone]
     (let [counts (sum-counts
                   (pfor [x (range (zone-x-size zone))]
                     (sum-counts
                      (map (fn [ze]
                             (cond (and (map? ze)
                                        (= :mob-spawner
                                           (:type ze)))
                                   { (keyword
                                      (str (:mob ze)
                                           " spawner"))
                                     1}
                                   (= ze :glowstone)
                                   {:Glowstone 1}))
                           (for [z (range (zone-z-size zone))
                                 y (range (zone-y-size zone))]
                             (zone-lookup zone x y z))))))
           blocks (sort-by counts (keys counts))]
       (doseq [b blocks]
         (println (str (name b)
                       ": " (counts b)))))))

(defn generic-map-maker
  ([x-chunks z-chunks generator]
     (let [mcmap (gen-mcmap (* x-chunks +chunk-side+)
                            (* z-chunks +chunk-side+)
                            generator)]
       (count-spawners (:block-zone mcmap))
       (mcmap-to-mca-binary mcmap 0 0)))
  ([x-chunks y-height z-chunks generator]
     (let [mcmap (gen-mcmap (* x-chunks +chunk-side+)
                            y-height
                            (* z-chunks +chunk-side+)
                            generator)]
       (count-spawners (:block-zone mcmap))
       (mcmap-to-mca-binary mcmap 0 0))))

(defn random-cave
  ([x-max z-max seed]
     (random-cave x-max +old-chunk-height+ z-max seed))
  ([x-max y-max z-max seed]
     (loop [salt 0]
       (let [x0 (* x-max (srand 1 seed salt 1))
             z0 (* z-max (srand 1 seed salt 2))
             radius (+ *min-spiral-radius*
                       (srand (- (* *max-spiral-radius*
                                    (min x-max z-max))
                                 *min-spiral-radius*)
                              seed salt 3))]
         (if (and (> (- x0 radius) 5)
                  (> (- z0 radius) 5)
                  (> x-max (+ x0 radius 5))
                  (> z-max (+ z0 radius 5)))
           {:x0 x0
            :z0 z0
            :min-y 0
            :max-y y-max
            :radius radius
            :turn-rate (if (> 0.5 (srand 1 seed salt 4))
                         (- *turn-rate*) *turn-rate*)
            :theta-offset (srand (* Math/PI 2) seed salt 5)
            :width-offset (srand (* Math/PI 2) seed salt 6)
            :width-rate (+ 3.5 (srand 3 seed salt 7))
            :base-width *base-width*
            :width-mul *width-mul*
            :twistiness (* (srand 1 seed salt 8)
                           (srand 1 seed salt 9)
                           (srand 1 seed salt 10))}
           (recur (inc salt)))))))

(defn twist-new-center
  "Returns the new x0 and z0 (as a vector, [x0 z0]) resulting from
  twisting the given cave-params at the given y"
  ([cave-params y]
     (let [in-cave? (in-cave?-fn cave-params)
           {cx :cave-x, cz :cave-z, td :theta-divisor} (in-cave? y)
           r (:radius cave-params)
           x0 (:x0 cave-params)
           z0 (:z0 cave-params)
           dx (- cx x0)
           dz (- cz z0)
           new-x0 (+ cx dx)
           new-z0 (+ cz dz)]
       [new-x0 new-z0 td])))

(defn can-twist?
  ([cave-params y x-max z-max]
     (let [ [x0 z0] (twist-new-center cave-params y)
            radius (:radius cave-params)
            width-mul (:width-mul cave-params)
            base-width (:base-width cave-params)
            min-distance (* width-mul (dec base-width)
                            *edge-distance-fudge-factor*)]
       (and (> (- x0 radius) min-distance)
            (> (- z0 radius) min-distance)
            (> x-max (+ x0 radius min-distance))
            (> z-max (+ z0 radius min-distance))))))

(defn split-cave
  "Vertically segments a cave-params (which must be a single hash)
  into two connecting cave-params at altitude y, going in opposite
  directions and spiraling around different axes such that the cave is
  continuous at altitude y; i.e., switches the curvature of the cave
  at altitude y"
  ([cave-params y]
     (let [ [x0 z0 theta-divisor] (twist-new-center cave-params y)
            theta-offset (:theta-offset cave-params)]
       [(assoc cave-params
          :min-y y
          :turn-rate (- (:turn-rate cave-params))
          :theta-offset (+ (if (> theta-offset Math/PI)
                             (- Math/PI)
                             Math/PI)
                           theta-offset
                           (* 2 y (/ theta-divisor)))
          :x0 x0
          :z0 z0)
        (assoc cave-params
          :max-y y)])))

(defn twist-cave
  "Takes cave-params for a single spiral cave and returns a seq of
  cave-params for a single continuous twisting cave"
  ([cave-params max-x max-z seed]
     (twist-cave [cave-params] max-x max-z (+ 0.5 (srand 1 seed 1))
                 seed))
  ([cave-params max-x max-z y seed]
     (loop [cave-params cave-params
            y           y
            salt        0]
       (let [cave1 (first cave-params)
             caver (rest  cave-params)]
         (cond (>= y (:max-y cave1))
                 cave-params
               (or (< (:twistiness cave1) (srand 1 seed salt 1))
                   (not (can-twist? cave1 y max-x max-z)))
                 (recur cave-params (+ y 0.5 (srand 1 seed salt 2))
                        (inc salt))
               :else
                 (recur (concat (split-cave cave1 y)
                                caver)
                        (+ y 0.5 (srand 1 seed salt 3))
                        (inc salt)))))))

(defn pick-centermost-cave
  "Returns [centermost-cave other-caves]"
  ([caves max-x max-y max-z]
     (let [cx (/ max-x 2)
           cz (/ max-z 2)
           top-points (map #( (in-cave?-fn %)
                              (dec max-y))
                           caves)
           dists-squared (map #(let [ {x :cave-x, z :cave-z} %
                                      dx (- x cx)
                                      dz (- z cz)]
                                 (+ (* dx dx) (* dz dz)))
                              top-points)
           min-dist-squared (apply min dists-squared)]
       (loop [n 0
              dists dists-squared]
         (if (= min-dist-squared (first dists))
           ;; n is the one; return it
           [(nth caves n)
            (concat (take n caves)
                    (drop (inc n) caves))]
           (recur (inc n)
                  (rest dists)))))))

(defn find-closest-point
  "Returns the y altitude between 20 and (- y-max 28) where the two
  given caves are at their closest"
  ([cave-params-1 cave-params-2 y-max]
     (let [in-cave?-1 (in-cave?-fn cave-params-1)
           in-cave?-2 (in-cave?-fn cave-params-2)
           y-range (range 20 (- y-max 27))
           points-1 (map #(let [{x :cave-x, z :cave-z}
                                (in-cave?-1 %)]
                            [x z])
                         y-range)
           points-2 (map #(let [{x :cave-x, z :cave-z}
                                (in-cave?-2 %)]
                            [x z])
                         y-range)
           dists-squared (map #(let [ [x1 z1] %1
                                      [x2 z2] %2
                                      dx (- x1 x2)
                                      dz (- z1 z2)]
                                 (+ (* dx dx) (* dz dz)))
                              points-1 points-2)
           min-dist-squared (apply min dists-squared)]
       (loop [y-range y-range
              dists-squared dists-squared]
         (if (= min-dist-squared (first dists-squared))
           (first y-range)
           (recur (rest y-range)
                  (rest dists-squared)))))))

(defn caves-intersect?
  "Returns true only if the two given caves intersect; this function
  may sometimes generate false negatives, but will never generate
  false positives"
  ([cave-params-1 cave-params-2 y-max]
     (let [y (find-closest-point cave-params-1 cave-params-2 y-max)
           in-cave?-1 (in-cave?-fn cave-params-1)
           in-cave?-2 (in-cave?-fn cave-params-2)
           {x1 :cave-x, z1 :cave-z} (in-cave?-1 y)
           {x2 :cave-x, z2 :cave-z} (in-cave?-2 y)
           x1 (double x1)
           x2 (double x2)
           z1 (double z1)
           z2 (double z2)
           [dx dz t-max]
              (if (> (Math/abs (- x1 x2))
                     (Math/abs (- z1 z2)))
                [(Math/signum (- x2 x1))
                 (/ (- z2 z1)
                    (Math/abs (- x2 x1)))
                 (Math/abs (- x2 x1))]
                [(/ (- x2 x1)
                    (Math/abs (- z2 z1)))
                 (Math/signum (- z2 z1))
                 (Math/abs (- z2 z1))])]
       (loop [x x1
              z z1
              t 0]
         (let [ix (int (+ x 0.5))
               iz (int (+ z 0.5))
               in-1 (in-cave?-1 ix y iz)
               in-2 (in-cave?-2 ix y iz)]
           (cond (not (or in-1 in-2))
                   false
                 (and in-1 in-2)
                   true
                 (> t t-max)
                   false
                 :else
                   (recur (+ x dx)
                          (+ z dz)
                          (inc t))))))))

(defn epic-cave-network
  "Returns a zone and a starting point as [zone start-x start-z].  The
  zone mostly contains :ground, with caverns of :air carved out of it,
  all of which are interconnected and reachable from the starting
  point, with bedrock on all vertical sides, and capped with :ground
  on top except for caves with openings near the middle."
  ([n-caves max-x max-z seed opts]
     (epic-cave-network n-caves max-x +old-chunk-height+ max-z seed opts))
  ([n-caves max-x max-y max-z seed opts]
     (when (< max-y 49)
       (die "cave height " max-y " is below the minimum, 49"))
     (msg 1 "Generating cave network ...")
     (let [gen-twisted-cave #(twist-cave (random-cave max-x max-y max-z
                                                      (long
                                                       (srand +seed-max+
                                                              seed % 1)))
                                         max-x max-z
                                         (long (srand +seed-max+
                                                      seed % 2)))
           cave-seq (map gen-twisted-cave (range))
           candidate-caves (take n-caves cave-seq)
           cave-seq (drop n-caves cave-seq)
           _ (msg 8 "Finding centermost cave")
           [start candidate-caves]
             (pick-centermost-cave candidate-caves max-x max-y max-z)
           caves [start]
           {start-x :cave-x,
            start-z :cave-z}  ( (in-cave?-fn start)
                                (dec max-y))]
       (loop [caves caves
              candidate-caves candidate-caves
              cave-seq cave-seq
              i 0]
         (msg 6 "Picked " (count caves) " caves, finding"
              " intersections with cave #" (inc i))
         (cond (>= (count caves) n-caves)
                 ;; return here
                 (let [_ (msg 3 "Picked enough caves; optimizing ...")
                       caves (optimize-cave-params (take n-caves caves)
                                                   max-y)
                       generator1 (epic-cave-generator
                                     caves max-x max-y max-z start-x start-z)
                       generator (if (:hires opts)
                                   (do
                                     (msg 0 "hires carving enabled")
                                     (fn [x y z]
                                       (let [lower (generator1 x y z)
                                             upper (generator1 x (+ y 1/2)
                                                               z)]
                                         (cond (= upper lower)
                                                 lower
                                               (= :ground lower)
                                                 :half-ground
                                               (= :ground upper)
                                                 :upper-half-ground
                                               :else
                                                 (die "no case")))))
                                   generator1)
                       _ (msg 3 "Carving ...")
                       zone (gen-mcmap-zone max-x max-y max-z generator)]
                   [zone start-x start-z])
               (>= i (count caves))
                 (let [more-to-get (inc (int (/ n-caves 3)))]
                   (msg 3 "generating " more-to-get " more caves; currently"
                        " have " (count caves) " that intersect")
                   (recur (reverse caves)
                          (concat (take more-to-get cave-seq)
                                  candidate-caves)
                          (drop more-to-get cave-seq)
                          0))
               :else
                 (let [criterion-cave (nth caves i)
                       [caves-to-add remaining-candidates]
                         (loop [caves-to-add ()
                                remaining-candidates ()
                                caves-to-try candidate-caves]
                           (if (not (seq caves-to-try))
                             [caves-to-add remaining-candidates]
                             (if (caves-intersect? (first caves-to-try)
                                                   criterion-cave
                                                   max-y)
                               (recur (cons (first caves-to-try)
                                            caves-to-add)
                                      remaining-candidates
                                      (rest caves-to-try))
                               (recur caves-to-add
                                      (cons (first caves-to-try)
                                            remaining-candidates)
                                      (rest caves-to-try)))))]
                   (recur (concat caves caves-to-add)
                          remaining-candidates
                          cave-seq
                          (inc i))))))))

(defn cave-exercise-1
  ([x-chunks z-chunks]
     (let [cave-params {:x0 (* x-chunks +chunk-side+ 1/2)
                        :z0 (* z-chunks +chunk-side+ 1/2)
                        :radius (* x-chunks +chunk-side+ 1/4)}]
       (generic-map-maker x-chunks z-chunks
                          (inverse-cave-generator cave-params 0)))))

(defn cave-exercise-2
  ([x-chunks z-chunks]
     (let [cave-params {:x0 (* x-chunks +chunk-side+ 1/2)
                        :z0 (* z-chunks +chunk-side+ 1/2)
                        :radius (* x-chunks +chunk-side+ 3/8)}]
       (generic-map-maker x-chunks z-chunks
                          (simple-cave-generator cave-params 0)))))

(defn cave-exercise-3
  ([x-chunks z-chunks]
     (let [cave-params [{:x0 (* x-chunks +chunk-side+ 1/2)
                         :z0 (* z-chunks +chunk-side+ 1/2)
                         :radius (* x-chunks +chunk-side+ 3/8)}
                        {:x0 (* x-chunks +chunk-side+ 1/2)
                         :z0 (* z-chunks +chunk-side+ 1/2)
                         :radius (* x-chunks +chunk-side+ 3/16)}]]
       (generic-map-maker x-chunks z-chunks
                          (simple-cave-generator cave-params 0)))))

(defn cave-exercise-4
  ([x-chunks z-chunks]
     (let [max-x (* x-chunks +chunk-side+)
           max-z (* z-chunks +chunk-side+)
           gen-rand-cave #(random-cave max-x max-z
                                       (long (rand +seed-max+)))
           cave-params (repeatedly 15 gen-rand-cave)
           cave-params (map #(twist-cave % max-x max-z
                                         (long (rand +seed-max+)))
                            cave-params)]
       (generic-map-maker x-chunks z-chunks
                          (dark-cave-generator cave-params)))))

(defn cave-exercise-5
  ([x-chunks z-chunks cave-params]
     (let [max-x (* x-chunks +chunk-side+)
           max-z (* z-chunks +chunk-side+)]
       (generic-map-maker x-chunks z-chunks
                          (dark-cave-generator cave-params)))))

(defn cave-exercise-6
  ([x-chunks z-chunks]
     (let [seed (long (rand +seed-max+))]
       (println "Chose seed:" seed)
       (cave-exercise-6 x-chunks z-chunks seed)))
  ([x-chunks z-chunks seed]
     (let [max-x (* x-chunks +chunk-side+)
           max-z (* z-chunks +chunk-side+)
           [epic-zone start-x start-z]
                 (epic-cave-network *num-caves* max-x max-z seed)
           _ (msg 3 "Adding bedrock ...")
           bedrock-generator (fn [x y z]
                               (let [ze (zone-lookup epic-zone x y z)
                                     neighbors (neighbors-of epic-zone
                                                             x y z)]
                                 (if (every? #{:ground :bedrock}
                                             (cons ze neighbors))
                                   :bedrock
                                   ze)))
           epic-zone (gen-mcmap-zone max-x max-z bedrock-generator)
           _ (msg 3 "Adding creamy middle ...")
           x-bound (dec max-x)
           z-bound (dec max-z)
           generator (fn [x y z]
                       (let [ze (zone-lookup epic-zone x y z)
                             neighbors (neighbors-of epic-zone x y z)]
                         (if (or (zero? x) (zero? z)
                                 (= x-bound x) (= z-bound z))
                           (mc-block :bedrock)
                           (case ze
                                 :bedrock
                                   (if (every? #(= :bedrock %) neighbors)
                                     (mc-block :lava-source)
                                     :bedrock)
                                 :air (mc-block :air)
                                 :ground (mc-block :sandstone)))))]
       (println "Start is x=" start-x " z=" start-z)
       (generic-map-maker x-chunks z-chunks generator))))

(defn cave-exercise-6
  "Generates a full-1.2-height epic cave network"
  ([x-chunks z-chunks]
     (let [seed (choose-random-seed)]
       (println "Chose seed:" seed)
       (cave-exercise-6 x-chunks z-chunks seed)))
  ([x-chunks z-chunks seed]
     (let [max-x (* x-chunks +chunk-side+)
           max-y 256
           max-z (* z-chunks +chunk-side+)
           [epic-zone start-x start-z]
                 (epic-cave-network *num-caves* max-x max-y max-z seed)
           _ (msg 3 "Adding bedrock ...")
           bedrock-generator (fn [x y z]
                               (let [ze (zone-lookup epic-zone x y z)
                                     neighbors (neighbors-of epic-zone
                                                             x y z)]
                                 (if (every? #{:ground :bedrock}
                                             (cons ze neighbors))
                                   :bedrock
                                   ze)))
           epic-zone (gen-mcmap-zone max-x max-y max-z bedrock-generator)
           _ (msg 3 "Adding lava sea ...")
           x-bound (dec max-x)
           z-bound (dec max-z)
           generator (fn [x y z]
                       (let [ze (zone-lookup epic-zone x y z)
                             neighbors (neighbors-of epic-zone x y z)]
                         (if (or (zero? x) (zero? z)
                                 (= x-bound x) (= z-bound z))
                           (mc-block :bedrock)
                           (case ze
                                 :bedrock
                                   (if (and (> y (- max-y 10))
                                            (every? #(= :bedrock %)
                                                    neighbors))
                                     (mc-block :lava-source)
                                     :bedrock)
                                 :air (mc-block :air)
                                 :ground (mc-block :sandstone)))))
           _ (println "Start is x=" start-x " z=" start-z)
           mcmap (gen-mcmap max-x max-y max-z generator)]
       (mcmap-to-mca-binary mcmap 0 0))))
