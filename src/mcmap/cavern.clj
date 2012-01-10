(ns mcmap.cavern
  (:use mcmap.core))

(defn in-cave?-fn
  "Takes a cave-params hash (see random-cave for an example) or a seq
of cave-params hashes, and returns a fn of x y z that returns true if
the coordinates are within the walls of a cave.  If cave-params is a
single cave, the function returned also takes a single y argument and
returns a vector [cx cz td] with the x and z coordinates of the center
of the cave at that y, and the theta divisor for the cave, used in
computing twists."
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
       ;; not (or (vector? ...) ...) -- i.e., cave-params is a single cave:
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
                      width (* width-mul (+ base-width
                                            (Math/sin (+ width-offset
                                                         (/ y width-rate)))))
                      dist-squared (+ (* x-dist x-dist)
                                      (* z-dist z-dist))
                      dist-skew (+ (* cave-x x-dist div-64-r)
                                   (* cave-z z-dist div-64-r))
                      dist-squared (+ dist-squared (Math/abs dist-skew))]
                  (< dist-squared (* width width)))))
           ([y]
              (let [theta (+ theta-offset (/ y theta-divisor))
                    cave-x (* r (Math/sin theta))
                    cave-z (* r (Math/cos theta))]
                {:cave-x (+ cave-x x0)
                 :cave-z (+ cave-z z0)
                 :theta-divisor theta-divisor})))))))

(defn inverse-cave-generator
  ([cave-params]
     (let [in-cave? (in-cave?-fn cave-params)]
       (fn [x y z]
         (cond (in-cave? x y z)
                 (if (> 0.01 (rand))
                   (mc-block :glowstone)
                   (mc-block :stone))
               :else (mc-block :air))))))

(defn simple-cave-generator
  ([cave-params]
     (let [in-cave? (in-cave?-fn cave-params)]
       (fn [x y z]
         (cond (in-cave? x y z)
                 (mc-block :air)
               :else (if (> 0.01 (rand))
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
     (let [x-dist (* (Math/abs (- x0 x1))
                     x-slope)
           z-dist (* (Math/abs (- z0 z1))
                     z-slope)]
       (Math/sqrt (+ (* x-dist x-dist)
                     (* z-dist z-dist))))))

(defn epic-cave-generator
  ([cave-params x-max z-max x-start z-start]
     (let [in-cave? (in-cave?-fn cave-params)
           x-bound (dec x-max)
           z-bound (dec z-max)
           ground-cap-x-slope (/ 40 x-max)
           ground-cap-z-slope (/ 40 z-max)]
       (fn [x y z]
         (cond (or (zero? x) (zero? z) (= x-bound x) (= z-bound z))
                 :bedrock
               (and (> y 125)
                    (> y (+ 125 (distance-2d-sloped
                                    x z x-start z-start 2/31 2/31))))
                 :air
               (and (> y 121)
                    (> y (- 138 (distance-2d-sloped
                                    x z x-start z-start
                                    ground-cap-x-slope
                                    ground-cap-z-slope))))
                 :ground
               (in-cave? x y z)
                 :air
               :else
                 :ground)))))

(defn generic-map-maker
  ([x-chunks z-chunks generator]
     (let [mcmap (gen-mcmap (* x-chunks +chunk-side+)
                            (* z-chunks +chunk-side+)
                            generator)]
       (mcmap-to-mcr-binary mcmap 0 0))))

(defn random-cave
  ([x-max z-max]
     (let [x0 (* x-max (rand))
           z0 (* z-max (rand))
           radius (+ 8 (rand (+ -8 (* 1/2 (min x-max z-max)))))]
       (if (and (> (- x0 radius) 5)
                (> (- z0 radius) 5)
                (> x-max (+ x0 radius 5))
                (> z-max (+ z0 radius 5)))
         {:x0 x0
          :z0 z0
          :min-y 0
          :max-y 128
          :radius radius
          :turn-rate (if (> 0.5 (rand))
                       -1 1)
          :theta-offset (rand (* Math/PI 2))
          :width-offset (rand (* Math/PI 2))
          :width-rate (+ 3.5 (rand 3))
          :twistiness (* (rand) (rand) (rand))}
         (recur x-max z-max)))))

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
            radius (:radius cave-params)]
       (and (> (- x0 radius) 5)
            (> (- z0 radius) 5)
            (> x-max (+ x0 radius 5))
            (> z-max (+ z0 radius 5))))))

(defn split-cave
  "Vertically segments a cave-params (which must be a single hash)
into two connecting cave-params at altitude y, going in opposite
directions and spiraling around different axes such that the cave is
continuous at altitude y; i.e., switches the curvature of the cave at
altitude y"
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
  ([cave-params max-x max-z]
     (twist-cave [cave-params] max-x max-z (+ 0.5 (rand))))
  ([cave-params max-x max-z y]
     (let [cave1 (first cave-params)
           caver (rest  cave-params)]
       (cond (>= y (:max-y cave1))
               cave-params
             (or (< (:twistiness cave1) (rand))
                 (not (can-twist? cave1 y max-x max-z)))
               (recur cave-params max-x max-z (+ y 0.5 (rand)))
             :else
               (recur (concat (split-cave cave1 y)
                              caver)
                      max-x
                      max-z
                      (+ y 0.5 (rand)))))))

(defn pick-centermost-cave
  "Returns [centermost-cave other-caves]"
  ([caves max-x max-z]
     (let [cx (/ max-x 2)
           cz (/ max-z 2)
           top-points (map #((in-cave?-fn %) 127)
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
  "Returns the y altitude between 20 and 100 where the two given caves
are at their closest"
  ([cave-params-1 cave-params-2]
     (let [in-cave?-1 (in-cave?-fn cave-params-1)
           in-cave?-2 (in-cave?-fn cave-params-2)
           y-range (range 20 101)
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
may sometimes generate false negatives, but will never generate false
positives"
  ([cave-params-1 cave-params-2]
     (let [y (find-closest-point cave-params-1 cave-params-2)
           in-cave?-1 (in-cave?-fn cave-params-1)
           in-cave?-2 (in-cave?-fn cave-params-2)
           {x1 :cave-x, z1 :cave-z} (in-cave?-1 y)
           {x2 :cave-x, z2 :cave-z} (in-cave?-2 y)
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
         (let [in-1 (in-cave?-1 x y z)
               in-2 (in-cave?-2 x y z)]
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
all of which are interconnected and reachable from the starting point,
with bedrock on all vertical sides, and capped with :ground on top
except for caves with openings near the middle."
  ([n-caves max-x max-z]
     (msg 10 "Generating cave network ...")
     (let [gen-twisted-cave #(twist-cave (random-cave max-x max-z)
                                         max-x max-z)
           candidate-caves (repeatedly n-caves gen-twisted-cave)
           _ (msg 8 "Finding centermost cave")
           [start candidate-caves] (pick-centermost-cave candidate-caves
                                                         max-x max-z)
           caves [start]
           {start-x :cave-x,
            start-z :cave-z}  ( (in-cave?-fn start) 127)]
       (loop [caves caves
              candidate-caves candidate-caves
              i 0]
         (msg 3 "Accumulated " (count caves) " caves, finding"
              " intersections with cave #" (inc i))
         (cond (>= (count caves) n-caves)
                 ;; return here
                 (let [_ (msg 3 "Picked enough caves; now carving")
                       caves (take n-caves caves)
                       generator (epic-cave-generator
                                    caves max-x max-z start-x start-z)
                       zone (gen-mcmap-zone max-x max-z generator)]
                   [zone start-x start-z])
               (>= i (count caves))
                 (let [more-to-get (inc (int (/ n-caves 3)))]
                   (msg 1 "generating " more-to-get " more caves; currently"
                        " have " (count caves) " that intersect")
                   (recur (reverse caves)
                          (concat (repeatedly more-to-get gen-twisted-cave)
                                  candidate-caves)
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
                                                   criterion-cave)
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
                          (inc i))))))))

(defn cave-exercise-1
  ([x-chunks z-chunks]
     (let [cave-params {:x0 (* x-chunks +chunk-side+ 1/2)
                        :z0 (* z-chunks +chunk-side+ 1/2)
                        :radius (* x-chunks +chunk-side+ 1/4)}]
       (generic-map-maker x-chunks z-chunks
                          (inverse-cave-generator cave-params)))))

(defn cave-exercise-2
  ([x-chunks z-chunks]
     (let [cave-params {:x0 (* x-chunks +chunk-side+ 1/2)
                        :z0 (* z-chunks +chunk-side+ 1/2)
                        :radius (* x-chunks +chunk-side+ 3/8)}]
       (generic-map-maker x-chunks z-chunks
                          (simple-cave-generator cave-params)))))

(defn cave-exercise-3
  ([x-chunks z-chunks]
     (let [cave-params [{:x0 (* x-chunks +chunk-side+ 1/2)
                         :z0 (* z-chunks +chunk-side+ 1/2)
                         :radius (* x-chunks +chunk-side+ 3/8)}
                        {:x0 (* x-chunks +chunk-side+ 1/2)
                         :z0 (* z-chunks +chunk-side+ 1/2)
                         :radius (* x-chunks +chunk-side+ 3/16)}]]
       (generic-map-maker x-chunks z-chunks
                          (simple-cave-generator cave-params)))))

(defn cave-exercise-4
  ([x-chunks z-chunks]
     (let [max-x (* x-chunks +chunk-side+)
           max-z (* z-chunks +chunk-side+)
           gen-rand-cave #(random-cave max-x max-z)
           cave-params (repeatedly 15 gen-rand-cave)
           cave-params (map #(twist-cave % max-x max-z) cave-params)]
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
     (let [max-x (* x-chunks +chunk-side+)
           max-z (* z-chunks +chunk-side+)
           [epic-zone start-x start-z]
                 (epic-cave-network 15 max-x max-z)
           generator (fn [x y z]
                       (let [ze (zone-lookup epic-zone x y z)]
                         (case ze
                               :ground (mc-block :sandstone)
                               :air (mc-block :air)
                               :bedrock (mc-block :bedrock))))]
       (println "Start is x=" start-x " z=" start-z)
       (generic-map-maker x-chunks z-chunks generator))))
