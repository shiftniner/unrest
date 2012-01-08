(ns mcmap.cavern
  (:use mcmap.core))

(defn in-cave?-fn
  "Takes a cave-params hash (see random-cave for an example) or a seq
of cave-params hashes, and returns a fn of x y z that returns true if
the coordinates are within the walls of a cave"
  ([cave-params]
     (if (or (vector? cave-params)
             (seq? cave-params))
       (if (seq cave-params)
         (let [fns (map in-cave?-fn cave-params)]
           (fn [x y z]
             (loop [in-cave? (first fns)
                    rest-fns (rest fns)]
               (cond (in-cave? x y z) true
                     (seq rest-fns) (recur (first rest-fns)
                                           (rest rest-fns))
                     :else false)))))
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

(defn generic-map-maker
  ([x-chunks z-chunks generator]
     (let [mcmap (gen-mcmap (* x-chunks +chunk-side+)
                            (* z-chunks +chunk-side+)
                            generator)]
       (mcmap-to-mcr-binary mcmap 0 0))))

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

(defn random-cave
  ([x-max z-max]
     (let [x0 (* x-max (rand))
           z0 (* z-max (rand))
           radius (rand (* 1/2 (max x-max z-max)))]
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
