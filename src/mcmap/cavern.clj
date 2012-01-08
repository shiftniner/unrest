(ns mcmap.cavern
  (:use mcmap.core))

(defn in-cave?-fn
  ([cave-params]
     (if (or (vector? cave-params)
             (seq? cave-params))
       (if (seq cave-params)
         (let [f-fn (in-cave?-fn (first cave-params))
               r-fn (in-cave?-fn (rest cave-params))]
           (fn [x y z]
             (or (f-fn x y z)
                 (r-fn x y z))))
         (fn [x y z]
           false))
       (let [x0 (:x0 cave-params)
             z0 (:z0 cave-params)
             r (:radius cave-params)
             turn-rate (or (:turn-rate cave-params) 1)
             theta-divisor (* turn-rate (* 0.25 r))
             theta-offset (or (:theta-offset cave-params) 0)
             div-64-r (/ 64 r)
             width-rate (or (:width-rate cave-params) 5.0)
             width-offset (or (:width-offset cave-params) 0)
             base-width (or (:base-width cave-params) 3)
             width-mul (or (:width-mul cave-params) 6)]
         (fn [x y z]
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
             (< dist-squared (* width width))))))))

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
           radius (rand (* 1/2 (max x-max z-max)))
           turn-rate (if (> 0.5 (rand))
                       -1 1)
           theta-offset (rand (* Math/PI 2))
           width-offset (rand (* Math/PI 2))
           width-rate (+ 3.5 (rand 3))]
       (if (and (pos? (- x0 radius))
                (pos? (- z0 radius))
                (> x-max (+ x0 radius))
                (> z-max (+ z0 radius)))
         {:x0 x0
          :z0 z0
          :radius radius
          :turn-rate turn-rate
          :theta-offset theta-offset
          :width-offset width-offset
          :width-rate width-rate}
         (recur x-max z-max)))))

(defn cave-exercise-4
  ([x-chunks z-chunks]
     (let [gen-rand-cave #(random-cave (* x-chunks +chunk-side+)
                                       (* z-chunks +chunk-side+))
           cave-params (repeatedly 15 gen-rand-cave)]
       (generic-map-maker x-chunks z-chunks
                          (dark-cave-generator cave-params)))))
