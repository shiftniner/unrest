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
             theta-divisor (* 0.25 r)
             div-64-r (/ 64 r)
             base-width (or (:base-width cave-params) 3)
             width-mul (or (:width-mul cave-params) 6)]
         (fn [x y z]
           (let [x (- x x0)
                 z (- z z0)
                 theta (/ y theta-divisor)
                 cave-x (* r (Math/sin theta))
                 cave-z (* r (Math/cos theta))
                 x-dist (- x cave-x)
                 z-dist (- z cave-z)
                 width (* width-mul (+ base-width (Math/sin (/ y 5.0))))
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
