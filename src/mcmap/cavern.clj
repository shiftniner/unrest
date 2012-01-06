(ns mcmap.cavern
  (:use mcmap.core))

(defn in-cave?
  ([cave-params x y z]
     (let [x0 (:x0 cave-params)
           z0 (:z0 cave-params)
           rx (or (:radius-x cave-params) (:radius cave-params))
           rz (or (:radius-z cave-params) (:radius cave-params))
           rh (Math/sqrt (+ (* rx rx) (* rz rz)))
           x (- x x0)
           z (- z z0)
           theta (/ y (* 0.09 rh))
           cave-x (* rx (Math/sin (/ y 3.0)))
           cave-z (* rz (Math/cos (/ y 3.0)))
           x-dist (- x cave-x)
           z-dist (- z cave-z)
           dist-squared (+ (* x-dist x-dist)
                           (* z-dist z-dist))
           dist-skew (+ (* cave-x x-dist 2 (/ 32 rx))
                        (* cave-z z-dist 2 (/ 32 rz)))
           dist-squared (+ dist-squared (Math/abs dist-skew))
           width (* 6 (+ 2 (Math/sin (/ y 5.0))))]
       (< dist-squared (* width width)))))

(defn inverse-cave-generator
  ([cave-params]
     (fn [x y z]
       (cond (in-cave? cave-params x y z)
               (if (> 0.01 (rand))
                 (mc-block :glowstone)
                 (mc-block :stone))
             :else (mc-block :air)))))

(defn simple-cave-generator
  ([cave-params]
     (fn [x y z]
       (cond (in-cave? cave-params x y z)
               (if (> 0.01 (rand))
                 (mc-block :mob-spawner :mob "Giant" :delay 200)
                 (mc-block :air))
             :else (if (> 0.01 (rand))
                     (mc-block :glowstone)
                     (mc-block :stone))))))

(defn generic-cave-maker
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
       (generic-cave-maker x-chunks z-chunks
                           (inverse-cave-generator cave-params)))))

(defn cave-exercise-2
  ([x-chunks z-chunks]
     (let [cave-params {:x0 (* x-chunks +chunk-side+ 1/2)
                        :z0 (* z-chunks +chunk-side+ 1/2)
                        :radius (* x-chunks +chunk-side+ 1/4)}]
       (generic-cave-maker x-chunks z-chunks
                           (simple-cave-generator cave-params)))))
