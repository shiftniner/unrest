(ns mcmap.unrest
  (:use mcmap.core
        mcmap.cavern
        mcmap.srand
        mcmap.blocks))

;;; Replace this with a function of local difficulty later
(def +max-cavern-light+ 10)

(defn gen-unrest-map
  ([params]
     (let [seed (long (rand +seed-max+))]
       (println "Chose seed:" seed)
       (gen-unrest-map seed params)))
  ([seed params]
     (let [x-chunks 16
           z-chunks 16
           max-x (* x-chunks +chunk-side+)
           max-y 256
           max-z (* z-chunks +chunk-side+)
           [epic-zone start-x start-z]
               (epic-cave-network *num-caves* max-x max-y max-z seed)
           _ (println "Start is x=" start-x " z=" start-z)
           _ (msg 3 "Adding bedrock crust ...")
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
           mcmap (gen-mcmap max-x max-y max-z generator)]
       (mcmap-to-mca-binary mcmap 0 0))))
