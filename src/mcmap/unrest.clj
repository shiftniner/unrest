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
           max-z (* z-chunks +chunk-side+)
           [epic-zone start-x start-z]
               (epic-cave-network *num-caves* max-x max-z seed)
           _ (msg 3 "Computing cavern mood lighting ...")
           cavern-mood-light
               (rising-recursive-gen-mcmap-zone max-x max-z
                 (fn [x y z prev-light]
                   (let [ze (zone-lookup epic-zone x y z)]
                     (if (= :air ze)
                       (min +max-cavern-light+ (inc (or prev-light -3)))
                       nil))))
           _ (msg 3 "Adding bedrock crust ...")
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
                                 :air
                                   (if-let [light (zone-lookup
                                                     cavern-mood-light
                                                     x y z)]
                                     (mc-block :air :light (max 0 light))
                                     (mc-block :air))
                                 :ground (mc-block :sandstone)))))]
       (println "Start is x=" start-x " z=" start-z)
       (generic-map-maker x-chunks z-chunks generator))))
