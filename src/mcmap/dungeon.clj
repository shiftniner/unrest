(ns mcmap.dungeon
  (:use mcmap.core
        mcmap.srand))

(def +hello-dungeon+
     [{:x0 6, :y0 0, :z0 0,
       :zone
         (gen-mcmap-zone 21 21 21
            (fn [x y z]
              (cond (and (= x 0)
                         (< 7 z 13)
                         (< 0 y 6))
                      :air
                    (some #{0 20} [x y z])
                      :moss-stone
                    (= [x y z] [10 1 10])
                      (mc-block :mob-spawner
                                :mob "Zombie" :delay 0)
                    :else
                      :air)))}
      {:x0 0, :y0 0, :z0 7,
       :zone
         (gen-mcmap-zone 6 6 6
            (fn [x y z]
              (cond (some #{0 5} [y z])
                      :moss-stone
                    (= [x y z] [3 3 1])
                      (mc-block :wall-sign
                                :text ["" "Hello," "Dungeon"]
                                :face :north)
                    :else
                    :air)))}])

(defn maybe-box-lookup
  ([box x y z]
     (let [{x0 :x0, y0 :y0, z0 :z0, zone :zone} box
           xd (zone-x-size zone)
           yd (zone-y-size zone)
           zd (zone-z-size zone)]
       (when (and (>= x x0) (>= y y0) (>= z z0)
                  (< x (+ x0 xd))
                  (< y (+ y0 yd))
                  (< z (+ z0 zd)))
         (zone-lookup zone (- x x0) (- y y0) (- z z0))))))

(defn place-dungeons
  "Takes a zone and a seq of dungeon boxes, and returns the zone with
the dungeons placed in it"
  ;; Might be better to have this return a fn for gen-mcmap[-zone]
  ([zone boxes]
     (gen-mcmap-zone (zone-x-size zone)
                     (zone-y-size zone)
                     (zone-z-size zone)
       (fn [x y z]
         (or (some #(maybe-box-lookup % x y z)
                   boxes)
             (zone-lookup zone x y z))))))

(defn dungeon-exercise-1
  "Makes an area with just empty air and a dungeon"
  ([x-chunks z-chunks dungeon]
     (let [x-size (* x-chunks +chunk-side+)
           z-size (* z-chunks +chunk-side+)
           zone (gen-mcmap-zone x-size z-size (fn [x y z] :air))
           zone (place-dungeons zone dungeon)
           mcmap (gen-mcmap x-size z-size
                            (fn [x y z]
                              (zone-lookup zone x y z)))]
       (mcmap-to-mcr-binary mcmap 0 0))))

