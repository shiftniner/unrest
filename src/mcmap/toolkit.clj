(ns mcmap.toolkit
  (:use mcmap.core
        mcmap.dungeon
        mcmap.srand
        mcmap.layout))

;;; XXX I probably need some way of controlling which mobs are allowed
;;; to appear; e.g., for a map where blaze rods are an objective.  The
;;; vector of allowed mobs could go in params.

(defn spawners
  ([x-size y-size z-size seed]
     (fnbox x-size y-size z-size [x y z params]
        (let [pain (:pain params)
              h (int (+ 0.5 (snorm [(* 2 pain) 1] seed x z)))
              spawner? (< y h)]
          (if (not spawner?)
            :air
            (let [mob-num (int (snorm [(dec (* 8 pain)) 2 0 8]
                                      seed x y z 1))
                  mob ( ["Enderman" "Zombie" "PigZombie"
                         "Spider" "Skeleton" "Ghast" "Creeper"
                         "Blaze" "CaveSpider"]
                          mob-num)]
              (mc-block :mob-spawner
                        :mob mob
                        :delay (int (snorm [(* 200 (- 1 pain))
                                            50 0]
                                           seed x y z 2)))))))))

(defn prize-chest
  "Returns a teensy dungeon consisting of a prize chest"
  ([]
     (fnbox 1 1 1 [_ _ _ params]
            (mc-block :chest
                      :items
                      (inventory-list
                       [{:id (mc-item :coal)
                         :slot 13}])))))

(defn add-entrance
  "Takes a dungeon, a vector (y and z specify the position for the
entrance, and x specifies the depth of the hole that needs to be
punched in the dungeon), entrance sign text, and a seed, and returns a
dungeon with an entrance added and with its location standardized"
  ([dungeon [depth ey ez] text seed]
     (let [aligned-dungeon (translate-dungeon dungeon
                                              0 (- ey) (- ez))
           hole-punch (fnbox depth 7 7
                        [x y z _]
                        (cond (some #{0 6} [y z])
                                (if (< x (dec depth))
                                  :bedrock
                                  nil)
                              (some #{1 5} [y z])
                                :stone-bricks
                              :else
                                :air))
           hole-punch (translate-dungeon hole-punch
                         (- (dungeon-min-x aligned-dungeon)
                            (dungeon-min-x hole-punch))
                         0
                         0)
           ;; XXX - no signs, no difficulty-signifying material
           entrance (fnbox 7 7 7
                      [x y z _]
                      (cond (some #{0 6} [y z])
                              :bedrock
                            (some #{1 5} [y z])
                              :stone-bricks
                            :else
                              :air))]
       (lineup :x :high
               entrance
               (dungeon-replace aligned-dungeon
                                hole-punch)))))

