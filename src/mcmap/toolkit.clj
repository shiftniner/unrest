(ns mcmap.toolkit
  (:use mcmap.core
        mcmap.blocks
        mcmap.dungeon
        mcmap.srand
        mcmap.layout
        mcmap.balance))

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

(defn prize-chest-items
  "Returns random contents for a prize chest, ignoring :prize but
using :reward-list if present"
  ([params seed]
     (let [many-items (map #(get-items params (rand-item-count seed 1 %)
                                       seed 2 %)
                           (range 100))
           half-reward (/ (:reward params) 2)
           low-reward-items (take-while #(> (nth % 1) half-reward)
                                        many-items)
           good-enough-items (take (inc (count low-reward-items))
                                   many-items)
           lowest-leftover-reward (apply min (map #(nth % 1)
                                                  good-enough-items))
           items (ffirst (filter #(= lowest-leftover-reward
                                     (nth % 1))
                                 good-enough-items))]
       (sort (comparator #(> (item-power params %1)
                             (item-power params %2)))
             items))))

(defn prize-chest
  "Returns a teensy dungeon consisting of a prize chest"
  ([seed]
     (prize-chest nil seed))
  ([face seed]
     (fnbox 1 1 1 [_ _ _ params]
       (let [items (or (:prize params)
                       (prize-chest-items params seed))
             inv-list (map balance-item-to-inventory-item items)]
         (mc-block :chest
                   :face face
                   :items
                   (inventory-list
                    (map #(assoc %1 :slot %2)
                         inv-list
                         [13, 14 12, 4 22, 15 11, 5 3 23 21, 16 10,
                          6 2 24 20, 17 9, 7 1 25 19, 8 0 26 18])))))))

(defn supply-chest
  "Returns a 1x1x1 dungeon consisting of a prize chest, ignoring
any :prize"
  ([seed]
     (supply-chest nil seed))
  ([face seed]
     (modify-params (prize-chest face seed) [p]
       (dissoc p :prize))))

(defn format-signs
  "Given a :face direction, some text (see sign-wrap-text for format
details), and an optional third argument specifying :sign-posts rather
than the default :wall-signs, returns a vector of sign blocks (with
air blocks for any gaps) containing the wrapped and formatted text"
  ([face text]
     (format-signs face text :wall-sign))
  ([face text sign-type]
     (let [wrapped-text (sign-wrap-text text)]
       (vec (map (fn [t]
                   (if t
                     (mc-block sign-type
                               :face face
                               :text (vec t))
                     :air))
                 wrapped-text)))))

(defn add-entrance
  "Takes a dungeon, a vector (y and z specify the position for the
entrance, and x specifies the depth of the hole that needs to be
punched in the dungeon), entrance sign text, and a seed, and returns a
dungeon with an entrance added and with its location standardized"
  ([dungeon [depth ey ez] text seed]
     (let [aligned-dungeon (translate-dungeon dungeon
                                              0 (- 2 ey) (- ez))
           hole-punch (fnbox depth 7 7
                        [x y z _]
                        (cond (some #{0 6} [y z])
                                (if (< x (dec depth))
                                  :bedrock
                                  nil)
                              (= y 1)
                                :sandstone
                              (some #{1 5} [z y])
                                :stone-bricks
                              :else
                                :air))
           hole-punch (translate-dungeon hole-punch
                         (- (dungeon-min-x aligned-dungeon)
                            (dungeon-min-x hole-punch))
                         0
                         0)
           signs (format-signs :north text)
           entrance-length (+ 6 (count signs))
           ;; XXX - no difficulty-signifying material
           entrance (fnbox entrance-length 7 7
                      [x y z _]
                      (cond (some #{0 6} [y z])
                              :bedrock
                            (= y 1)
                              :sandstone
                            (some #{1 5} [z y])
                              :stone-bricks
                            (and (= [3 2] [y z])
                                 (< 2 x (+ 3 (count signs))))
                              (get signs (- x 3))
                            :else
                              :air))]
       (-> (lineup :x :high
                   entrance
                   (dungeon-replace aligned-dungeon
                                    hole-punch))
           (translate-dungeon 0 0 3)))))

