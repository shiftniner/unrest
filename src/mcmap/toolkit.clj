(ns mcmap.toolkit
  (:use mcmap.core
        mcmap.util
        mcmap.blocks
        mcmap.dungeon.build
        mcmap.srand
        mcmap.layout
        mcmap.balance))

(def +standard-mobs+
     [[0.02 nil]
      [0.04 "Zombie"]
      [0.02 "Skeleton"]
      [0.02 "Spider"]
      [0.01 "Enderman"]
      [0.02 "Skeleton"]
      [0.02 "Spider"]
      [0.04 "Zombie"]
      [0.06 "Skeleton"]
      [0.03 "Spider"]
      [0.01 "PigZombie"]
      [0.03 "Spider"]
      [0.06 "Skeleton"]
      [0.18 "Spider"]
      [0.07 "Ghast"]
      [0.01 "Creeper"]
      [0.18 [[0.8  "Blaze"]
             [0.1  "Spider"]
             [0.07 "Skeleton"]
             [0.03 "Ghast"]]]
      [0.18 [[0.73 "CaveSpider"]
             [0.09 "Blaze"]
             [0.06 "Ghast"]
             [0.06 "Skeleton"]
             [0.03 "Spider"]
             [0.03 "Zombie"]]]])

(memo defn scale-mobs
  ([mobs]
     (let [sum (reduce + (map first mobs))
           scale (/ sum)
           scaled-mobs (map (fn [ [w mob]]
                              [(* scale w)
                               mob])
                            (drop-last mobs))]
       (concat scaled-mobs
               [ [ (reduce - 1.0 (map first scaled-mobs))
                   (second (last mobs))]]))))

(defn pick-mob
  ([mobs pain seed salt & salts]
     (if-not (seq mobs)
       (die "Ran out of mobs with " pain " pain left")
       (if (> pain (ffirst mobs))
         (recur (rest mobs)
                (- pain (ffirst mobs))
                seed salt salts)
         (let [result (second (first mobs))]
           (if (or (string? result)
                   (nil? result))
             result
             (apply pick-mob
                    (scale-mobs result)
                    (apply srand 1 seed salt salts)
                    seed
                    (inc salt)
                    salts)))))))

(defn spawners
  "Returns a chunk of spawners; frac scales the difficulty linearly,
  unlike (:pain params), for dividing difficulty among several groups
  of spawners in a single dungeon"
  ([x-size y-size z-size seed]
     (spawners x-size y-size z-size seed 1))
  ([x-size y-size z-size seed frac]
     (spawners x-size y-size z-size seed frac :air))
  ([x-size y-size z-size seed frac non-spawner]
     (fnbox x-size y-size z-size [x y z params]
        (let [pain (:pain params)
              ctr-dist (Math/sqrt (+ (square (- x (* 1/2 (dec x-size))))
                                     (square (* 2 y))
                                     (square (- z (* 1/2 (dec z-size))))))
              adjusted-pain (scale-pain (* 0.8 frac
                                           (Math/pow 0.5 ctr-dist))
                                        pain)
              adjusted-pain (scale-pain adjusted-pain
                                        (snorm [0.5 0.16 0.2 0.8]
                                               seed x y z 1))
              mobs (or (:mobs params)
                       +standard-mobs+)
              mobs (if (and (zero? ctr-dist)
                            (> pain (Math/pow 1e-13
                                              (* (srand 1 seed x y z 4)
                                                 frac)))
                            (nil? (second (first mobs))))
                     (rest mobs)
                     mobs)
              mobs (scale-mobs mobs)
              mob (pick-mob mobs adjusted-pain seed 1 x y z 3)]
          (if mob
            (mc-block :mob-spawner
                      :mob mob
                      :delay (int (snorm [(* 200 (- 1 pain))
                                          50 0]
                                         seed x y z 2)))
            non-spawner)))))

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

(defn unit-sum-series
  "Takes an integer N and returns the series [x, 2x, 3x, ... Nx] with
  sum 1"
  ([n]
     (unit-sum-series n 1))
  ([n total]
     (let [denom (reduce + (range (inc n)))]
       (map #(* %3 (/ %1 %2))
            (range 1 (inc n))
            (repeat denom)
            (repeat total)))))

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
  details), and an optional third argument specifying :sign-posts
  rather than the default :wall-signs, returns a vector of sign
  blocks (with air blocks for any gaps) containing the wrapped and
  formatted text"
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
  punched in the dungeon), entrance sign text, and a seed, and returns
  a dungeon with an entrance added and with its location standardized"
  ([dungeon [depth ey ez] text seed]
     (let [aligned-dungeon (translate-dungeon dungeon
                                              0 (- 2 ey) (- ez))
           hole-punch (fnbox depth 7 7
                        [x y z params]
                        (cond (some #{0 6} [y z])
                                (if (< x (dec depth))
                                  :bedrock
                                  nil)
                              (= y 1)
                                :cavern-wall
                              (some #{1 5} [z y])
                                (hall-material (:pain params)
                                               seed x y z 2)
                              :else
                                :air))
           hole-punch (translate-dungeon hole-punch
                         (- (dungeon-min-x aligned-dungeon)
                            (dungeon-min-x hole-punch))
                         0
                         0)
           signs (format-signs :north text)
           entrance-length (+ 6 (count signs))
           entrance (fnbox entrance-length 7 7
                      [x y z params]
                      (cond (some #{0 6} [y z])
                              :bedrock
                            (= y 1)
                              :cavern-wall
                            (some #{1 5} [z y])
                              (hall-material (:pain params)
                                             seed x y z)
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


(def +wool-color-labels+
     (map #(.replaceAll (str (name %) " wool")
                        "-" " ")
          +color-array+))

(def +ore-block-labels+
     ["iron block"
      "gold block"
      "diamond block"])

(def +victory-labels+ (concat +wool-color-labels+ +ore-block-labels+))

(defn victory-monument
  "Given a collection of block type labels, returns a victory monument
  dungeon"
  ([labels]
     (let [signs (format-signs :south (mapcat (fn [t] ["" t :newsign])
                                              (reverse labels)))]
       (stack (box (+ 4 (count signs))
                   1 5 :obsidian)
              (fnbox (+ 2 (count signs))
                     1 3
                 [x y z _]
                 (if (or (> z 0)
                         (= x 0)
                         (= x (inc (count signs))))
                   :glass
                   :torch))
              (-> (count signs)
                  (fnbox 1 1
                     [x y z _]
                     (get signs x))
                  (surround-fn
                   (fn [x y z _ x-max _ _]
                     (cond (#{0 (dec x-max)} x) :glass
                           (= z 2) :glass
                           :else :air))))))))

(defn rect-theta
  "Takes dimensions and a point, and returns the clockwise angle in
  radians between the vector (0,0)->(1,0) and the vector from the
  center of the dimensions and the point"
  ([x-dim z-dim x z]
     (Math/atan2 (- z (/ (dec z-dim) 2))
                 (- x (/ (dec x-dim) 2)))))

(defn rect-center?
  "Returns true if (x,z) is at the center of the area of size x-dim x
  z-dim"
  ([x-dim z-dim x z]
     (and (= x (/ (dec x-dim) 2))
          (= z (/ (dec z-dim) 2)))))

(defn rect-corner?
  "Returns true if (x,z) is in a corner of the area of size x-dim x
  z-dim"
  ([x-dim z-dim x z]
     (and ( #{0 (dec x-dim)} x)
          ( #{0 (dec z-dim)} z))))

(defn spiral-stairs
  "Returns a 3 by y-dim by 3 dungeon with spiral stairs using the
  given half block and full block; direction is either 1 for clockwise
  going up, or -1 for clockwise going down"
  ([y-dim half-block full-block center direction]
     (let [x-dim 3, z-dim 3]
       (fnbox x-dim y-dim z-dim [x y z _]
         (let [theta (/ (rect-theta x-dim z-dim x z)
                        (* Math/PI 2))]
           (if (= (mod (int (+ 4.5 (* theta 4)))
                       4)
                  (mod (* y direction)
                       4))
             (cond (rect-center? x-dim z-dim x z) center
                   (rect-corner? x-dim z-dim x z) half-block
                   :else                          full-block)
             :air))))))

(defn stone-ore-box
  "Returns a box consisting mainly of stone, with some coal and iron
  veins in much greater concentrations than typically seen in vanilla
  Minecraft"
  ([x-dim y-dim z-dim seed]
     (let [twopi (* Math/PI 2)
           thetas (map #(srand twopi seed %)
                       (range))
           x-areas (-> x-dim (+ 9) (/ 10) int)
           y-areas (-> y-dim (+ 9) (/ 10) int)]
       (fnbox x-dim y-dim z-dim [x y z _]
         (let [x-area (-> x (+ 9) (/ 10) int)
               y-area (-> y (+ 9) (/ 10) int)
               z-area (-> z (+ 9) (/ 10) int)
               to-drop (* 36 (+ x-area
                                (* y-area x-areas)
                                (* z-area y-areas x-areas)))
               coal-wave (delay
                          (apply + (map #(Math/sin (+ %1 (/ %2 %3)))
                                        (drop to-drop thetas)
                                        (cycle [x y z])
                                        (dup-seq [1.1 0.6 1.5 1.8] 3))))
               iron-wave (delay
                          (apply + (map #(Math/sin (+ %1 (/ %2 %3)))
                                        (drop (+ to-drop 12) thetas)
                                        (cycle [x y z])
                                        (dup-seq [0.3 0.2 0.4 0.6] 3))))
               stone-wave (apply + (map #(Math/sin (+ %1 (/ %2 %3)))
                                        (drop (+ to-drop 24) thetas)
                                        (cycle [x y z])
                                        (dup-seq [1.1 0.6 1.5 1.8] 3)))]
           (cond (< -3.0 stone-wave 3.0) :stone
                 (< -2.0 @coal-wave 2.0) :coal-ore
                 (< -1.0 @iron-wave 1.0) :iron-ore
                 :else                   :stone))))))

(defn remove-mobs-from-distribution
  "Takes a mob distribution, e.g. +standard-mobs+, and returns the
  distribution with the given mobs (a set of strings) removed"
  ([dist mobs]
     (when (seq dist)
       (lazy-seq
        (let [freq (ffirst dist)
              mob (second (first dist))
              the-rest (remove-mobs-from-distribution (rest dist) mobs)]
          (cond (coll? mob)
                  (cons [freq (remove-mobs-from-distribution mob mobs)]
                        the-rest)
                (mobs mob)
                  the-rest
                :else
                  (cons (first dist)
                        the-rest)))))))

(defn remove-mobs
  "Takes a dungeon and a seq of mob *strings*, and returns a dungeon
  with no spawners of that mob type"
  ([dungeon mobs]
     (modify-params dungeon [params]
       (assoc params :mobs
              (remove-mobs-from-distribution (or (:mobs params)
                                                 +standard-mobs+)
                                             (set mobs))))))
