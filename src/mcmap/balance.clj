(ns mcmap.balance
  (:use mcmap.srand))

(def metaarmor
              ; [relative durability, relative damage survivable]
     {:diamond-armor     [55    (/ 0.2)]
      :iron-armor        [25    (/ 0.4)]
      :chainmail-armor   [25    (/ 0.52)]
      :gold-armor        [11.75 (/ 0.56)]
      :leather-armor     [8.5   (/ 0.72)]
              ; [relative durability, shirts provided by diamond armor]
      :helmet            [1 3/2]
      :chestplate        [1 4]
      :leggings          [1 3]
      :boots             [1 3/2]})

(def +materials+ ["diamond" "iron" "chainmail" "gold" "leather"])
(def +armor-pieces+ ["helmet" "chestplate" "leggings" "boots"])

(def +durabilities+
     {:bow                     385
      :diamond-sword           1562
      :iron-sword              251
      :stone-sword             132
      :wood-sword              60
      :gold-sword              33
      :diamond-pickaxe         1563
      :iron-pickaxe            251
      :stone-pickaxe           131
      :gold-pickaxe            33
      :wood-pickaxe            60
      :chainmail-boots         196
      :chainmail-chestplate    241
      :chainmail-helmet        166
      :chainmail-leggings      226
      :diamond-boots           430
      :diamond-chestplate      529
      :diamond-helmet          364
      :diamond-leggings        496
      :gold-boots              92
      :gold-chestplate         81
      :gold-helmet             78
      :gold-leggings           106
      :iron-boots              196
      :iron-chestplate         241
      :iron-helmet             166
      :iron-leggings           226
      :leather-boots           66
      :leather-cap             56
      :leather-chestplate      81
      :leather-helmet          56
      :leather-leggings        76
      :leather-pants           76
      :leather-tunic           81})

(def armor-map
     (let [am
           (apply hash-map
             (apply concat
               (for [material +materials+
                     piece    +armor-pieces+]
                 (let [piece-power (metaarmor (keyword piece))
                       material-power (metaarmor (keyword
                                                  (str material
                                                       "-armor")))]
                   [ (keyword (str material "-" piece))
                     [ (* (first  piece-power) (first  material-power))
                       (* (second piece-power) (second material-power))]]))))]
       (assoc am
         :leather-cap (:leather-helmet am)
         :leather-tunic (:leather-chestplate am)
         :leather-pants (:leather-leggings am))))

(let [alt-val (fn [power-map & items-and-nums]
                (let [item-num-pairs (partition 2 items-and-nums)
                      vals (map (fn [ [item num] ]
                                  (/ (apply * (power-map item))
                                     num))
                                item-num-pairs)
                      versatility-adjust (+ 1 (/ (count item-num-pairs)
                                                 10))]
                  [1 (* versatility-adjust (apply max vals))]))
      power (fn [pm thing]
              (apply * (pm thing)))]
  (defn derive-material-vals
    ([pm]
       (let [pm (assoc pm
                  :gold-ingot (alt-val pm :gold-sword 2 :gold-helmet 5
                                       :gold-chestplate 8 :gold-leggings 7
                                       :gold-boots 4)
                  :diamond (alt-val pm :diamond-sword 2 :diamond-helmet 5
                                    :diamond-chestplate 8
                                    :diamond-leggings 7
                                    :diamond-boots 4)
                  :iron-ingot (alt-val pm :iron-sword 2 :iron-helmet 5
                                       :iron-chestplate 8 :iron-leggings 7
                                       :iron-boots 4 :bucket 3)
                  :leather (alt-val pm :leather-cap 5 :leather-tunic 8
                                    :leather-pants 7 :leather-boots 4)
                  :wood-plank (alt-val pm :wood-sword 2.5
                                       :bow 3/2))
             pm (assoc pm
                  :gold-block    [1 (* 9 (power pm :gold-ingot))]
                  :diamond-block [1 (* 9 (power pm :diamond   ))]
                  :iron-block    [1 (* 9 (power pm :iron-ingot))]
                  :wood          [1 (* 4 (power pm :wood-plank))])]
         pm))))

(let [pm (assoc armor-map
           :diamond-sword   [156.2 6]
           :iron-sword      [25.1  5]
           :stone-sword     [13.2  4]
           :wood-sword      [6     3]
           :gold-sword      [3.3   3]
           :diamond-pickaxe [156.2 20]
           :iron-pickaxe    [25.1  10]
           :stone-pickaxe   [13.2  5]
           :gold-pickaxe    [3.3   4]
           :wood-pickaxe    [6     3]
           :bucket          [2000  4]
           :bow             [38.5  4.5])]
  (def power-map (derive-material-vals pm)))

(defn enchant-value
  "Adjusts the given [longevity force] value for the given
enchantment"
  ;; This is full of magic numbers, but there doesn't seem to be a
  ;; much better place for them.  Several of these numbers are based
  ;; on my gut feeling as to what the enchantments will be worth in
  ;; the kinds of maps I expect to generate.  Fortune, in particular,
  ;; is worth less here than it would be in vanilla Minecraft because
  ;; I expect to have few if any ore blocks to mine.
  ;;
  ;; The protection enchants are the ones I'm the least sure of.
  ([[longevity force] enchant]
     (let [level (:level enchant)]
       (case (:type enchant)
             :protection
             [(* longevity (/ (+ force level) force))
              (+ force level)]
             (:fire-protection :feather-falling :blast-protection
              :projectile-protection :respiration)
             [(* longevity (/ (+ force (/ level 3)) force))
              (+ force (/ level 3))]
             :aqua-affinity
             [longevity (inc force)]
             (:sharpness :looting)
             [longevity (+ force (* level 3/4))]
             (:bane-of-arthropods :smite :knockback :fire-aspect :punch
              :flame)
             [longevity (+ force (* level 1/2))]
             :power
             [longevity (* force (+ 5/4 (* level 1/4)))]
             :infinity
             [longevity (* force 3)]
             :efficiency
             [longevity (* force (+ 1 (/ level 2)))]
             :silk-touch
             [longevity (* force 10)]
             :unbreaking
             [(* longevity (inc level)) force]
             :fortune
             [longevity (* force (+ 1 (/ level 10)))]))))

(defn item-power
  "Given an item, returns the power of that item"
  ([item]
     (if (keyword? item)
       (item-power {:type item})
       (let [{item-type :type
              enchants  :ench
              damage    :damage
              num       :count}
             item
             base-power (power-map item-type)
             durability (+durabilities+ item-type)
             dur-frac (if durability
                        (- 1 (/ (or damage 0)
                                durability))
                        1)
             adjusted-power (if enchants
                              (reduce enchant-value base-power enchants)
                              base-power)]
         (apply *
                (or num 1)
                dur-frac
                adjusted-power)))))

(defn damage-item
  "Takes an item and a damage fraction, 0 meaning undamaged and 1
meaning already broken, and returns the item with the given damage
level (regardless of any damage it might have already had)"
  ([item dfrac]
     (let [item (if (keyword? item)
                  {:type item}
                  item)
           durability (+durabilities+ (:type item))
           damage (if durability
                    (int (* dfrac durability))
                    0)]
       (assoc item :damage damage))))

(let [all-items (apply vector (keys power-map))]
  (defn get-item
    "Takes params, a seed, and salts, and returns a single unenchanted
item less powerful than (:reward params), along with new-params,
as [new-params item]"
    ([params seed & salts]
       (let [reward (:reward params)]
         (loop [i 0]
           (let [item (apply sranditem all-items seed (concat salts [i 1]))
                 item (if (< 1/2 (apply srand 1 seed (concat salts [i 2])))
                        (damage-item item (apply srand 1 seed
                                                 (concat salts [i 3])))
                        item)
                 power (item-power item)]
             (cond (< power reward)
                   [(assoc params :reward (- reward power))
                    item]
                   (< reward 0.5)
                   [params (apply sranditem [:string :spider-eye :bone
                                             :arrow :gunpowder]
                                  seed (concat salts [i 4]))]
                   :else
                   (recur (inc i)))))))))

(defn powerup-item
  "Takes params, an item, and a seed and salts and either increases
the count of the item and/or adds enchantments to it to
approach (params :reward); returns [new-params new-item]"
  ([params item seed & salts]
     
     ;; XXX
     ))

(defn get-items
  "Returns multiple, possibly enchanted items adding up to less
than (:reward params) in total power"
  ([params n-items seed & salts]
     (let [results (reduce
                    (fn [state n]
                      (let [ [new-params item]
                             (apply get-item state seed
                                    (concat salts [n]))]
                        (assoc new-params
                          :items (conj (:items state)
                                       item))))
                    (assoc params :items [])
                    (range n-items))
           ;; XXX - powerup-items here
           ]
       results)))
