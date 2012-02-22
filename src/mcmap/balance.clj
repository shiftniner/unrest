(ns mcmap.balance
  (:use mcmap.srand
        mcmap.blocks))

(def metaarmor
              ; [relative durability, relative damage survivable]
     {:diamond-armor     [55    (/ 0.2)]
      :iron-armor        [25    (/ 0.4)]
      :chainmail-armor   [25    (/ 0.52)]
      :gold-armor        [11.75 (/ 0.56)]
      :leather-armor     [8.5   (/ 0.72)]
              ; [relative durability, shirts provided by diamond armor]
      :helmet            [2 3/2]
      :chestplate        [2 4]
      :leggings          [2 3]
      :boots             [2 3/2]})

(def +materials+ ["diamond" "iron" "chainmail" "gold" "leather"])
(def +armor-pieces+ ["helmet" "chestplate" "leggings" "boots"])

(def +max-stack-size+ 64)
(def +food-value+ 3)

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
                  ;; Crafting, and therefore wood, is now overpowered,
                  ;; since mobs drop iron.  I picked 100 as an
                  ;; arbitrary fairly-large number; once you craft a
                  ;; workbench you can make 100 buckets or chestplates
                  ;; or swords before getting bored with the game.
                  :wood-plank (alt-val pm :wood-sword 2.5
                                       :bow 3/2 :bucket 4/100
                                       :iron-chestplate 4/100
                                       :iron-sword 4/100))
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
           :diamond-pickaxe [156.2 200]
           :iron-pickaxe    [25.1  500/4]
           :stone-pickaxe   [13.2  600/5]
           :wood-pickaxe    [6     600/5]
           :gold-pickaxe    [3.3   600/5]
           :bucket          [300   400]
           :bow             [38.5  4.5]
           :tnt             [0.1   20]
           :flint-and-steel [6.5   4]
           :ice             [1     100]
           :redstone        [3     1]
           :button          [10    5]
           :wood-pressure-plate [5 5]
           :bread           [(+ 2.5 6)   +food-value+]
           :cake            [(+ 6   2.4) +food-value+]
           :cookie          [(+ 0.5 0.2) +food-value+]
           :melon-slice     [(+ 1   1.2) +food-value+]
           :mushroom-soup   [(+ 4   9.6) +food-value+]
           :raw-chicken     [(+ 1   1.2) (* 0.7 +food-value+)]
           :cooked-chicken  [(+ 3   7.2) +food-value+]
           :raw-beef        [(+ 1.5 1.8) +food-value+]
           :steak           [(+ 4  12.8) +food-value+]
           :raw-porkchop    [(+ 1.5 1.8) +food-value+]
           :cooked-porkchop [(+ 4  12.8) +food-value+]
           :raw-fish        [(+ 1   1.2) +food-value+]
           :cooked-fish     [(+ 2.5 6)   +food-value+]
           :red-apple       [(+ 2   2.4) +food-value+]
           :golden-apple    [(+ 2  24)   +food-value+]
           :rotten-flesh    [(+ 2   0.8) (* 0.2 +food-value+)]
           :spider-eye      [(+ 1   0.8) (* 1/2 +food-value+)]
           :arrow           [0.1  5]
           :ladder          [0.2  50]
           :vines           [0.1  150]
           :sandstone       [0.1  100])
      potions (mapcat
               (fn [ [item power]]
                 [item [1 power]])
               (concat
                ;; Potions where all four combinations work
                (mapcat (fn [ [base baseval]]
                          [ [(keyword (str "potion-of-" base)) baseval]
                            [(keyword (str "potion-of-" base "-ii"))
                             (* 1.4 baseval)]
                            [(keyword (str "ext-potion-of-" base))
                             (* 2 baseval)]
                            [(keyword (str "ext-potion-of-" base "-ii"))
                             (* 2.8 baseval)]
                            [(keyword (str "splash-potion-of-" base))
                             baseval]
                            [(keyword (str "splash-potion-of-" base
                                           "-ii"))
                             (* 1.4 baseval)]
                            [(keyword (str "splash-ext-potion-of-" base))
                             (* 2 baseval)]
                            [(keyword (str "splash-ext-potion-of-" base
                                           "-ii"))
                             (* 2.8 baseval)]])
                        [["regeneration" 150]
                         ["swiftness" 90]
                         ["strength" 100]])
                ;; Potions that are only altered by time extension
                (mapcat (fn [ [base baseval]]
                          [ [(keyword (str "potion-of-" base)) baseval]
                            [(keyword (str "ext-potion-of-" base))
                             (* 2 baseval)]
                            [(keyword (str "splash-potion-of-" base))
                             baseval]
                            [(keyword (str "splash-ext-potion-of-" base))
                             (* 2 baseval)]])
                        [["fire-resistance" 500]])
                ;; Potions that are only altered by strengthening
                (mapcat (fn [ [base baseval]]
                          [ [(keyword (str "potion-of-" base)) baseval]
                            [(keyword (str "potion-of-" base "-ii"))
                             (* 2 baseval)]
                            [(keyword (str "splash-potion-of-" base))
                             baseval]
                            [(keyword (str "splash-potion-of-" base
                                           "-ii"))
                             (* 2 baseval)]])
                        [["healing" 250]])
                ;; Splash-only (offensive) potions
                (mapcat (fn [ [base baseval]]
                          [ [(keyword (str "splash-potion-of-" base))
                             baseval]
                            [(keyword (str "splash-potion-of-" base
                                           "-ii"))
                             (* 1.4 baseval)]
                            [(keyword (str "splash-ext-potion-of-" base))
                             (* 2 baseval)]
                            [(keyword (str "splash-ext-potion-of-" base
                                           "-ii"))
                             (* 2.8 baseval)]])
                        [["poison" 80]])
                ;; Splash-only (offensive) potions that are only
                ;; altered by time extension
                (mapcat (fn [ [base baseval]]
                          [ [(keyword (str "splash-potion-of-" base))
                             baseval]
                            [(keyword (str "splash-ext-potion-of-" base))
                             (* 2 baseval)]])
                        [["weakness" 70]
                         ["slowness" 50]])
                ;; Splash-only (offensive) potions that are only
                ;; altered by strengthening
                (mapcat (fn [ [base baseval]]
                          [ [(keyword (str "splash-potion-of-" base))
                             baseval]
                            [(keyword (str "splash-potion-of-" base
                                           "-ii"))
                             (* 2 baseval)]])
                        [["harming" 100]])))
      pm (apply assoc pm potions)]
  ;; This adds ingots, diamonds, blocks, and leather, which are
  ;; useless without a crafting table; I'm withholding crafting
  ;; because mobs drop iron starting in MCr1.2, and much that can be
  ;; crafted with iron is OP.
  ;; (def power-map (derive-material-vals pm))
  (def power-map pm))

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
  ([params item]
     (if (keyword? item)
       (item-power params {:type item})
       (let [{item-type :type
              enchants  :ench
              damage    :damage
              num       :count}
             item
             power-map  (or (:power-map params) power-map)
             base-power (or (power-map item-type) [0.1 0.1])
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

(let [all-items (apply vector (keys power-map))
      swords (apply vector (filter #(.endsWith (str %) "-sword")
                                   all-items))
      foods [:bread :cake :cookie :melon-slice :mushroom-soup
             :raw-chicken :cooked-chicken :raw-beef :steak
             :raw-porkchop :cooked-porkchop :raw-fish :cooked-fish
             :red-apple :golden-apple :rotten-flesh :spider-eye]]
  (defn get-item
    "Takes params, a seed, and salts, and returns a single unenchanted
item less powerful than (:reward params), along with new-params,
as [new-params item]"
    ([params seed & salts]
       (let [reward (:reward params)]
         (loop [i 0]
           (let [item-type-roll (apply srand 1 seed
                                       (concat salts [i 5]))
                 items (cond (> i 100)
                               all-items
                             (:reward-list params)
                               (:reward-list params)
                             (> 0.2 item-type-roll)
                               (conj swords :bow)
                             (> 0.4 item-type-roll)
                               foods
                             :else
                               all-items)
                 item (apply sranditem items seed (concat salts [i 1]))
                 item (if (< 1/2 (apply srand 1 seed (concat salts [i 2])))
                        (damage-item item (apply srand 1 seed
                                                 (concat salts [i 3])))
                        item)
                 power (item-power params item)]
             (cond (< power reward)
                   [(assoc params :reward (- reward power))
                    item]
                   (< reward 0.5)
                   [params (apply sranditem [:string :bone :gunpowder]
                                  seed (concat salts [i 4]))]
                   :else
                   (recur (inc i))))))))
  (let [pickaxes (filter #(.endsWith (str %) "-pickaxe")
                         all-items)
        boots (filter #(.endsWith (str %) "-boots")
                      all-items)
        helmets (cons :leather-cap
                      (filter #(.endsWith (str %) "-helmet")
                              all-items))
        armor (concat boots helmets [:leather-pants :leather-tunic]
                      (filter #(.endsWith (str %) "-chestplate")
                              all-items)
                      (filter #(.endsWith (str %) "-leggings")
                              all-items))
        armor?   (apply hash-set armor)
        boots?   (apply hash-set boots)
        helmet?  (apply hash-set helmets)
        sword?   (apply hash-set swords)
        pickaxe? (apply hash-set pickaxes)
        bow?     #{:bow}]
    (defn available-enchants
      "Takes an item and returns a vector of available enchantments
that can be applied to the item (which may include enchantments not
available in vanilla Minecraft for that item), and have not already
been applied to it"
      ([item]
         (let [item-type (if (keyword? item) item (:type item))
               possible-enchants
               (concat (when (armor? item-type)
                         [:protection :fire-protection
                          :blast-protection
                          :projectile-protection
                          :unbreaking])
                       (when (boots? item-type)
                         [:feather-falling])
                       (when (helmet? item-type)
                         [:respiration :aqua-affinity])
                       (when (sword? item-type)
                         [:sharpness :smite :bane-of-arthropods
                          :knockback :fire-aspect :looting
                          :unbreaking])
                       (when (bow? item-type)
                         [:power :punch :flame :infinity
                          :unbreaking :knockback :sharpness :smite
                          :bane-of-arthropods :knockback :fire-aspect
                          :looting])
                       (when (pickaxe? item-type)
                         [:efficiency :silk-touch :unbreaking
                          :fortune :sharpness :smite
                          :bane-of-arthropods
                          :knockback :fire-aspect :looting]))
               applied-enchants (when (map? item)
                                  (map :type (:ench item)))
               applied-enchant? (apply hash-set applied-enchants)
               applied-enchant? (cond (applied-enchant? :silk-touch)
                                      (conj applied-enchant? :fortune)
                                      (applied-enchant? :fortune)
                                      (conj applied-enchant? :silk-touch)
                                      :else applied-enchant?)]
           (filter (comp not applied-enchant?)
                   possible-enchants))))))

(def max-enchant-level
     (apply hash-map
       (apply concat
         (concat
           (map #(do [% 10])
                [:protection :fire-protection :feather-falling
                 :blast-protection :projectile-protection
                 :respiration :sharpness :smite :bane-of-arthropods
                 :knockback :fire-aspect :looting :power :punch
                 :efficiency :unbreaking :fortune])
           (map #(do [% 1])
                [:aqua-affinity :flame :infinity :silk-touch])))))

(defn add-enchant
  "Returns the given item with the given enchantment added"
  ([item enchantment level]
     (let [item (if (keyword? item)
                  {:type item}
                  item)
           item (if (:ench item)
                  item
                  (assoc item :ench []))]
       (assoc item
         :ench (conj (:ench item)
                     {:type enchantment
                      :level level})))))

(defn powerup-item
  "Takes params, an item, and a seed and salts and either increases
the count of the item and/or adds enchantments to it to
approach (params :reward); returns [new-params new-item]"
  ([params item seed & salts]
     (let [item (if (keyword? item) {:type item} item)
           orig-p (item-power params item)
           reward (:reward params)]
       (if (> 0.5 (apply srand 1 seed (concat salts [1])))
         (let [n (min (inc (int (/ reward orig-p)))
                      +max-stack-size+)]
           [(assoc params :reward (- reward (* orig-p (dec n))))
            (assoc item :count n)])
         (loop [salt2 1
                item item
                new-params params
                randroll 1]
           (if (> 0.5 randroll)
             [new-params item]
             (let [enchants (available-enchants item)]
               (if (not (seq enchants))
                 [params item]
                 (let [enchantment (apply sranditem (vec enchants)
                                          seed (concat salts [salt2 1]))
                       level (inc (int (apply srand
                                              (max-enchant-level enchantment)
                                              seed (concat salts [salt2 2]))))
                       powered-item (add-enchant item enchantment level)
                       new-power (item-power params powered-item)
                       power-added (- new-power orig-p)
                       [new-reward item]
                         (if (and (pos? level)
                                  (< power-added reward))
                           [(- reward power-added) powered-item]
                           [(:reward new-params) item])]
                   (recur (inc salt2)
                          item
                          (assoc new-params :reward new-reward)
                          (apply srand 1 seed
                                 (concat salts [salt2 3]))))))))))))

(defn get-items
  "Returns multiple, possibly enchanted items adding up to less
than (:reward params) in total power"
  ([params n-items seed & salts]
     (let [results (reduce
                    (fn [state n]
                      (let [ [new-params item]
                             (apply get-item state seed
                                    (concat salts [n 1]))]
                        (assoc new-params
                          :items (conj (:items state)
                                       item))))
                    (assoc params :items [])
                    (range n-items))
           items (:items results)
           results (reduce
                    (fn [state n]
                      (let [item (items n)
                            reward-adjust (* (/ (inc n)
                                                (count items))
                                             (:reward state))
                            state (assoc state
                                    :reward (- (:reward state)
                                               reward-adjust))
                            [new-params pow-item]
                              (apply powerup-item state item seed
                                     (concat salts [n 2]))]
                        (assoc new-params
                          :items (conj (:items state)
                                       pow-item)
                          :reward (+ (:reward new-params)
                                     reward-adjust))))
                    (assoc results :items [])
                    (range n-items))]
       (:items results))))

(defn best-items
  "Takes a collection of collections of items and returns the best
collection"
  ([params items-seqs]
     (first (sort (comparator #(> (reduce + (map item-power
                                                 (repeat params) %1))
                                  (reduce + (map item-power
                                                 (repeat params) %2))))
                  items-seqs))))

(let [enchant-ids
      {:protection                      0
       :fire-protection                 1
       :feather-falling                 2
       :blast-protection                3
       :projectile-protection           4
       :respiration                     5
       :aqua-affinity                   6
       :sharpness                       16
       :smite                           17
       :bane-of-arthropods              18
       :knockback                       19
       :fire-aspect                     20
       :looting                         21
       :efficiency                      32
       :silk-touch                      33
       :unbreaking                      34
       :fortune                         35
       :power                           48
       :punch                           49
       :flame                           50
       :infinity                        51}]
  (defn convert-enchantment
    "Takes a single enchantment as would be added to an item by
add-enchant, and returns the same enchantment in the format expected
by inventory-list"
    ([ench]
       {:id (enchant-ids (:type ench))
        :lvl (:level ench)})))

(defn balance-item-to-inventory-item
  "Takes an item as would be returned by get-item or get-items, and
returns an item compatible with the inventory-list function"
  ([item]
     (let [item-type (if (keyword? item)
                       item
                       (:type item))
           mc-item (mc-item item-type)
           item-id (if (coll? mc-item)
                     (mc-item 0)
                     mc-item)
           damage (if (coll? mc-item)
                    (mc-item 1)
                    (or (and (map? item)
                             (:damage item))
                        0))
           count (or (and (map? item)
                          (:count item))
                     1)
           ench (when (and (map? item)
                           (:ench item))
                  (map convert-enchantment (:ench item)))]
       {:id item-id
        :damage damage
        :count count
        :ench ench})))
