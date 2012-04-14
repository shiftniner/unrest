(ns mcmap.dungeons
  (:use mcmap.layout
        mcmap.toolkit
        mcmap.blocks
        mcmap.core
        mcmap.srand
        mcmap.util))

;;; mcmap.dungeons contains definitions of various dungeons.
;;; mcmap.dungeon contains code for placing dungeons in a map.

(letfn [(dungeon-type-match
         ;; returns true if d either is, or is a
         ;; collection that contains, dtype
         ([dtype d]
            (some (partial = dtype)
                  (vec-if-not-coll d))))]
  (defmacro defdungeons
    "Defines dungeons using the following syntax:

  (defdungeons
   optional-docstring
   (type-or-list-of-types
    name
    fn) ...)

  Where type-or-list-of-types is either a keyword of a collection of
  keywords, name is a symbol, the fn is of two parameters (the y
  coordinate of the dungeon and the seed) returning a dungeon."
    ([& dungeons]
       (let [dungeons (filter #(not (string? %)) dungeons)
             dungeon-types (distinct (mapcat? first dungeons))
             names (map second dungeons)
             _ (when-let [dup (first (duplicates names))]
                 (throw (IllegalArgumentException.
                         (str "Dungeon name " dup
                              " encountered more than once"))))
             types-and-dungeons
               (forcat [dungeon-type dungeon-types]
                 [dungeon-type
                  (vec (map (comp keyword second)
                            (filter #(dungeon-type-match dungeon-type %)
                                    dungeons)))])
             names-and-dungeons
               (forcat [dungeon dungeons]
                 (let [dungeon-name (second dungeon)
                       dungeon-types (vec-if-not-coll (first dungeon))
                       dungeon-fn (nth dungeon 2)]
                   [(keyword dungeon-name)
                    {:types dungeon-types
                     :fn dungeon-fn}]))]
         `(do
            (def +dungeon-type-map+
                 (hash-map ~@types-and-dungeons))
            (def +dungeon-map+
                 (hash-map ~@names-and-dungeons)))))))

(defdungeons

  "Simple box with spawners in the middle and chests around the edge,
  reminiscent of the vanilla Minecraft dungeon, but larger, darker,
  and usually with more spawners"
  (:std
   hello-dungeon
   (let [[r1 r2] (unit-sum-series 2)]
     (fn [_ seed]
       (-> (stack (htable [(pad 7 2 19)]
                          [(pad 1 1 6)
                           (spawners 5 3 5 (reseed seed 1))
                           (pad 1 1 5)
                           (-> (supply-chest :north (reseed seed 4))
                               (reward * r1))]
                          [(pad 6 2 19)]
                          [(-> (prize-chest :east (reseed seed 3))
                               (reward * r2))])
                  (pad 19 14 19))
           (surround :nether-brick)
           (surround :bedrock)
           (surround :ground)
           (add-entrance [3 1 0]
                         ["" "Simple" "Exercises"]
                         (reseed seed 2))))))

  "A zig-zagging hallway with multiple clusters of spawners"
  (:uncommon
   back-and-forth
   (let [wall-n #(strict-dungeon (htable [(box % 5 3 :wall)]))
         short-wall (wall-n 5)
         long-wall  (wall-n 25)
         space-5  (strict-dungeon (pad 1 5 5))
         space-13 (strict-dungeon (pad 1 5 13))
         [p1 p2 p3] (unit-sum-series 3 1.5)
         [r1 r2]    (unit-sum-series 2)]
     (fn [_ seed]
       (-> (htable [ space-13 short-wall space-5]
                   [ (spawners 5 3 5 (reseed seed 5) p3)
                     long-wall
                     (spawners 5 3 5 (reseed seed 3) p2)
                     long-wall
                     (spawners 5 3 5 (reseed seed 4) p1)]
                   [ (htable [space-5]
                             [(-> (prize-chest :east (reseed seed 1))
                                  (reward * r2))])
                     short-wall
                     (htable [space-13]
                             [(pad 1 5 7)
                              (-> (supply-chest :east (reseed seed 6))
                                  (reward * r1))])])
           (surround :wall)
           (dungeon-map-neighbors
            (fn [b ns]
              (cond (not= b :wall)       b
                    (every? #{:wall} ns) :bedrock
                    :else                :sandstone)))
           (surround :bedrock)
           (surround :ground)
           (add-entrance [3 0 -8]
                         ["" "Back and" "Forth"]
                         (reseed seed 2))))))

  "A zig-zagging hallway going the other way, with multiple clusters
  of spawners"
  (:uncommon
   back-and-forth-backwards
   (let [wall-n #(strict-dungeon (htable [(box % 5 3 :wall)]))
         short-wall (wall-n 5)
         long-wall  (wall-n 25)
         space-5  (strict-dungeon (pad 1 5 5))
         space-13 (strict-dungeon (pad 1 5 13))
         [p1 p2 p3] (unit-sum-series 3 1.5)
         [r1 r2]    (unit-sum-series 2)]
     (fn [_ seed]
       (-> (htable [ space-5 short-wall space-13]
                   [ (spawners 5 3 5 (reseed seed 5) p1)
                     long-wall
                     (spawners 5 3 5 (reseed seed 3) p2)
                     long-wall
                     (spawners 5 3 5 (reseed seed 4) p3)]
                   [ (htable [space-13]
                             [(-> (prize-chest :east (reseed seed 1))
                                  (reward * r1))
                              (pad 1 5 8)])
                     short-wall
                     (htable [space-5]
                             [(-> (supply-chest :east (reseed seed 6))
                                  (reward * r2))])])
           (surround :wall)
           (dungeon-map-neighbors
            (fn [b ns]
              (cond (not= b :wall)       b
                    (every? #{:wall} ns) :bedrock
                    :else                :sandstone)))
           (surround :bedrock)
           (surround :ground)
           (add-entrance [3 0 8]
                         ["" "Back and" "Forth"]
                         (reseed seed 2))))))

  "A familiar spiral staircase, then another one going back"
  (:rare
   stairs-and-flowers
   (let [ [r1 r2] (unit-sum-series 2)]
     (fn [y seed]
       (letfn [ (stairs [y-dim]
                  (spiral-stairs y-dim
                                 (mc-block :smooth-stone-half-slab)
                                 (mc-block :smooth-stone-double-slab)
                                 :air 1))]
         (if (> y 40)
           ;; climb down and then back up
           (-> (htable [ (stone-ore-box 1 26 5 (reseed seed 7 1))]
                       [ (stone-ore-box 3 26 1 (reseed seed 7 2))
                         (stairs 26)
                         (stone-ore-box 3 26 1 (reseed seed 7 3))]
                       [ (apply stack
                                (pad 1 4 1)
                                (concat (map #(spawners 1 3 5
                                                        (reseed seed 1 %1)
                                                        %2 :stone)
                                             (range 1 8)
                                             [1/12 1/36 1/108 1/4
                                              1/108 1/36 1/12])
                                        [ (box 1 1 5 :stone)]))]
                       [ (stack (htable [ (-> (supply-chest :south
                                                            (reseed seed 5))
                                              (reward * r1))
                                          (pad 1  1 1)
                                          (box (mc-block
                                                (sranditem [:rose :dandelion]
                                                           seed 3)
                                                :force-nospread-light 8))
                                          (pad 1  1 2)])
                                (pad 1  3 5)
                                (box 1  1 5 :stone)
                                (box 1 21 5 :bedrock))]
                       [ (apply stack
                                (pad 1 4 1)
                                (concat (map #(spawners 1 3 5
                                                        (reseed seed 4 %1)
                                                        %2 :stone)
                                             (range 1 8)
                                             [1/12 1/36 1/108 1/4
                                              1/108 1/36 1/12])
                                        [ (box 1 1 5 :stone)]))]
                       [ (stone-ore-box 3 26 1 (reseed seed 7 4))
                         (stack (stairs 24)
                                (-> (prize-chest :south
                                                 (reseed seed 6))
                                    (reward * r2)))
                         (stone-ore-box 3 26 1 (reseed seed 7 5))]
                       [ (stone-ore-box 1 26 5 (reseed seed 7 6))])
               (surround :bedrock)
               (dungeon-map-neighbors
                (fn [b ns]
                  (if (and (not= :air b)
                           (some #( #{:rose :dandelion}
                                    (block-type %))
                                 ns))
                    :dirt
                    b)))
               (surround :bedrock)
               (remove-mobs ["Ghast"])
               (surround :ground)
               (add-entrance [4 23 0]
                             ["" "Stairs and Flowers"]
                             (reseed seed 2)))
           ;; (<= y 40): climb up and then back down
           (-> (htable [ (stone-ore-box 1 26 5 (reseed seed 7 1))]
                       [ (stone-ore-box 3 26 1 (reseed seed 7 2))
                         (stairs 26)
                         (stone-ore-box 3 26 1 (reseed seed 7 3))]
                       [ (apply stack
                                (box 1 1 5 :stone)
                                (concat (map #(spawners 1 7 5
                                                        (reseed seed 1 %)
                                                        1/3 :stone)
                                             (range 1 4))
                                        [ (pad 1 4 1)]))]
                       [ (stack (box 1 21 5 :bedrock)
                                (box 1  1 5 :stone)
                                (htable [ (-> (supply-chest :south
                                                            (reseed seed 5))
                                              (reward * r1))
                                          (pad 1  1 1)
                                          (box (mc-block
                                                (sranditem [:rose :dandelion
                                                            :dead-bush]
                                                           seed 3)
                                                :force-nospread-light 8))
                                          (pad 1  1 2)])
                                (pad 1  3 5))]
                       [ (apply stack
                                (box 1 1 5 :stone)
                                (concat (map #(spawners 1 7 5
                                                        (reseed seed 4 %)
                                                        1/3 :stone)
                                             (range 1 4))
                                        [ (pad 1 4 1)]))]
                       [ (stone-ore-box 3 26 1 (reseed seed 7 4))
                         (dungeon-replace (stairs 25)
                                          (-> (prize-chest :south
                                                           (reseed seed 6))
                                              (reward * r2)))
                         (stone-ore-box 3 26 1 (reseed seed 7 5))]
                       [ (stone-ore-box 1 26 5 (reseed seed 7 6))])
               (surround :bedrock)
               (dungeon-map-neighbors
                (fn [b ns]
                  (cond (= :air b)
                          b
                        (some #( #{:rose :dandelion}
                                 (block-type %))
                              ns)
                          :dirt
                        (some #( #{:dead-bush}
                                 (block-type %))
                              ns)
                          :sand
                        :else
                          b)))
               (surround :bedrock)
               (remove-mobs ["Ghast"])
               (surround :ground)
               (add-entrance [4 0 0]
                             ["" "Stairs and Flowers"]
                             (reseed seed 2))))))))

  "A zig-zagging hallway going up"
  (:uncommon
   back-and-forth-ii
   (let [wall (strict-dungeon (htable [(box 25 3 5 :wall)]))
         space-2  (strict-dungeon (pad  2 5 1))
         space-7  (strict-dungeon (pad  7 5 1))
         space-10 (strict-dungeon (pad 10 5 1))
         space-3z (strict-dungeon (pad  1 1 3))
         [p1 p2 p3] (unit-sum-series 3)
         [r1 r2]    (unit-sum-series 2)]
     (fn [_ seed]
       (-> (stack
            (htable [ space-10]
                    [ (spawners 5 3 5 (reseed seed 1) p1)]
                    [ space-10]
                    [ (box 5 5 1
                           (mc-block :vines :faces #{:south}))
                      space-3z
                      (box 5 5 1
                           (mc-block :vines :faces #{:north}))])
            (htable [ wall]
                    [ (box 5 3 1
                           (mc-block :vines :faces #{:south}))
                      space-3z
                      (box 5 3 1
                           (mc-block :vines :faces #{:north}))])
            (htable [ (box 5 5 1
                           (mc-block :vines :faces #{:south}))
                      space-3z
                      (box 5 5 1
                           (mc-block :vines :faces #{:north}))]
                    [ space-7]
                    [ (spawners 5 3 5 (reseed seed 2) p2)]
                    [ space-7]
                    [ (-> (supply-chest :west (reseed seed 3))
                          (reward * r1))]
                    [ (box 5 5 1
                           (mc-block :vines :faces #{:south}))
                      space-3z
                      (box 5 5 1
                           (mc-block :vines :faces #{:north}))])
            (htable [ (box 5 3 1
                           (mc-block :vines :faces #{:south}))
                      space-3z
                      (box 5 3 1
                           (mc-block :vines :faces #{:north}))]
                    [ wall])
            (htable [ (box 5 5 1
                           (mc-block :vines :faces #{:south}))
                      space-3z
                      (box 5 5 1
                           (mc-block :vines :faces #{:north}))]
                    [ space-10]
                    [ (spawners 5 3 5 (reseed seed 5) p3)]
                    [ space-7]
                    [ (-> (prize-chest :east (reseed seed 6))
                          (reward * r2))]
                    [ space-2]))
           (surround :wall)
           (dungeon-map-neighbors
            (fn [b ns]
              (cond (not= b :wall)       b
                    (every? #{:wall} ns) :bedrock
                    :else                :iron-block)))
           (surround :bedrock)
           (surround :ground)
           (add-entrance [3 0 0]
                         ["" "Back and" "Forth 2"]
                         (reseed seed 4))))))

  "Tard bridges over lava (or the void), with spawners on top"
  (:std
   spite-and-malice
   (letfn [(bridge-section [seed frac lava?]
             (stack (box 9 3 17 (if lava? :lava-source :air))
                    (pad 1 8 1)
                    (fnbox 9 1 17 [x _ z params]
                      (let [p (:pain params)
                            r (srand 1 seed 1 x z)]
                        (if (= 2 (mod z 3))
                          (if (and (zero? (mod (+ x (quot z 3))
                                               3))
                                   (< r (Math/pow p 3)))
                            :air
                            :cobble)
                          (if (< r (Math/pow p 0.5))
                            :air
                            :iron-bars))))
                    (spawners 5 3 5 (reseed seed 2)
                              frac)))]
     (let [ [r1 r2] (unit-sum-series 2)
            [f1 f2] (unit-sum-series 2)]
       (fn [y seed]
         (let [open-to-void? (< y 22)
               remove-bottom-side (if open-to-void? 2 0)
               bottom-trim (if open-to-void?
                             (min 9 (- 10 y))
                             0)
               dungeon-resizer (cond (neg? bottom-trim)
                                       #(extrude-dungeon % :y :low
                                                         (- bottom-trim))
                                     (pos? bottom-trim)
                                       #(trim-dungeon % 0 0 0 bottom-trim 0 0)
                                     :else
                                       identity)
               lava? (not open-to-void?)]
           (-> (htable [(box 6 12 17 :ground)]
                       [(bridge-section (reseed seed 1) f1 lava?)]
                       [(stack (box 5 12 17 :ground)
                               (-> (supply-chest :east (reseed seed 2))
                                   (reward * r1)))]
                       [(bridge-section (reseed seed 3) f2 lava?)]
                       [(stack (box 5 12 17 :ground)
                               (-> (prize-chest :east (reseed seed 4))
                                   (reward * r2)))])
               (stack (box (+ (* 9 2) 6 5 5)
                           1 17 :ground))
               boxify
               (remove-mobs ["Ghast"])
               (dungeon-map-neighbors
                (fn [b ns]
                  (cond (not= b :ground)       b
                        (every? #{:ground} ns) :bedrock
                        :else                  :stone)))
               (surround :bedrock)
               (surround :ground)
               (trim-dungeon 0 0 0 remove-bottom-side 0 0)
               dungeon-resizer
               (add-entrance [2 12 0]
                             ["" "Spite &" "Malice"]
                             (reseed seed 5))))))))

  "An inconvenient (especially on harder levels) victory monument
  room"
  (:home
   life-in-a-glass-house
   (fn [_ seed labels]
     (-> (stack (htable [(victory-monument labels)
                         (pad 1 1 10)
                         (supply-chest (reseed seed 2))])
                (pad 10 10 10))
         (align :y :low)
         (surround :glass)
         (surround-fn (fn [x y z params & _]
                        (if (> (:pain params)
                               (srand 1 seed x y z))
                          :lava-source
                          :sandstone)))
         (surround :bedrock)
         (surround :ground)
         (add-entrance [4 -3 0]
                       ["" "Life in a Glass House"]
                       (reseed seed 1))))))

(defn get-dungeons
  "Returns a vector of keywords naming all dungeons of the given
  type"
  ([t]
     (+dungeon-type-map+ t))
  ([t1 t2 & ts]
     (vec (mapcat get-dungeons (list* t1 t2 ts)))))

(defn get-dungeon
  "Given a dungeon name (keyword), a y coordinate, a seed, and any
  extra arguments required by that dungeon, returns a dungeon"
  ([dungeon-name y seed & extra-args]
     (let [dungeon (+dungeon-map+ dungeon-name)
           f (:fn dungeon)]
       (when-not dungeon
         (throw (RuntimeException. (str "Dungeon " dungeon-name
                                        "not found"))))
       (apply f y seed extra-args))))

(defn hello-dungeon
  "Temporary replacement for the former hello-dungeon fn, for testing"
  ([y-level seed]
     (msg -10 "Deprecated hello-dungeon fn called")
     (get-dungeon :hello-dungeon y-level seed)))
