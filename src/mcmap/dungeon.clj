(ns mcmap.dungeon
  (:use mcmap.core
        mcmap.srand
        mcmap.cavern))

(def +dungeon-placement-retries+ 20)

(def +hello-dungeon+
     [(fn [params])
      {:x0 6,  :y0 0,  :z0 0,
       :xd 21, :yd 21, :zd 21,
       :zone
         (atom
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
                       :air))))}
      {:x0 0, :y0 0, :z0 7,
       :xd 6, :yd 7, :zd 7,
       :zone
         (atom
           (gen-mcmap-zone 6 7 7
             (fn [x y z]
               (cond (some #{0 6} [y z])
                       :moss-stone
                     (= [x y z] [3 3 1])
                       (mc-block :wall-sign
                                 :text ["" "Hello," "Dungeon"]
                                 :face :south)
                     :else
                       :air))))}])

(defn maybe-box-lookup
  ([box x y z]
     (let [{x0 :x0, y0 :y0, z0 :z0, zone :zone} box]
       (maybe-zone-lookup @zone (- x x0) (- y y0) (- z z0)))))

(defmacro dungeon-max
  "Takes an axis (x, y, or z), and a dungeon, and returns the maximum
extent of the dungeon along that axis"
  ([axis dungeon]
     (let [size-key   (keyword (str axis "d"))
           origin-key (keyword (str axis "0"))]
       `(apply max (map #(+ (~origin-key %)
                            (~size-key   %))
                        (rest ~dungeon))))))

(defmacro dungeon-min
  "Takes an axis (x, y, or z), and a dungeon, and returns the minimum
extent of the dungeon along that axis"
  ([axis dungeon]
     (let [origin-key (keyword (str axis "0"))]
       `(apply min (map #(~origin-key %)
                        (rest ~dungeon))))))

(defn dungeon-max-x ([dungeon] (dungeon-max x dungeon)))
(defn dungeon-max-y ([dungeon] (dungeon-max y dungeon)))
(defn dungeon-max-z ([dungeon] (dungeon-max z dungeon)))
(defn dungeon-min-x ([dungeon] (dungeon-min x dungeon)))
(defn dungeon-min-y ([dungeon] (dungeon-min y dungeon)))
(defn dungeon-min-z ([dungeon] (dungeon-min z dungeon)))

(defn dungeon-max-extent
  ([dungeon]
     (max (- (dungeon-max-x dungeon)
             (dungeon-min-x dungeon))
          (- (dungeon-max-y dungeon)
             (dungeon-min-y dungeon))
          (- (dungeon-max-z dungeon)
             (dungeon-min-z dungeon)))))

(defn translate-dungeon
  "Takes a dungeon and x, y, and z deltas, and returns the dungeon
moved to that position"
  ([dungeon xd yd zd]
     (cons (first dungeon)
           (map #(assoc %
                   :x0 (+ (:x0 %) xd)
                   :y0 (+ (:y0 %) yd)
                   :z0 (+ (:z0 %) zd))
                (rest dungeon)))))

(defn multi-dungeon-lookup
  "Takes a structure containing any number of dungeons, and x, y, and
z coordinates, and returns the zone element at that point, which will
be nil for any point that is not inside any dungeon"
  ([dungeons x y z]
     (some #(maybe-box-lookup % x y z)
           (mapcat rest dungeons))))

(defn place-dungeons
  "Takes a zone and a seq of dungeons, and returns the zone with the
dungeons placed in it"
  ;; Might be better to have this return a fn for gen-mcmap[-zone]
  ([zone dungeons]
     (gen-mcmap-zone (zone-x-size zone)
                     (zone-y-size zone)
                     (zone-z-size zone)
        (fn [x y z]
          (or (multi-dungeon-lookup dungeons x y z)
              (zone-lookup zone x y z))))))

(defn round-to-chunk-size
  ([n]
     (* 16 (quot (+ n 15)
                 16))))

(defn chest-full-of
  "Takes either an item ID or [id damage], and an optional
count (default 64), and returns a chest block full of that item"
  ([id]
     (chest-full-of id 64))
  ([id count]
     (let [damage (if (sequential? id)
                    (second id)
                    0)
           id (if (sequential? id)
                (first id)
                id)]
       (mc-block :chest
                 :items (inventory-list
                         (map (fn [slot]
                                {:id id
                                 :count count
                                 :slot slot
                                 :damage damage})
                              (range 27)))))))

(defn pick-hallway
  ([orientation seed salt]
     )

  )

(defn rotate-box-clockwise
  ([box]
     ;; XXX
     ))

(defn flip-box
  ([box]
     ;; XXX
     ))

(defn rotate-box-counterclockwise
  ([box]
     ;; XXX
     ))

(defn rotate-dummy-dungeon
  "Takes a dungeon and an orientation (a number of
clockwise-from-overhead 90-degree turns) and returns a rotated
dummy dungeon (with no zones and a dummy delivery fn)"
  ([dungeon orientation]
     (cons (fn [params])
           (map (case orientation
                      0 identity
                      1 rotate-box-clockwise
                      2 flip-box
                      3 rotate-box-counterclockwise)
                (rest dungeon)))))

(defn space-filling-seq
  ([size grid]
     (if (< grid 2)
       nil
       (let [half-grid (/ grid 2)
             incg (fn [x] (+ x half-grid))]
         (concat
          (apply concat
                 (for [x (range 0 size grid)
                       y (range 0 size grid)
                       z (range 0 size grid)]
                   [ [(incg x) y z]
                     [x (incg y) z]
                     [x y (incg z)]
                     [(incg x) (incg y) z]
                     [(incg x) y (incg z)]
                     [x (incg y) (incg z)]
                     [(incg x) (incg y) (incg z)]]))
          (space-filling-seq size half-grid))))))

(defn point-in-dungeon
  ([dungeon [x y z]]
     ;; XXX
     ))

(defn dungeon-filling-seq
  ([dungeon orientation xd yd zd]
     (let [max-dimension (dungeon-max-extent dungeon)
           grid-size (first (filter #(> % max-dimension)
                                    (iterate #(* 2 %) 1)))
           rotated-dungeon (rotate-dummy-dungeon dungeon orientation)]
       (map ; XXX - translate by xd yd zd
            (filter #(point-in-dungeon dungeon %)
                    (concat [0 0 0]
                            (space-filling-seq grid-size grid-size)))))))

(defn try-find-place-for-dungeon
  "Makes on attempt at finding a place for a dungeon, and returns nil
if the attempt fails, and [x y z orientation hallway] if it succeeds"
  ([dungeon zone accept-fn pick-place-fn seed salt]
     (let [ [xt yt zt orientation] (pick-place-fn seed salt)
            [hallway xh yh zh] (pick-hallway orientation seed salt)
            blocks-to-check (dungeon-filling-seq dungeon orientation
                                                 (+ xt xh)
                                                 (+ yt yh)
                                                 (+ zt zh))]
       (when (every? accept-fn blocks-to-check)
         [xt yt zt orientation hallway]))))

(defn find-place-for-dungeon
  "Takes a dungeon, a zone in which to place it, a function that takes
a zone element and returns true if the zone element may be overwritten
by a dungeon block, a function of a seed salt that returns a location
to try placing the entrance to a dungeon, and a seed; and returns a
location, orientation, and hallway configuration that succeeds, or nil
if placement failed too many times"
  ([dungeon zone accept-fn pick-place-fn seed]
     (loop [retries +dungeon-placement-retries+]
       (or (try-find-place-for-dungeon dungeon zone accept-fn
                                       pick-place-fn seed retries)
           (if (pos? retries)
             (recur (dec retries))
             nil)))))

(defn dungeon-exercise-1
  "Makes an area with just empty air and a dungeon"
  ([x-chunks z-chunks dungeon]
     (let [x-size (* x-chunks +chunk-side+)
           z-size (* z-chunks +chunk-side+)
           zone (gen-mcmap-zone x-size z-size (fn [x y z] :air))
           zone (place-dungeons zone [dungeon])
           mcmap (gen-mcmap x-size z-size
                            (fn [x y z]
                              (zone-lookup zone x y z)))]
       (mcmap-to-mcr-binary mcmap 0 0))))

(defn dungeon-exercise-2
  "Makes an area with just empty air and a dungeon, automatically
choosing the appropriate number of chunks for the given dungeon"
  ([dungeon]
     (println (map #(% dungeon)
                   [dungeon-min-x dungeon-max-x
                    dungeon-min-y dungeon-max-y
                    dungeon-min-z dungeon-max-z]))
     (when (or (some #(neg? (% dungeon))
                     [dungeon-min-x dungeon-min-y dungeon-min-z])
               (> (dungeon-max-y dungeon)
                  +chunk-height+)
               (some #(> (% dungeon)
                         +region-side+)
                     [dungeon-max-x dungeon-max-z]))
       (throw (RuntimeException. (str "dungeon not sized or located"
                                      " appropriately for region 0,0"))))
     (let [x-size (round-to-chunk-size (inc (dungeon-max-x dungeon)))
           z-size (round-to-chunk-size (inc (dungeon-max-z dungeon)))
           _ (println "zone size: x=" x-size " z=" z-size)
           zone (gen-mcmap-zone x-size z-size (fn [x y z] :air))
           zone (place-dungeons zone [dungeon])
           mcmap (gen-mcmap x-size z-size
                            (fn [x y z]
                              (zone-lookup zone x y z)))]
       (mcmap-to-mcr-binary mcmap 0 0))))

(defn survival-map-supplies-1
  "Makes a single chunk with chests full of goodies, to be transferred
out of region 0,0 and used for playtesting dungeons"
  ([]
     (let [x-size 16
           z-size 16
           chest-items (vec (map mc-item [:wood :cobble :iron-block
                                          :diamond-block :leather
                                          :fire :coal :white-wool
                                          :string :arrow :steak]))
           n-chest-items (count chest-items)
           gen-fn (fn [x y z]
                    (cond (< y 64)
                            (mc-block :stone)
                          (= [x z] [8 8])
                            (mc-block :south-ladder)
                          (= [x z] [8 9])
                            (mc-block :cobble)
                          (= [x z] [7 8])
                            (chest-full-of
                             (chest-items (mod (* y 2)
                                               n-chest-items)))
                          (= [x z] [9 8])
                            (chest-full-of
                             (chest-items (mod (inc (* y 2))
                                               n-chest-items)))
                          :else
                            (mc-block :air)))]
       (mcmap-to-mcr-binary (gen-mcmap x-size z-size gen-fn)
                            0 0))))
