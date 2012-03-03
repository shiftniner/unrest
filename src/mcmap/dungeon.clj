(ns mcmap.dungeon
  (:use mcmap.core
        mcmap.blocks
        mcmap.srand
        mcmap.cavern
        mcmap.octree
        mcmap.util
        mcmap.dungeons))

(def +dungeon-placement-retries+ 1000)

(def +air-finder-retries+ 50000)

(def +hello-dungeon+
     [(fn [params])
      {:x0 6,  :y0 0,  :z0 -7,
       :xd 21, :yd 21, :zd 21,
       :zone
         (atom
           (gen-mcmap-zone 21 21 21
             (fn [x y z]
               (cond (and (#{0 1} x)
                          (< 8 z 12)
                          (< 1 y 5))
                       :air
                     (some #{0 20} [x y z])
                       :bedrock
                     (some #{1 19} [x y z])
                       :moss-stone
                     (#{[10 2 10] [8 2 8] [12 2 12]
                        [8 2 12] [12 2 8] [10 3 10]} [x y z])
                       (mc-block :mob-spawner
                                 :mob "Zombie" :delay 0)
                     :else
                       :air))))}
      {:x0 0, :y0 0, :z0 0,
       :xd 6, :yd 7, :zd 7,
       :zone
         (atom
           (gen-mcmap-zone 6 7 7
             (fn [x y z]
               (cond (some #{0 6} [y z])
                       :bedrock
                     (some #{1 5} [y z])
                       :moss-stone
                     (= [x y z] [3 3 2])
                       (mc-block :wall-sign
                                 :text ["" "Hello," "Dungeon"]
                                 :face :north)
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

(defn dungeon-x-extent
  ([dungeon]
     (- (dungeon-max-x dungeon)
        (dungeon-min-x dungeon))))

(defn dungeon-y-extent
  ([dungeon]
     (- (dungeon-max-y dungeon)
        (dungeon-min-y dungeon))))

(defn dungeon-z-extent
  ([dungeon]
     (- (dungeon-max-z dungeon)
        (dungeon-min-z dungeon))))

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
translated by that vector"
  ([dungeon xd yd zd]
     (cons (first dungeon)
           (map #(assoc %
                   :x0 (+ (:x0 %) xd)
                   :y0 (+ (:y0 %) yd)
                   :z0 (+ (:z0 %) zd))
                (rest dungeon)))))

(defn maybe-multibox-lookup
  "Takes a seq of boxes, and x, y, and z coordinates, and returns the
zone element at that point, which will be nil for any point that is
not inside any box"
  ([boxes x y z]
     (some #(maybe-box-lookup % x y z)
           boxes)))

(defn maybe-dungeon-lookup
  "Takes a dungeon, and x, y, and z coordinates, and returns the zone
element at that point, which will be nil for any point that is not
inside the dungeon"
  ([dungeon x y z]
     (maybe-multibox-lookup (rest dungeon) x y z)))

(defn render-dungeon
  "Takes an unrendered dungeon, renders it, and returns it"
  ([dungeon params]
     ( (first dungeon) params)
     dungeon))

(defn place-dungeons
  "Takes a zone, a seq of dungeons, and a seq of hallways, and returns
the zone with the dungeons placed in it"
  ;; Might be better to have this return a fn for gen-mcmap[-zone]
  ([zone dungeons hallways]
     (let [zone-size (max (zone-x-size zone)
                          (zone-z-size zone)
                          (zone-y-size zone))
           octree-size (first (filter #(>= % zone-size)
                                      (iterate (partial * 2)
                                               8)))
           dungeon-tree (octree octree-size 8)
           hallway-tree (octree octree-size 8)
           dungeon-tree (reduce (fn [oct box]
                                  (oct-assoc-box oct box))
                                dungeon-tree
                                (mapcat rest dungeons))
           hallway-tree (reduce (fn [oct box]
                                  (oct-assoc-box oct box))
                                hallway-tree
                                (mapcat rest hallways))]
       (gen-mcmap-zone (zone-x-size zone)
                       (zone-y-size zone)
                       (zone-z-size zone)
         (fn [x y z]
           (let [ze (zone-lookup zone x y z)]
             (if-let [hze (and (not= ze :air)
                               (maybe-multibox-lookup
                                (oct-lookup hallway-tree x y z) x y z))]
               hze
               (if-let [dze (maybe-multibox-lookup
                             (oct-lookup dungeon-tree x y z) x y z)]
                 dze
                 ze))))))))

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

(defn rotate-empty-box-clockwise
  ([box]
     (let [{x0 :x0, y0 :y0, z0 :z0,
            xd :xd, yd :yd, zd :zd} box]
       {:x0 (- 0 z0 zd) :y0 y0, :z0 x0,
        :xd zd, :yd yd, :zd xd})))

(defn flip-empty-box
  ([box]
     (let [{x0 :x0, y0 :y0, z0 :z0,
            xd :xd, yd :yd, zd :zd} box]
       {:x0 (- 0 x0 xd) :y0 y0, :z0 (- 0 z0 zd),
        :xd xd, :yd yd, :zd zd})))

(defn rotate-empty-box-counterclockwise
  ([box]
     (let [{x0 :x0, y0 :y0, z0 :z0,
            xd :xd, yd :yd, zd :zd} box]
       {:x0 z0 :y0 y0, :z0 (- 0 x0 xd),
        :xd zd, :yd yd, :zd xd})))

(def +rotate-face-90+
     {:north           :east
      :north-northeast :east-southeast
      :northeast       :southeast
      :east-northeast  :south-southeast
      :east            :south
      :east-southeast  :south-southwest
      :southeast       :southwest
      :south-southeast :west-southwest
      :south           :west
      :south-southwest :west-northwest
      :southwest       :northwest
      :west-southwest  :north-northwest
      :west            :north
      :west-northwest  :north-northeast
      :northwest       :northeast
      :north-northwest :east-northeast})

(defn rotate-block
  ([rotation ze]
     (if (or (zero? rotation)
             (not (map? ze))
             (not (:face ze)))
       ze
       (rotate-block (dec rotation)
                     (assoc ze :face (+rotate-face-90+ (:face ze)))))))

(defn rotate-empty-box
  "Takes a box and an orientation (a number of clockwise-from-overhead
90-degree turns around the origin), and returns a rotated box with
no :zone"
  ([orientation box]
     ( (case (mod orientation 4)
             0 identity
             1 rotate-empty-box-clockwise
             2 flip-empty-box
             3 rotate-empty-box-counterclockwise)
       box)))

(defn rotate-zone-clockwise
  ([zone]
     (let [in-x (zone-x-size zone)
           in-y (zone-y-size zone)
           in-z (zone-z-size zone)]
       (gen-mcmap-zone in-z in-y in-x
         (fn [x y z]
           (rotate-block 1 (zone-lookup zone z y (dec (- in-z x)))))))))

(defn flip-zone
  ([zone]
     (let [in-x (zone-x-size zone)
           in-y (zone-y-size zone)
           in-z (zone-z-size zone)]
       (gen-mcmap-zone in-x in-y in-z
         (fn [x y z]
           (rotate-block 2
                         (zone-lookup zone
                                      (dec (- in-x x))
                                      y
                                      (dec (- in-z z)))))))))

(defn rotate-zone-counterclockwise
  ([zone]
     (let [in-x (zone-x-size zone)
           in-y (zone-y-size zone)
           in-z (zone-z-size zone)]
       (gen-mcmap-zone in-z in-y in-x
         (fn [x y z]
           (rotate-block 3 (zone-lookup zone (dec (- in-x z)) y x)))))))

(defn rotate-zone
  "Takes a zone and an orientation (a number of clockwise-from-overhead
90-degree turns around the origin), and returns a rotated zone"
  ([orientation zone]
     ( (case (mod orientation 4)
             0 identity
             1 rotate-zone-clockwise
             2 flip-zone
             3 rotate-zone-counterclockwise)
       zone)))


(defn rotate-dungeon
  "Takes a dungeon and an orientation (a number of
clockwise-from-overhead 90-degree turns around the origin) and returns
a rotated dungeon"
  ([dungeon orientation]
     (let [promises (repeatedly (dec (count dungeon))
                                promise)]
       (cons (fn [params]
               ( (first dungeon) params)
               (dorun (map deliver
                           promises
                           (map #(rotate-zone orientation @(:zone %))
                                (rest dungeon)))))
             (map #(assoc (rotate-empty-box orientation %1)
                     :zone %2)
                  (rest dungeon)
                  promises)))))

(defn pick-hallway
  "Given a _dungeon_ orientation (not necessarily the orientation at
which the player will enter the hallway), seed, and salt, returns a
randomly-chosen hallway (in dungeon form), and x, y, and z deltas from
traveling through the hallway, as [hallway xd yd zd]"
  ([orientation seed salt]
     (let [len (int (snorm [10 10 5] seed salt))
           ;; These blocks need to be determined at render time based
           ;; on params
           hall-fn (fn [w y]
                     (cond (some #{0 6} [w y])
                             :ground
                           (= y 1)
                             :sandstone
                           (some #{1 5} [w y])
                             (mc-block :moss-brick)
                           :else
                             :air))
           zone-fn (case orientation
                         (0 2) (fn [x y z] (hall-fn z y))
                         (1 3) (fn [x y z] (hall-fn x y)))
           [x-dim z-dim] (case orientation
                               (0 2) [len 7]
                               (1 3) [7 len])
           y-dim 7
           zone (gen-mcmap-zone x-dim y-dim z-dim zone-fn)
           [x0 z0 xd zd]
             (case orientation
                   0 [0 0 len 0]
                   1 [-7 0 0 len]
                   2 [(- len) -7 (- len) 0]
                   3 [0 (- len) 0 (- len)])
           yd 0]
       [ [(fn [params])
          {:x0 x0, :y0 0, :z0 z0,
           :xd x-dim, :yd y-dim, :zd z-dim,
           :axis (case orientation
                       (0 2) :x
                       (1 3) :z)
           :range (case orientation
                        (0 1) (range len)
                        (2 3) (range (dec len) -1 -1))
           :zone (atom zone)}]
         xd yd zd])))

(defn space-filling-seq
  "Returns a seq of all points, with x y and z coordinates >= 0 and <
size, in an order such that a large extent of the volume is covered
relatively quickly"
  ([size]
     (cons [0 0 0]
           (space-filling-seq size size)))
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

(defn point-in-box
  ([box x y z]
     (let [x0 (:x0 box)
           y0 (:y0 box)
           z0 (:z0 box)]
       (and (>= x x0) (>= y y0) (>= z z0)
            (< x (+ x0 (:xd box)))
            (< y (+ y0 (:yd box)))
            (< z (+ z0 (:zd box)))))))

(defn point-in-dungeon
  ([dungeon [x y z]]
     (some #(point-in-box % x y z)
           (rest dungeon))))

(defn dungeon-filling-seq
  "Takes a dungeon, an orientation, and offsets by which to move the
dungeon, and returns a seq of points that would be within the
dungeon's boxes if it were placed at that orientation and offset"
  ([dungeon orientation xd yd zd]
     (let [max-dimension (dungeon-max-extent dungeon)
           grid-size (first (filter #(> % max-dimension)
                                    (iterate #(* 2 %) 1)))
           rotated-dungeon (rotate-dungeon dungeon orientation)
           min-x (dungeon-min-x rotated-dungeon)
           min-y (dungeon-min-y rotated-dungeon)
           min-z (dungeon-min-z rotated-dungeon)]
       (map (fn [ [x y z] ]
              [(+ x xd) (+ y yd) (+ z zd)])
            (filter #(point-in-dungeon rotated-dungeon %)
                    (map (fn [ [x y z] ]
                           [(+ x min-x)
                            (+ y min-y)
                            (+ z min-z)])
                         (space-filling-seq grid-size)))))))

(defn try-find-place-for-dungeon
  "Makes on attempt at finding a place for a dungeon, and returns nil
if the attempt fails, and [x y z orientation hallway] if it succeeds"
  ([dungeon zone accept-fn pick-place-fn seed salt]
     (let [ [xt yt zt orientation] (pick-place-fn seed salt)
            hallvect (pick-hallway orientation seed salt)
            [hallway xh yh zh] hallvect
            ;; Only really need to check the surfaces of the boxes
            blocks-to-check (dungeon-filling-seq dungeon orientation
                                                 (+ xt xh)
                                                 (+ yt yh)
                                                 (+ zt zh))]
       (when (every? #(accept-fn (apply maybe-zone-lookup zone %))
                     blocks-to-check)
         [xt yt zt orientation hallvect]))))

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

(let [air-check-seq (filter #(every? (partial > 5) %)
                            (space-filling-seq 8))]
  (defn air-finder
    "Takes a zone and returns a pick-place-fn for find-place-for-dungeon
that just finds a random pocket of air, which is assumed to be
reachable, or nil on failure"
    ([zone]
       (fn [seed salt]
         (loop [salt2 +air-finder-retries+]
           (let [x0 (+ 3 (int (srand (- (zone-x-size zone) 10)
                                     seed salt salt2 1)))
                 y0 (+ 8 (int (srand (- (zone-y-size zone) 16)
                                     seed salt salt2 2)))
                 z0 (+ 3 (int (srand (- (zone-z-size zone) 10)
                                     seed salt salt2 3)))
                 orientation (int (srand 4 seed salt salt2 4))]
             (cond (every? (fn [ [x y z]]
                             ( #{:air}
                               (zone-lookup zone (+ x x0) (+ y y0) (+ z z0))))
                           air-check-seq)
                     (let [ [x-offset z-offset]
                              (case orientation
                                    0 [0 0]
                                    1 [7 0]
                                    2 [0 7]
                                    3 [0 0])]
                       [(+ x0 x-offset) y0 (+ z0 z-offset) orientation])
                   (pos? salt2)
                     (recur (dec salt2))
                   :else
                     (throw (RuntimeException. "air-finder failed")))))))))

(defn place-dungeon-in-caves
  "Takes a zone and a seed and returns [placed-dungeon
placed-hallway]; throws an exception if placement failed"
  ([zone seed]
     (let [dungeon +hello-dungeon+      ; XXX
           [dun-x dun-y dun-z orientation [hallway hx hy hz]]
               (find-place-for-dungeon dungeon zone #{:ground}
                                       (air-finder zone)
                                       seed)
           _ (when-not dun-x
               (throw (RuntimeException. "find-place-for-dungeon failed")))
           placed-dungeon (translate-dungeon (rotate-dungeon dungeon
                                                             orientation)
                                             (+ dun-x hx)
                                             (+ dun-y hy)
                                             (+ dun-z hz))
           placed-hallway (translate-dungeon hallway dun-x dun-y dun-z)]
       [placed-dungeon placed-hallway])))

(defn pick-dungeon-place
  "Takes too many arguments: a zone, a seed, a function that takes a
  zone, seed, and salts and returns a place from which a hallway may
  originate, a function that takes a block and returns whether or not
  a dungeon can occupy that block, a function of a seed and salts that
  returns a hallway, a function of a zone and a seq of seqs of blocks
  that returns true if the path defined by that seq is an acceptable
  hallway, a vector of possible dungeon names, and extra args to pass
  to the dungeon generator; and returns a vector of a placed dungeon
  and a placed hallway, or nil if placement failed too many times"
  ([zone seed entrance-finder dungeon-accepter hall-chooser
    hall-accepter d-names dg-args]
     (loop [salt 0]
       (when (< salt +dungeon-placement-retries+)
         (let [dungeon-name (sranditem d-names seed salt 1)]
           (if-let* [ [ex ey ez eo] (entrance-finder zone seed salt 2)
                      [hall hx hy hz] (hall-chooser eo seed salt)
                      _ (hall-accepter zone
                                       (sequence-hallway-slices
                                          hall hx hy hz))
                      dungeon (apply get-dungeon dungeon-name (+ ey hy)
                                     (reseed seed salt 3)
                                     dg-args)
                      _ (every? #(dungeon-accepter
                                      (apply maybe-zone-lookup
                                             zone %))
                                (dungeon-filling-seq dungeon eo
                                                     (+ ex hx)
                                                     (+ ey hy)
                                                     (+ ez hz)))]
              (let [placed-dungeon (translate-dungeon
                                    (rotate-dungeon dungeon eo)
                                    (+ ex hx)
                                    (+ ey hy)
                                    (+ ez hz))
                    placed-hallway (translate-dungeon hallway
                                                      ex ey ez)]
                [placed-dungeon placed-hallway])
              (recur (inc salt))))))))

(defn non-intersecting-dunhalls
  "Takes a seq of dungeon/hallway pairs -- ideally a lazy and fairly
long but NOT infinite seq -- and returns a lazy seq of dunhalls that
do not intersect with each other by filtering out those that overlap
with earlier-returned dunhalls"
  ([dunhalls size]
     (non-intersecting-dunhalls dunhalls nil (octree size 8)))
  ([dunhalls _ returned]
     (lazy-seq
      (loop [dunhalls dunhalls]
        (when (seq dunhalls)
          (if (some (fn [ {x0 :x0, y0 :y0, z0 :z0,
                           xd :xd, yd :yd, zd :zd}]
                      (oct-any-intersecting? returned x0 y0 z0
                                             (+ x0 xd)
                                             (+ y0 yd)
                                             (+ z0 zd)))
                    (mapcat rest (first dunhalls)))
            (recur (rest dunhalls))
            (cons (first dunhalls)
                  (lazy-seq
                   (let [returned (reduce (fn [returned box]
                                            (oct-assoc-box returned box))
                                          returned
                                          (mapcat rest (first dunhalls)))]
                     (non-intersecting-dunhalls
                      (rest dunhalls)
                      nil
                      returned))))))))))

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

(defn dungeon-playtest-1
  "Makes an area with just empty air and a dungeon, automatically
choosing the appropriate number of chunks for the given dungeon"
  ([dungeon]
     (println (map #(% dungeon)
                   [dungeon-min-x dungeon-max-x
                    dungeon-min-y dungeon-max-y
                    dungeon-min-z dungeon-max-z]))
     (let [[hallway hx hy hz]
             (pick-hallway 0 (long (rand +seed-max+)) 1)
           hallway (translate-dungeon hallway
                                      (- (dungeon-min-x dungeon))
                                      63
                                      (- (dungeon-min-z dungeon)))
           dungeon (translate-dungeon dungeon
                                      (- hx (dungeon-min-x dungeon))
                                      (+ hy 63)
                                      (- hz (dungeon-min-z dungeon)))]
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
             zone (gen-mcmap-zone x-size z-size
                                  (fn [x y z]
                                    (if (< y 65) :stone :air)))
             zone (place-dungeons zone [dungeon hallway] [])
             mcmap (gen-mcmap x-size z-size
                              (fn [x y z]
                                (let [ze (zone-lookup zone x y z)]
                                  (if (= :ground ze)
                                    :sandstone
                                    ze))))]
         (mcmap-to-mcr-binary mcmap 0 0)))))

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
                            {:type :ladder :face :south}
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

(defn dungeon-exercise-2
  "Creates an epic cave network and puts a dungeon (or several
dungeons) in it someplace reachable"
  ([seed]
     (let [chunks 16
           max-x (* chunks +chunk-side+)
           max-z (* chunks +chunk-side+)
           [epic-zone start-x start-z]
                 (epic-cave-network 15 max-x max-z seed)
           _ (msg 3 "Finding dungeons ...")
           excess-dunhalls (pmap place-dungeon-in-caves
                                 (repeat epic-zone)
                                 (map #(long (srand +seed-max+ seed
                                                    5516 %))
                                      (range 1000)))
           dunhalls (take 64 (non-intersecting-dunhalls excess-dunhalls
                                                        max-x))
           dungeons (map first dunhalls)
           hallways (map second dunhalls)
           _ (doseq [d dungeons]
               ( (first d) nil))        ; replace with render-dungeon
           _ (msg 3 (str "Got " (count dungeons) " dungeons"))
           _ (msg 3 "Placing dungeons and hallways ...")
           epic-zone (place-dungeons epic-zone dungeons hallways)
           _ (msg 3 "Adding bedrock ...")
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
                         (cond (or (zero? x) (zero? z)
                                   (= x-bound x) (= z-bound z))
                                 (mc-block :bedrock)
                               :else
                                 (case ze
                                       :bedrock
                                       (if (every? #(= :bedrock %)
                                                   neighbors)
                                         (mc-block :lava-source)
                                         :bedrock)
                                       :air (mc-block :air)
                                       :ground (mc-block :sandstone)
                                       ze))))]
       (println "Start is x=" start-x " z=" start-z)
       (generic-map-maker chunks chunks generator))))

