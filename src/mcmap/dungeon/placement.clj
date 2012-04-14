(ns mcmap.dungeon.placement
  (:use mcmap.core
        mcmap.util
        mcmap.blocks
        mcmap.dungeon.build
        mcmap.toolkit
        mcmap.srand
        mcmap.octree
        mcmap.dungeons
        mcmap.cavern
        mcmap.layout
        mcmap.balance))


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

(def dungeons-dump (atom nil))

(def hallways-dump (atom nil))

(defn place-dungeons
  "Takes a zone, a seq of dungeons, and a seq of hallways, and returns
  the zone with the dungeons placed in it"
  ;; Might be better to have this return a fn for gen-mcmap[-zone]
  ([zone dungeons hallways]
     (when (some #(not (dungeon-rendered? %))
                 dungeons)
       (compare-and-set! dungeons-dump @dungeons-dump dungeons)
       (compare-and-set! hallways-dump @hallways-dump hallways)
       (die "dungeon or hallway not rendered; dungeons dumped to"
            " @mcmap.placement/dungeons-dump, and hallways dumped to"
            " @mcmap.placement/hallways-dump"))
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

(defn- generic-hall-fn
  ([w y v params seed salt max-y floor-block]
     (cond (> y max-y)
             nil
           (or (#{0 6} w)
               (#{0 max-y} y))
             :ground
           (or (#{1 5} w)
               (= y (dec max-y)))
             (hall-material (:pain params)
                            seed salt w y v)
           (= y 1)
             floor-block
           :else
             :air)))

(defn- hall-fn
  ([w y v params seed salt]
     (generic-hall-fn w y v params seed salt 6 :cavern-wall)))

(defn- stair-fn
  ([w y v params seed salt stair-face max-y flat-floor]
     (generic-hall-fn w y v params seed salt max-y
                      (if flat-floor
                        :cavern-wall
                        (mem-mc-block :stone-brick-stairs
                                      :face stair-face)))))

(defn dungeon-intersects-octree?
  "Takes a dungeon and an octree and returns true if and only if at
  least one of the boxes of the dungeon overlaps at least one box in
  the octree"
  ([octree dungeon]
     (some (fn [ {x0 :x0, y0 :y0, z0 :z0,
                  xd :xd, yd :yd, zd :zd}]
             (oct-any-intersecting? octree x0 y0 z0
                                    (+ x0 xd)
                                    (+ y0 yd)
                                    (+ z0 zd)))
           (rest dungeon))))

(defn oct-assoc-dungeon
  "Takes an octree and one or more dungeons and returns the octree
  with the boxes from the dungeon(s) added to it"
  ([octree dungeon]
     (reduce (fn [tree box]
               (oct-assoc-box tree box))
             octree
             (rest dungeon)))
  ([octree d1 d2 & more]
     (reduce oct-assoc-dungeon octree (list* d1 d2 more))))

(defn pick-hallway
  "Given a _dungeon_ orientation (not necessarily the orientation at
  which the player will enter the hallway), seed, and salt, returns a
  randomly-chosen hallway (in dungeon form), and x, y, and z deltas
  from traveling through the hallway, as [hallway xd yd zd]"
  ([orientation seed salt]
     (let [len (int (snorm [10 10 5] seed salt))
           ;; These blocks need to be determined at render time based
           ;; on params
           [x-dim z-dim] (case orientation
                               (0 2) [len 7]
                               (1 3) [7 len])
           y-dim 7
           [x0 z0 xd zd]
             (case orientation
                   0 [0 0 len 0]
                   1 [-7 0 0 len]
                   2 [(- len) -7 (- len) 0]
                   3 [0 (- len) 0 (- len)])
           yd 0
           p (promise)]
       [ [(fn [params]
            (let [zone-fn (case orientation
                                (0 2) (fn [x y z]
                                        (hall-fn z y x params seed salt))
                                (1 3) (fn [x y z]
                                        (hall-fn x y z params seed salt)))]
              (deliver p
                       (gen-mcmap-zone x-dim y-dim z-dim zone-fn))))
          {:x0 x0, :y0 0, :z0 z0,
           :xd x-dim, :yd y-dim, :zd z-dim,
           :axis (case orientation
                       (0 2) :x
                       (1 3) :z)
           :range (case orientation
                        (0 1) (range len)
                        (2 3) (range (dec len) -1 -1))
           :zone p}]
         xd yd zd orientation])))

(defn pick-joint
  "Given an exit orientation, seed, and salt, returns either a
  leftward bending joint or a rightward bending joint hallway, the
  entry orientation, and the x, y, and z deltas from traveling through
  the joint, as [hallway xd yd zd orientation]"
  ([orientation seed salt]
     (let [dir (sranditem [:left :right] seed salt)
           entry-orientation (mod ( (if (= :left dir) inc dec)
                                    orientation)
                                  4)
           openings (set [orientation (mod (+ 2 entry-orientation)
                                           4)])
           [has-east-wall? has-south-wall? has-west-wall? has-north-wall?]
             (map #(not (openings %)) (range 4))
           [x-dim y-dim z-dim len] (repeat 7)
           [x0 z0 xd zd]
             (case [entry-orientation orientation]
                   [1 0] [-7 0 0 0]
                   [0 1] [0 0 7 7]
                   [2 1] [-7 -7 0 0]
                   [1 2] [-7 0 -7 7]
                   [3 2] [0 -7 0 0]
                   [2 3] [-7 -7 -7 -7]
                   [0 3] [0 0 0 0]
                   [3 0] [0 -7 7 -7])
           p (promise)]
       [ [(fn [params]
            (let [zone-fn
                    (fn [x y z]
                      (let [n-dist z
                            s-dist (- z-dim z 1)
                            e-dist (- x-dim x 1)
                            w-dist x
                            min-dist (min n-dist s-dist e-dist w-dist)
                            [side wall?]
                              (condp = min-dist
                                  n-dist [:north has-north-wall?]
                                  s-dist [:south has-south-wall?]
                                  e-dist [:east has-east-wall?]
                                  w-dist [:west has-west-wall?])
                            hall-orientation
                              (case [side wall?]
                                    ([:north true] [:south true]
                                     [:east false] [:west false]) 0
                                    1)]
                        (case hall-orientation
                              0 (hall-fn z y x params seed salt)
                              1 (hall-fn x y z params seed salt))))]
              (deliver p (gen-mcmap-zone x-dim y-dim z-dim zone-fn))))
          {:x0 x0, :y0 0, :z0 z0,
           :xd x-dim, :yd y-dim, :zd z-dim,
           :axis (case orientation
                       (0 2) :x
                       (1 3) :z)
           :range (case orientation
                        (0 1) (range len)
                        (2 3) (range (dec len) -1 -1))
           :zone p}]
         xd 0 zd entry-orientation])))

(defn pick-stair
  "Given an exit orientation, seed, and salt, returns either an upward
  or downward staircase hallway, the entry orientation, and the x, y,
  and z deltas from traveling through the hallway, as [hallway xd yd
  zd orientation]"
  ([orientation seed salt]
     (let [dir (sranditem [:up :down] seed salt)
           len (int (snorm [10 10 3] seed salt))
           [x-dim z-dim] (case orientation
                               (0 2) [1 7]
                               (1 3) [7 1])
           y-dim 8
           [x0 z0 xstep zstep xd zd]
             (case orientation
                   0 [0 0 1 0 (inc len) 0]
                   1 [-7 0 0 1 0 (inc len)]
                   2 [-1 -7 -1 0 (- (inc len)) 0]
                   3 [0 -1 0 -1 0 (- (inc len))])
           ystep (case dir :up 1 :down -1)
           yd (* ystep len)
           ps (vec (repeatedly (inc len)
                               promise))]
       [ (vec
          (list*
           (fn [params]
             (let [stair-face ( [:east :south :west :north]
                                (mod (+ orientation
                                        (if (= dir :up) 0 2))
                                     4))
                   zone-fn-fn (case orientation
                                    (0 2) (fn [dist max-y flat-floor]
                                            (fn [x y z]
                                              (stair-fn z y x params
                                                        (reseed seed salt
                                                                dist)
                                                        0 stair-face max-y
                                                        flat-floor)))
                                    (1 3) (fn [dist max-y flat-floor]
                                            (fn [x y z]
                                              (stair-fn x y z params
                                                        (reseed seed salt
                                                                dist)
                                                        0 stair-face max-y
                                                        flat-floor))))]
               (doseq [dist (range (inc len))]
                 (let [max-y (if (or (and (= dir :down) (= dist 0))
                                     (and (= dir :up)   (= dist len)))
                               6 7)
                       flat-floor (or (and (= dir :up)   (= dist 0))
                                      (and (= dir :down) (= dist len)))]
                     (deliver (ps dist)
                              (gen-mcmap-zone x-dim y-dim z-dim
                                              (zone-fn-fn dist max-y
                                                          flat-floor)))))))
           (for [dist (range (inc len))]
             {:x0 (+ x0 (* xstep dist))
              :y0 (* ystep dist)
              :z0 (+ z0 (* zstep dist))
              :xd x-dim, :yd y-dim, :zd z-dim,
              :axis (case orientation (0 2) :x (1 3) :z)
              :range [0]
              :stair true
              :zone (ps dist)})))
         xd yd zd orientation])))

(defn has-stairs?
  "Takes a dungeon and returns whether it is a hallway containing
  stairs"
  ([hallway]
     (some :stair (rest hallway))))

(defn join-hallways
  "Takes two hallvects and returns the hallvect that is the
  combination of those, in the order: entrance, first hallway, second
  hallway, dungeon, as [hallway xd yd zd exit-orientaton]"
  ([h1 h2]
     (vec (concat
           [ (merge-dungeons (first h1)
                             (apply translate-dungeon
                                    (first h2)
                                    (take 3 (rest h1))))]
           (map +
                (take 3 (rest h1))
                (take 3 (rest h2)))
           [ (nth h2 4)]))))

(defn pick-complex-hallway
  "Given an exit orientation, seed, and salt, returns a hallway
  composed of one or more segments"
  ([orientation seed salt]
     (let [n-segments (+ 4 (int (srand 7 seed salt 1)))
           segment-fns (uniq (concat [pick-hallway]
                                     (map #(sranditem [pick-hallway
                                                       pick-joint
                                                       pick-stair]
                                                      seed salt 2 %)
                                          (range n-segments))
                                     [pick-hallway]))
           octree-size 128
           octree-offset (/ octree-size -2)
           first-segment ( (first segment-fns)
                           orientation (reseed seed salt 3) 1)
           segment-tree (octree octree-size 4 octree-offset
                                octree-offset octree-offset)
           segment-tree (oct-assoc-dungeon segment-tree
                                           (first first-segment))]
       (loop [segment-fns (rest segment-fns)
              segment-tree segment-tree
              hallway first-segment
              orientation (nth first-segment 4)
              i 4]
         (if-not (seq segment-fns)
           hallway
           (let [new-segment ( (first segment-fns)
                               orientation (reseed seed salt i) 1)
                 placed-new-segment (apply translate-dungeon
                                           (first new-segment)
                                           (map -
                                                (repeat 0)
                                                (take 3 (rest new-segment))
                                                (take 3 (rest hallway))))]
             (if (dungeon-intersects-octree? segment-tree
                                             placed-new-segment)
               hallway
               (recur (rest segment-fns)
                      (oct-assoc-dungeon segment-tree placed-new-segment)
                      (join-hallways new-segment hallway)
                      (nth new-segment 4)
                      (inc i)))))))))

(defn try-find-place-for-dungeon
  "Makes on attempt at finding a place for a dungeon, and returns nil
  if the attempt fails, and [x y z orientation hallway] if it
  succeeds"
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
  a zone element and returns true if the zone element may be
  overwritten by a dungeon block, a function of a seed salt that
  returns a location to try placing the entrance to a dungeon, and a
  seed; and returns a location, orientation, and hallway configuration
  that succeeds, or nil if placement failed too many times"
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
    "Takes a zone and returns a pick-place-fn for
  find-place-for-dungeon that just finds a random pocket of air, which
  is assumed to be reachable, or nil on failure"
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
                                    0 [2 -1]
                                    1 [6  2]
                                    2 [3  6]
                                    3 [-1 3])]
                       [(+ x0 x-offset) (dec y0) (+ z0 z-offset) orientation])
                   (pos? salt2)
                     (recur (dec salt2))
                   :else
                     (throw (RuntimeException. "air-finder failed")))))))))

(defn new-air-finder
  ([zone seed & salts]
     ( (air-finder zone)
       (apply reseed seed salts) 0)))

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

(defn sequence-hallway-slices
  "Takes a hallway, and coordinates for the entrance to the hallway,
  and returns a seq of slices of the hallway, each of which is a seq
  of [x y z] coordinates.  The slices start from the entrance to the
  hallway and end at the entrance to the dungeon."
  ([hall dx dy dz]
     (forcat [hsegment (rest hall)]
       (let [x0 (:x0 hsegment)
             y0 (:y0 hsegment)
             z0 (:z0 hsegment)
             hallway-slice
               (case (:axis hsegment)
                     :x (fn [x]
                          (for [z (range (:zd hsegment))
                                y (range (:yd hsegment))]
                            [(+ x dx x0)
                             (+ y dy y0)
                             (+ z dz z0)]))
                     :z (fn [z]
                          (for [x (range (:xd hsegment))
                                y (range (:yd hsegment))]
                            [(+ x dx x0)
                             (+ y dy y0)
                             (+ z dz z0)])))]
         (map hallway-slice (:range hsegment))))))

(defn fully-increasing-count?
  "Takes a seq of seqs and a function of one argument, and returns
  true only if the final seq returns true for all of its elements, and
  each seq has a fraction of items that returns true that is no less
  than the one before it"
  ([ss f]
     (letfn [(check-count
              ([n-s1f s1]
                 (= n-s1f (count s1)))
              ([n-s1total n-s1f s1 s2 & ss]
                 (let [n-s2total (count s2)
                       n-s2f (count (filter f s2))]
                   (and (<= (/ n-s1f n-s1total)
                            (/ n-s2f n-s2total))
                        (if (seq ss)
                          (recur n-s2total n-s2f s2 (first ss) (rest ss))
                          (check-count n-s2f s2))))))]
       (let [n-s1total (count (first ss))
             n-s1f (count (filter f (first ss)))]
         (and (> (- n-s1total n-s1f)
                 8)
              (apply check-count n-s1total n-s1f ss))))))

(defn cave-hallway-accepter
  "Takes a zone and a seq of hallway coordinate slices as returned by
  sequence-hallway-slices, and returns true if the hallway starts in
  air, passes into ground, and remains fully within the ground for the
  remainder of its length"
  ([zone coord-seqs]
     (fully-increasing-count?
         coord-seqs
         (fn [ [x y z]]
           (= :ground (maybe-zone-lookup zone x y z))))))

(defn pick-dungeon-place
  "Takes too many arguments: a zone, a seed, a function that takes a
  zone, seed, and salts and returns a place from which a hallway may
  originate, a function that takes a block and returns whether or not
  a dungeon can occupy that block, a function of an orientation, seed,
  and salt that returns a hallway, a function of a zone and a seq of
  seqs of blocks that returns true if the path defined by that seq is
  an acceptable hallway, a vector of possible dungeon names, and extra
  args to pass to the dungeon generator; and returns a vector of a
  placed dungeon and a placed hallway, or nil if placement failed too
  many times"
  ([zone seed entrance-finder dungeon-accepter hall-chooser
    hall-accepter d-names dg-args]
     (loop [salt 0]
       (when (< salt +dungeon-placement-retries+)
         (let [dungeon-name (sranditem d-names seed salt 1)]
           (if-let* [ [ex ey ez eo] (entrance-finder zone seed salt 2)
                      [hall hx hy hz] (hall-chooser eo seed salt)
                      dungeon (apply get-dungeon dungeon-name (+ ey hy)
                                     (reseed seed salt 3)
                                     dg-args)
                      _ (every? #(dungeon-accepter
                                      (apply maybe-zone-lookup
                                             zone %))
                                (dungeon-filling-seq dungeon eo
                                                     (+ ex hx)
                                                     (+ ey hy)
                                                     (+ ez hz)))
                      _ (hall-accepter zone
                                       (sequence-hallway-slices
                                        hall ex ey ez))]
              (let [placed-dungeon (translate-dungeon
                                      (rotate-dungeon dungeon eo)
                                      (+ ex hx)
                                      (+ ey hy)
                                      (+ ez hz))
                    placed-hallway (translate-dungeon hall ex ey ez)]
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
          (let [ [dungeon hallway] (first dunhalls)]
            (if (dungeon-intersects-octree? returned dungeon)
              (recur (rest dunhalls))
              (let [new-returned (oct-assoc-dungeon returned dungeon)]
                (if (dungeon-intersects-octree? new-returned hallway)
                  (recur (rest dunhalls))
                  (cons (first dunhalls)
                        (lazy-seq
                         (let [returned (oct-assoc-dungeon new-returned
                                                           hallway)]
                           (non-intersecting-dunhalls
                            (rest dunhalls)
                            nil
                            returned)))))))))))))

(defn agl-map
  "Given a zone and a function that returns true if a block is in the
  ground, and false if it is air, returns a zone mapping each
  coordinate in zone to its distance above ground or floor level"
  ([zone is-ground?]
     (rising-recursive-gen-mcmap-zone (zone-x-size zone)
                                      (zone-y-size zone)
                                      (zone-z-size zone)
       (fn [x y z prev-val]
         (when-not (is-ground? (zone-lookup zone x y z))
           (inc (or prev-val -1)))))))

(def +null-dungeon+ [ (fn [params])])

(defn add-chests
  "Takes several arguments and returns a seq of
  n (possibly-overlapping) dunhalls; the dungeons contain chests
  filled with the given item or items and topped with a snow block and
  a sign, and the hallways are null hallways (which do not overlap
  with anything)"
  ([cavern-zone n contents sign place-chooser seed]
     (let [agl-map (agl-map cavern-zone #{:ground})
           text (sign-wrap-text sign)
           _ (when (not= 1 (count text))
               (die "in add-chests, text too long for one sign: \""
                    sign "\""))
           base-dungeon (->
                         (lineup :x :high
                                 (stack (box nil)
                                        (box (mc-block :wall-sign
                                               :face :east
                                               :text (vec (first text)))))
                                 (stack (prize-chest :east 0)
                                        (box :snow-block)))
                         (clobber-params {:prize contents})
                         (surround nil)
                         (strict-dungeon))
           chests (vec (map rotate-dungeon
                            (repeat base-dungeon)
                            (range 4)))]
       ( (fn chest-seq [n]
           (when (pos? n)
             (lazy-seq
              (loop [salt2 0]
                (if-let* [orientation (int (srand 4 seed n salt2 1))
                          [x y z] (place-chooser seed n salt2 2)
                          [xd zd face xo zo] (case orientation
                                                   0 [-1 0 :west -1  0]
                                                   1 [0 -1 :south 1 -1]
                                                   2 [1  0 :east  2  1]
                                                   3 [0  1 :north 0  2])
                          h1 (maybe-zone-lookup agl-map x y z)
                          _ (pos? h1)
                          [x2 z2] (map + [x z] [xd zd])
                          h2 (maybe-zone-lookup agl-map x2 y z2)
                          _ (<= h1 h2 (+ h1 2))
                          y (- y h1)
                          _ (pos? y)
                          dunhall [ (translate-dungeon
                                     (chests orientation)
                                     (+ x xo) y (+ z zo))
                                    +null-dungeon+]]
                  (cons dunhall
                        (chest-seq (dec n)))
                  (recur (inc salt2)))))))
         n))))

(defn rand-place-fn
  "Returns a function of seed and salts that returns a randomly-chosen
  point [x y z] such that 0 <= x < x-max, 0 <= y < y-max,
  0 <= z < z-max, and x, y, and z are integers"
  ([x-max y-max z-max]
     (fn [seed & salts]
       (vec (map #(int (apply srand %1 seed %2 salts))
                 [x-max y-max z-max]
                 (range))))))

(defn hallway-y
  "Takes a hallway dungeon and returns the y coordinate of the
  entrance to that hallway"
  ([hallway]
     (:y0 (second hallway))))

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

(defn dungeon-playtest
  "Makes an area with just empty air and a dungeon, automatically
  choosing the appropriate number of chunks for the given dungeon"
  ([dungeon params hallway-fn]
     (println (map #(% dungeon)
                   [dungeon-min-x dungeon-max-x
                    dungeon-min-y dungeon-max-y
                    dungeon-min-z dungeon-max-z]))
     (let [hall-seed (long (rand +seed-max+))
           _ (msg 1 "hall seed: " hall-seed)
           [hallway hx hy hz]
             (hallway-fn 0 hall-seed 1)
           dunhall-merged (merge-dungeons hallway
                                          (translate-dungeon
                                           dungeon hx hy hz))
           hallway (translate-dungeon hallway
                                      (- (dungeon-min-x dunhall-merged))
                                      63
                                      (- (dungeon-min-z dunhall-merged)))
           dungeon (translate-dungeon dungeon
                                      (- hx (dungeon-min-x dunhall-merged))
                                      (+ hy 63)
                                      (- hz (dungeon-min-z dunhall-merged)))
           dunhall-merged (merge-dungeons hallway dungeon)]
       (render-dungeon hallway params)
       (render-dungeon dungeon params)
       (when (or (some #(neg? (% dungeon))
                       [dungeon-min-x dungeon-min-y dungeon-min-z])
                 (> (dungeon-max-y dungeon)
                    +max-map-height+)
                 (some #(> (% dungeon)
                           +region-side+)
                       [dungeon-max-x dungeon-max-z]))
         (throw (RuntimeException. (str "dungeon not sized or located"
                                        " appropriately for region 0,0"))))
       (let [x-size (round-to-chunk-size (inc (dungeon-max-x dunhall-merged)))
             y-size (round-to-chunk-size (inc (dungeon-max-y dunhall-merged)))
             z-size (round-to-chunk-size (inc (dungeon-max-z dunhall-merged)))
             _ (println "zone size: x=" x-size " z=" z-size)
             zone (gen-mcmap-zone x-size y-size z-size
                                  (fn [x y z]
                                    (if (< y 65) :stone :air)))
             zone (place-dungeons zone [dungeon hallway] [])
             mcmap (gen-mcmap x-size y-size z-size
                              (fn [x y z]
                                (let [ze (zone-lookup zone x y z)]
                                  (if ( #{:ground :cavern-wall} ze)
                                    :sandstone
                                    ze))))]
         (count-spawners (:block-zone mcmap))
         (mcmap-to-mca-binary mcmap 0 0)))))

(defn dungeon-playtest-1
  "Dungeon playtest with a simple linear hallway"
  ([dungeon params]
     (dungeon-playtest dungeon params pick-hallway)))

(defn dungeon-playtest-2
  "Dungeon playtest with a complex hallway"
  ([dungeon params]
     (dungeon-playtest dungeon params pick-complex-hallway)))

(defn dungeon-playtest-3
  "Dungeon playtest with a hardcoded complex hallway a la
  dungeon-exercise-5, for confirming the documentation of the order of
  args of join-hallways is accurate"
  ([dungeon params]
     (dungeon-playtest dungeon params
       (fn [orientation seed salt]
         (let [h1 (pick-hallway orientation seed 1)
               _ (msg 0 "hall 1: " (rest h1))
               hj1 (pick-joint orientation seed 2)
               orientation2 (hj1 4)
               h2 (pick-stair orientation2 seed 3)
               _ (msg 0 "hall 2: " (rest h2))]
           (reduce join-hallways [h2 hj1 h1]))))))

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

(defn dungeon-exercise-3
  "Creates an epic cave network and puts a dungeon (or several
  dungeons) in it someplace reachable"
  ([seed]
     (dungeon-exercise-3 seed 22.5 80))
  ([seed level start-level]
     (dungeon-exercise-3 seed seed level start-level))
  ([cavern-seed seed level start-level]
     (let [n-caves 15
           n-dungeons 64
           chunks 16
           map-difficulty (/ level 100)
           start-difficulty (/ start-level 100)
;;            cavern-wall   (mc-block :stone)
;;            uncommon-wall (mc-block :stone-bricks)
;;            rare-wall     (mc-block :diamond-ore)
           cavern-wall   (mc-block :sandstone)
           uncommon-wall (mc-block :smooth-sandstone)
           rare-wall     (mc-block :creeper-sandstone)

           max-x (* chunks +chunk-side+)
           max-y 128
           max-z (* chunks +chunk-side+)
           max-dim (max max-x max-y max-z)
           [epic-zone start-x start-z]
               (epic-cave-network n-caves max-x max-y max-z cavern-seed)
           _ (println "Start is x=" start-x " z=" start-z)
           _ (msg 3 "Finding dungeons ...")
           excess-dunhalls (pmap pick-dungeon-place
                                 (repeat epic-zone)
                                 (map #(reseed seed 1 %)
                                      (vtake 1000 100
                                             "Tried %d dungeons ..."
                                             (range 1000)))
                                 (repeat new-air-finder)
                                 (repeat #{:ground})
                                 (transition 1000
                                             pick-hallway
                                             pick-complex-hallway)
                                 (repeat cave-hallway-accepter)
                                 (repeat (apply get-dungeons
                                                (concat
                                                 (repeat 6 :std)
                                                 (repeat 3 :uncommon)
                                                 (repeat 1 :rare))))
                                 (repeat nil))
           excess-dunhalls (filter identity excess-dunhalls)
           dunhalls (vtake n-dungeons 5 "Got %d dungeons ..."
                           (non-intersecting-dunhalls excess-dunhalls
                                                      max-dim))
           actual-dungeons dunhalls
           torch-chests (add-chests epic-zone 1000
                                    (prize-items -1 :torch)
                                    ["" "Torches"]
                                    (rand-place-fn max-x (- max-y 5)
                                                   max-z)
                                    (reseed seed 2))
           rail-chests (add-chests epic-zone 1000
                                   (prize-items +chest-slots+
                                                -1/100 :minecart
                                                -4 :powered-rail
                                                -4 :redstone-torch-on
                                                -1/100 :button
                                                -1/100 :detector-rail
                                                -1/100 :redstone-dust
                                                -20 :rail)
                                   (str "Catch a riiiiiiiiiiiiiiiiiiiii"
                                        "iiiiide?")
                                   (rand-place-fn max-x (- max-y 15)
                                                  max-z)
                                   (reseed seed 2))
           dunhalls (vtake (+ 128 n-dungeons)
                           64 "Dungeons/torch chests: %d"
                           (non-intersecting-dunhalls
                            (concat dunhalls
                                    (interleave-n 3 torch-chests
                                                  1 rail-chests))
                            max-dim))
           dungeons (map first dunhalls)
           hallways (map second dunhalls)
           _ (msg 3 "Rendering dungeons ...")
           _ (dorun (pmap render-dungeon
                          (apply concat dunhalls)
                          (dup-seq (map (fn [hallway dungeon]
                                          (let [y (or (hallway-y hallway)
                                                      (:y0 (second dungeon))
                                                      (/ max-y 2))
                                                y-frac (- 1 (/ y max-y))]
                                            {:pain (dungeon-pain
                                                    y-frac
                                                    map-difficulty
                                                    start-difficulty)
                                             :reward (* (Math/pow 256.0
                                                                  y-frac)
                                                        200)}))
                                        hallways dungeons))))
           _ (msg 3 (str "Got " (count actual-dungeons) " dungeons"))
           _ (msg 3 "Placing dungeons and hallways ...")
           epic-zone (place-dungeons epic-zone dungeons hallways)
           _ (msg 3 "Adding crisp bedrock crust ...")
           bedrock-generator (fn [x y z]
                               (let [ze (zone-lookup epic-zone x y z)
                                     neighbors (neighbors-of epic-zone
                                                             x y z)]
                                 (if (every? #{:ground :bedrock}
                                             (cons ze neighbors))
                                   :bedrock
                                   ze)))
           epic-zone (gen-mcmap-zone max-x max-y max-z bedrock-generator)
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
                                         (if (and (> y (- max-y 10))
                                                  (every? #(= :bedrock %)
                                                          neighbors))
                                           (mc-block :lava-source)
                                           :bedrock)
                                       :air (mc-block :air)
                                       (:ground :cavern-wall)
                                         (let [r  (srand 1 seed 51 x y z)
                                               r2 (srand 1 seed 52 x y z)
                                               p (dungeon-pain
                                                  (- 1 (/ y max-y))
                                                  map-difficulty
                                                  start-difficulty)]
                                           (cond
                                            (and (> r 0.96)
                                                 (< r2 (Math/pow 1e-18 p)))
                                              :glowstone
                                            (and (> r 0.96)
                                                 (< (- 1 r2)
                                                    (* p (Math/pow 1e-5
                                                                   (- 1 p)))))
                                              (mc-block
                                               :mob-spawner
                                               :mob (pick-mob
                                                     +standard-mobs+
                                                     1 seed 1 54 x y z)
                                               :delay (int (snorm
                                                            [(* 200 (- 1 p))
                                                             50 0]
                                                            seed 53 x y z)))
                                            (< r 0.002) rare-wall
                                            (< r 0.012) uncommon-wall
                                            :else       cavern-wall))
                                       ze))))]
       (generic-map-maker chunks chunks generator))))

(defn dungeon-exercise-4
  "Hallways as dungeons, for testing alignment; returns a dungeon"
  ([orientation seed]
     (let [h1 (pick-hallway orientation seed 1)
           _ (msg 0 "hall 1: " (rest h1))
           hj1 (pick-joint orientation seed 2)
           orientation2 (hj1 4)
           h2 (pick-hallway orientation2 seed 3)
           _ (msg 0 "hall 2: " (rest h2))
           hj2 (pick-joint orientation2 seed 4)
           orientation3 (hj2 4)
           h3 (pick-hallway orientation3 seed 5)
           _ (msg 0 "hall 3: " (rest h3))
           [h hx hy hz o] (reduce join-hallways [h3 hj2 h2 hj1 h1])]
       (translate-dungeon h (- (dungeon-min-x h))
                          0 0))))

(defn dungeon-exercise-5
  "Hallways as dungeons, for testing alignment; returns a dungeon"
  ([orientation seed]
     (let [h1 (pick-hallway orientation seed 1)
           _ (msg 0 "hall 1: " (rest h1))
           hj1 (pick-joint orientation seed 2)
           orientation2 (hj1 4)
           h2 (pick-stair orientation2 seed 3)
           _ (msg 0 "hall 2: " (rest h2))
           hj2 (pick-joint orientation2 seed 4)
           orientation3 (hj2 4)
           h3 (pick-hallway orientation3 seed 5)
           _ (msg 0 "hall 3: " (rest h3))
           [h hx hy hz o] (reduce join-hallways [h3 hj2 h2 hj1 h1])]
       (translate-dungeon h (- (dungeon-min-x h))
                          0 0))))
