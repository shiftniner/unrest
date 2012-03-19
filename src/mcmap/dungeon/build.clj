(ns mcmap.dungeon.build
  (:use mcmap.core
        mcmap.util
        mcmap.blocks
        mcmap.srand))

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

(defn round-to-chunk-size
  ([n]
     (* 16 (quot (+ n 15)
                 16))))

(defn container-full-of
  "Takes a block type, inventory list count, numeric item ID (or [id
  damage]), and stack size, and returns a block with that inventory
  list"
  ([block-type n-slots id count]
     (let [damage (if (sequential? id)
                    (second id)
                    0)
           id (if (sequential? id)
                (first id)
                id)]
       (mc-block block-type
                 :items (inventory-list
                         (map (fn [slot]
                                {:id id
                                 :count count
                                 :slot slot
                                 :damage damage})
                              (range n-slots)))))))

(defn chest-full-of
  "Takes either a numeric item ID or [id damage], and an optional
  count (default 64), and returns a chest block full of that item"
  ([id]
     (chest-full-of id 64))
  ([id count]
     (container-full-of :chest 27 id count)))

(defn dispenser-full-of
  "Takes either a numeric item ID or [id damage], and an optional
  count (default 64), and returns a dispenser block full of that item"
  ([id]
     (dispenser-full-of id 64))
  ([id count]
     (container-full-of :dispenser 9 id count)))

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

(let [materials [:snow-block :stone-bricks :nether-brick :bedrock]]
  (defn hall-material
    "Takes a pain level, seed, and salts, and returns a block type"
    ([pain seed & salts]
       (materials (int (apply snorm [(* pain (count materials))
                                     0.5 0 (count materials)]
                              seed salts))))))
