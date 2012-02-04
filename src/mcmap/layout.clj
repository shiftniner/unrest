(ns mcmap.layout
  (:use mcmap.core
        mcmap.dungeon
        mcmap.srand))

;;; Number of blocks per z slice at which it is worthwhile to use
;;; p-gen-mcmap-zone instead of gen-mcmap-zone.  XXX 400 is a wild
;;; guess; experiment.
(def +size-at-which-pmap-faster+ 400)

(defn fnbox-fn
  "Given x, y, and z sizes, and a fn of x, y, z, and params, returns a
dungeon centered at 0,0,0 with the given size and contents determined
by then fn"
  ([x-size y-size z-size f]
     (let [zone (promise)
           generator (if (and (> x-size 1)
                              (> (* y-size z-size)
                                 +size-at-which-pmap-faster+))
                       p-gen-mcmap-zone
                       gen-mcmap-zone)]
       [(fn [params]
          (deliver zone
                   (generator x-size y-size z-size
                              (fn [x y z]
                                (f x y z params)))))
        {:x0 (int (/ x-size -2))
         :y0 (int (/ y-size -2))
         :z0 (int (/ z-size -2))
         :xd x-size, :yd y-size, :zd z-size
         :zone zone}])))

;;; This is a somewhat lame macro.  It only saves one pair of parens
;;; and "fn".
(defmacro fnbox
  "Given x, y, and z sizes, a binding vector for x, y, z, and params,
and code, returns a dungeon centered at 0,0,0 with the given size and
with contents determined by evaluating the code at each coordinate"
  ([x-size y-size z-size binding-vector & body]
     `(fnbox-fn ~x-size ~y-size ~z-size
               (fn ~binding-vector ~@body))))

(defn box
  "Returns a dungeon centered at 0,0,0 and filled with the given zone
element; default size is 1x1x1"
  ([ze]
     (box 1 1 1 ze))
  ([x-size y-size z-size ze]
     (fnbox x-size y-size z-size [_ _ _ _] ze)))

(defn pad
  "Returns a dungeon of the given size filled with air"
  ([x y z]
     (box x y z :air)))

(defn merge-dungeons
  "Takes any number of dungeons and returns a single dungeon with the
same boxes"
  ([& dungeons]
     (let [fns (map first dungeons)
           boxes (mapcat rest dungeons)]
       (cons (fn [params]
               (doseq [f fns]
                 (f params)))
             boxes))))

(defn lineup
  "Takes an axis, an alignment, and any number of dungeons and lines
them up along the axis, not altering their positions on the other two
axes, aligning the entire thing such that all coordinates are >0 for
alignment :high, <0 for alignment :low, and centered around =0 for
alignment :center"
  ([axis alignment & dungeons]
     (let [ [translator dungeon-extent dungeon-min-axis]
            ({:x [#(translate-dungeon %1 %2 0 0)
                  dungeon-x-extent dungeon-min-x]
              :y [#(translate-dungeon %1 0 %2 0)
                  dungeon-y-extent dungeon-min-y]
              :z [#(translate-dungeon %1 0 0 %2)
                  dungeon-z-extent dungeon-min-z]} axis)
            sizes (map dungeon-extent dungeons)
            total-size (reduce + sizes)
            targets (reductions + (case alignment
                                        :high   0
                                        :center (int (/ total-size -2))
                                        :low    (- total-size))
                                sizes)
            currents (map dungeon-min-axis dungeons)
            deltas (map - targets currents)]
       (apply merge-dungeons
              (map translator dungeons deltas)))))

(defn stack
  "Takes any number of dungeons and stacks them (the dungeons, not
their constituent boxes) bottom-to-top, centering the result
vertically around y=0, and makes them a single dungeon"
  ([& dungeons]
     (apply lineup :y :high dungeons)))

(defn htable
  "Takes any number of vectors of dungeons and arranges them in a
table, each row being arranged from south to north, and the rows being
ordered from west to east (such that the arrangement of code matches
the map view in minutor)"
  ([& dungeon-vectors]
     (apply lineup :x :center
            (map #(apply lineup :z :center (reverse %))
                 dungeon-vectors))))

(defn surround
  "Takes a dungeon and a zone element and returns a single-box dungeon
containing the given dungeon, bounded by walls one block thick made of
the given zone element; coordinates are preserved"
  ([dungeon ze]
     (let [min-x (dungeon-min-x dungeon)
           min-y (dungeon-min-y dungeon)
           min-z (dungeon-min-z dungeon)
           x-size (+ 2 (dungeon-x-extent dungeon))
           y-size (+ 2 (dungeon-y-extent dungeon))
           z-size (+ 2 (dungeon-z-extent dungeon))
           p (promise)]
       [ (fn [params]
           (let [f (first dungeon)]
             (f params)
             (deliver p
                      (p-gen-mcmap-zone
                       (+ x-size 2) (+ y-size 2) (+ z-size 2)
                       (fn [x y z]
                         (if (or (#{0 (dec x-size)} x)
                                 (#{0 (dec y-size)} y)
                                 (#{0 (dec z-size)} z))
                           ze
                           (or (maybe-dungeon-lookup dungeon
                                                     (+ x min-x -1)
                                                     (+ y min-y -1)
                                                     (+ z min-z -1))
                               :air)))))))
         {:x0 (dec min-x), :y0 (dec min-y), :z0 (dec min-z)
          :xd x-size,      :yd y-size,      :zd z-size
          :zone p}])))

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

(defn dungeon-replace
  "Takes two dungeons, returning a dungeon of the same shape as the
first, but with blocks from the second wherever the two dungeons
intersect"
  ([dungeon overlay-dungeon]
     (let [ps (repeatedly (dec (count dungeon))
                          promise)
           boxes (map assoc
                      (rest dungeon)
                      (repeat :zone)
                      ps)]
       (cons (fn [params]
               ( (first dungeon) params)
               ( (first overlay-dungeon) params)
               (dorun
                (map (fn [p orig-box]
                       (let [x-size (:xd orig-box)
                             y-size (:yd orig-box)
                             z-size (:zd orig-box)
                             x0 (:x0 orig-box)
                             y0 (:y0 orig-box)
                             z0 (:z0 orig-box)
                             orig-zone @(:zone orig-box)
                             generator
                               (if (and (> x-size 1)
                                        (> (* y-size z-size)
                                           +size-at-which-pmap-faster+))
                                 p-gen-mcmap-zone
                                 gen-mcmap-zone)]
                         (deliver p
                           (generator x-size y-size z-size
                             (fn [x y z]
                               (or (maybe-dungeon-lookup overlay-dungeon
                                                         (+ x x0)
                                                         (+ y y0)
                                                         (+ z z0))
                                   (zone-lookup orig-zone x y z)))))))
                     ps
                     (rest dungeon))))
             boxes))))

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

(defn prize-chest
  "Returns a teensy dungeon consisting of a prize chest"
  ([]
     (fnbox 1 1 1 [_ _ _ params]
            (mc-block :chest
                      :items
                      (inventory-list
                       [{:id (mc-item :coal)
                         :slot 13}])))))

