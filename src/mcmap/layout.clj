(ns mcmap.layout
  (:use mcmap.core
        mcmap.dungeon))

(defn fbox-fn
  "Given x, y, and z sizes, and a fn of x, y, z, and params, returns a
dungeon centered at 0,0,0 with then given size and contents determined
by then fn"
  ([x-size y-size z-size f]
     (let [zone (promise)
           generator (if (and (> x-size 1)
                              ;; XXX 400 is a wild guess
                              (> (* y-size z-size) 400))
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
(defmacro fbox
  "Given x, y, and z sizes, a binding vector for x, y, z, and params,
and code, returns a dungeon centered at 0,0,0 with the given size and
with contents determined by evaluating the code at each coordinate"
  ([x-size y-size z-size binding-vector & body]
     `(fbox-fn ~x-size ~y-size ~z-size
               (fn ~binding-vector ~@body))))

(defn box
  "Returns a dungeon centered at 0,0,0 and filled with the given zone
element; default size is 1x1x1"
  ([ze]
     (box 1 1 1 ze))
  ([x-size y-size z-size ze]
     (fbox x-size y-size z-size [_ _ _ _] ze)))

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
            {:x [#(translate-dungeon %1 %2 0 0)
                 dungeon-x-extent dngeon-min-x]
             :y [#(translate-dungeon %1 0 %2 0)
                 dungeon-y-extent dngeon-min-y]
             :z [#(translate-dungeon %1 0 0 %2)
                 dungeon-z-extent dngeon-min-z]}
            sizes (map dungeon-extent dungeons)
            total-size (reduce + sizes)
            targets (reductions + (case alignment
                                        :high   0
                                        :center (int (/ total-size -2))
                                        :low    (- total-size))
                                sizes)
            currents (map dungeon-min-axis dungeons)
            deltas (map - targets currents)]
       (merge-dungeons
        (map translator dungeons deltas)))))

(defn stack
  "Takes any number of dungeons and stacks them (the dungeons, not
their constituent boxes) bottom-to-top, centering the result
vertically around y=0, and makes them a single dungeon"
  ([& dungeons]
     (lineup :y :center dungeons)))

(defn htable
  "Takes any number of vectors of dungeons and arranges them in a
table, each row being arranged from south to north, and the rows being
ordered from west to east (such that the arrangement of code matches
the map view in minutor)"
  ([& dungeon-vectors]
     (apply lineup :x :center
            (map #(apply lineup :z :high (reverse %))
                 dungeon-vectors))))

(defn surround
  "Takes a dungeon and a zone element and returns a single-box dungeon
containing the given dungeon, bounded by walls one block thick made of
the given zone element; coordinates are preserved"
  ([dungeon ze]
     (let [min-x (dungeon-min-x dungeon)
           min-y (dungeon-min-y dungeon)
           min-z (dungeon-min-z dungeon)
           x-size (dungeon-x-extent dungeon)
           y-size (dungeon-y-extent dungeon)
           z-size (dungeon-z-extent dungeon)
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
                           (or (maybe-multibox-lookup (rest dungeon)
                                                      (- x min-x 1)
                                                      (- y min-y 1)
                                                      (- z min-z 1))
                               :air)))))))
         :x0 min-x,  :y0 min-y,  :z0 min-z
         :xd x-size, :yd y-size, :zd z-size
         :zone p])))

;;; XXX I probably need some way of controlling which mobs are allowed
;;; to appear; e.g., for a map where blaze rods are an objective.  The
;;; vector of allowed mobs could go in params.

(defn spawners
  ([x-size y-size z-size seed]
     (fbox x-size y-size z-size [x y z params]
        (let [pain (:pain params)
              h (int (+ 0.5 (snorm [(* 2 pain) 1] seed x z)))
              spawner? (< y h)]
          (if (not spawner?)
            :air
            (let [mob-num (int (snorm [(dec (* 8 pain)) 2 0 8]
                                      seed x y z))
                  mob ( ["Enderman" "Zombie" "PigZombie"
                         "Spider" "Skeleton" "Ghast" "Creeper"
                         "Blaze" "CaveSpider"]
                          mob-num)]
              (mc-block :mob-spawner
                        :mob mob
                        :delay (int (* 200 (- 1 pain))))))))))

(defn add-entrance
  "Takes a dungeon, a vector (y and z specify the position for the
entrance, and x specifies the depth of the hole that needs to be
punched in the dungeon), entrance sign text, and a seed, and returns a
dungeon with an entrance added and with its location standardized"
  ([dungeon [ex ey ez] text seed]
     ;; XXX
     ))

