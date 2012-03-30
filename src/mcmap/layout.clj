(ns mcmap.layout
  (:use mcmap.core
        mcmap.util
        mcmap.dungeon.build
        mcmap.srand))

(def +chest-slots+ 27)

(defn fnbox-fn
  "Given x, y, and z sizes, and a fn of x, y, z, and params, returns a
  dungeon centered at 0,0,0 with the given size and contents
  determined by then fn"
  ([x-size y-size z-size f]
     (let [zone (promise)]
       [(fn [params]
          (deliver zone
                   (gen-mcmap-zone x-size y-size z-size
                                   (fn [x y z]
                                     (f x y z params)))))
        {:x0 (int (/ x-size -2))
         :y0 0
         :z0 (int (/ z-size -2))
         :xd x-size, :yd y-size, :zd z-size
         :zone zone}])))

;;; This is a somewhat lame macro.  It only saves one pair of parens
;;; and "fn".
(defmacro fnbox
  "Given x, y, and z sizes, a binding vector for x, y, z, and params,
  and code, returns a dungeon centered at 0,0,0 with the given size
  and with contents determined by evaluating the code at each
  coordinate"
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
  them up along the axis, not altering their positions on the other
  two axes, aligning the entire thing such that all coordinates are >0
  for alignment :high, <0 for alignment :low, and centered around =0
  for alignment :center"
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

(defn align
  "Like lineup, but takes only one dungeon as the first argument, for
  convenience with the -> macro"
  ([dungeon axis alignment]
     (lineup axis alignment dungeon)))

(defn stack
  "Takes any number of dungeons and stacks them (the dungeons, not
  their constituent boxes) bottom-to-top, centering the result
  vertically around y=0, and makes them a single dungeon"
  ([& dungeons]
     (apply lineup :y :high dungeons)))

(defn htable
  "Takes any number of vectors of dungeons and arranges them in a
  table, each row being arranged from south to north, and the rows
  being ordered from west to east (such that the arrangement of code
  matches the map view in minutor) -- note that columns are not
  aligned across rows; only rows are aligned

      W
    [...]
  S [...] N
    [...]
      E"
  ([& dungeon-vectors]
     (apply lineup :x :center
            (map #(apply lineup :z :center (reverse %))
                 dungeon-vectors))))

(defn surround-fn
  "Takes a dungeon and a function of seven arguments (three
  coordinates, params, and three dimensions of the surrounding box)
  returning a zone element and returns a single-box dungeon containing
  the given dungeon, bounded by walls one block thick made of zone
  elements determined by the function; coordinates of the given
  dungeon are preserved"
  ([dungeon f]
     (surround-fn dungeon f :air))
  ([dungeon f fill]
     (let [min-x (dungeon-min-x dungeon)
           min-y (dungeon-min-y dungeon)
           min-z (dungeon-min-z dungeon)
           x-size (+ 2 (dungeon-x-extent dungeon))
           y-size (+ 2 (dungeon-y-extent dungeon))
           z-size (+ 2 (dungeon-z-extent dungeon))
           p (promise)]
       [ (fn [params]
           (let [render (first dungeon)]
             (render params)
             (deliver p
                      (gen-mcmap-zone
                       x-size y-size z-size
                       (fn [x y z]
                         (if (or (#{0 (dec x-size)} x)
                                 (#{0 (dec y-size)} y)
                                 (#{0 (dec z-size)} z))
                           (f x y z params x-size y-size z-size)
                           (or (maybe-dungeon-lookup dungeon
                                                     (+ x min-x -1)
                                                     (+ y min-y -1)
                                                     (+ z min-z -1))
                               fill)))))))
         {:x0 (dec min-x), :y0 (dec min-y), :z0 (dec min-z)
          :xd x-size,      :yd y-size,      :zd z-size
          :zone p}])))

(defn surround
  "Takes a dungeon and a zone element and returns a single-box dungeon
  containing the given dungeon, bounded by walls one block thick made
  of the given zone element; coordinates are preserved"
  ([dungeon ze]
     (surround-fn dungeon (fn [_ _ _ _ _ _ _] ze)))
  ([dungeon ze fill]
     (surround-fn dungeon
                  (fn [_ _ _ _ _ _ _] ze)
                  fill)))

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
                             orig-zone @(:zone orig-box)]
                         (deliver p
                           (gen-mcmap-zone x-size y-size z-size
                             (fn [x y z]
                               (or (maybe-dungeon-lookup overlay-dungeon
                                                         (+ x x0)
                                                         (+ y y0)
                                                         (+ z z0))
                                   (zone-lookup orig-zone x y z)))))))
                     ps
                     (rest dungeon))))
             boxes))))

(defn dungeon-neighbors-of
  "Returns a seq of up to six blocks adjacent to the given coordinates
  in the given zone"
  ([dungeon x y z]
     (lazy-seq
      (filter identity
              [(maybe-dungeon-lookup dungeon (inc x) y z)
               (maybe-dungeon-lookup dungeon (dec x) y z)
               (maybe-dungeon-lookup dungeon x (inc y) z)
               (maybe-dungeon-lookup dungeon x (dec y) z)
               (maybe-dungeon-lookup dungeon x y (inc z))
               (maybe-dungeon-lookup dungeon x y (dec z))]))))

(defn dungeon-map-neighbors
  "Takes a dungeon and a fn of a block and a collection of its
  neighbors and returns a dungeon made up of blocks returned by the
  fn"
  ([dungeon f]
     (let [ps (repeatedly (dec (count dungeon))
                          promise)
           boxes (map assoc
                      (rest dungeon)
                      (repeat :zone)
                      ps)]
       (cons (fn [params]
               ( (first dungeon) params)
               (dorun
                (map (fn [p orig-box]
                       (let [x-size (:xd orig-box)
                             y-size (:yd orig-box)
                             z-size (:zd orig-box)
                             x0 (:x0 orig-box)
                             y0 (:y0 orig-box)
                             z0 (:z0 orig-box)]
                         (deliver p
                           (gen-mcmap-zone x-size y-size z-size
                             (fn [x y z]
                               (f (maybe-dungeon-lookup dungeon
                                                        (+ x x0)
                                                        (+ y y0)
                                                        (+ z z0))
                                  (dungeon-neighbors-of dungeon
                                                        (+ x x0)
                                                        (+ y y0)
                                                        (+ z z0))))))))
                     ps
                     (rest dungeon))))
             boxes))))

(defn rand-prize
  "Takes a dungeon and returns a dungeon that will contain only random
  contents in prize-chests, ignoring (:prize params)"
  ([dungeon]
     (cons (fn [params]
             ( (first dungeon)
               (dissoc params :prize)))
           (rest dungeon))))

(defn modify-params-fn
  "Takes a dungeon and a fn f, and returns a dungeon generated with
  params as the result of (f params)"
  ([dungeon f]
     (cons (fn [params]
             ( (first dungeon)
               (f params)))
           (rest dungeon))))

(defmacro modify-params
  "Wraps a single dungeon generation call, modifying the params that
  will be passed to that dungeon generator"
  ([dungeon [binding & body]]
     (apply #'modify-params dungeon binding body))
  ([dungeon binding & body]
     `(modify-params-fn ~dungeon
                        (fn ~binding ~@body))))

(defn reward
  "Takes a dungeon, an operator, and subsequent arguments, and returns
  a dungeon generated with its params' :reward value replaced
  with (apply operator (:reward params) subsequent-arguments)"
  ([dungeon op & args]
     (modify-params dungeon [params]
       (assoc params :reward
              (apply op (:reward params)
                     args)))))

(defn prize-items
  "Returns a seq of n items, suitable for set-prize"
  ([n item]
     (prize-items +chest-slots+ n item))
  ([slots n item]
     (when (and (not= 0 n)
                (pos? slots))
       (lazy-seq
        (let [avg-stack-left (int (+ 0.5 (/ n (max 1 slots))))
              one-fifth-items (inc (int (/ n 5)))
              stack (if (neg? n)
                      64
                      (min 64 (max one-fifth-items
                                   avg-stack-left)))]
          (cons (if (map? item)
                  (assoc item :count stack)
                  {:type item :count stack})
                (prize-items (dec slots)
                             (- n stack)
                             item))))))
  ([slots n item & more]
     (let [ns    (cons n    (take-nth 2 more))
           items (cons item (take-nth 2 (rest more)))
           ns-sum (reduce + ns)
           subslots (scale-ints slots ns)]
       (mapcat prize-items subslots ns items))))

(defn set-prize
  "Takes a dungeon and a seq of items in the same form that would be
  returned by prize-chest-items, and returns a dungeon generated with
  its :prize set to that seq"
  ([dungeon prize]
     (modify-params dungeon [params]
       (assoc params :prize prize))))

(defn clobber-params
  "Takes a dungeon and a map, and returns a dungeon generated with
  params equal to that map"
  ([dungeon params]
     (modify-params dungeon [_]
       params)))

(defn pain
  "Takes a dungeon, an operator, and subsequent arguments, and returns
  a dungeon generated with its params' :pain value replaced
  with (apply operator (:pain params) subsequent-arguments)"
  ([dungeon op & args]
     (modify-params dungeon [params]
       (assoc params :pain
              (apply op (:pain params)
                     args)))))

(defn strict-dungeon
  "Takes a dungeon, renders it, forbidding the use of params, and
  returns a dungeon that may be rendered repeatedly"
  ([dungeon]
     (render-dungeon dungeon #())
     (cons (fn [_])
           (rest dungeon))))
