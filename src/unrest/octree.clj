(ns unrest.octree)

(set! *warn-on-reflection* true)

(defrecord OctreeContents [o
                           ^long x0 ^long y0 ^long z0
                           ^long x1 ^long y1 ^long z1])

(defn octree
  "Returns a new empty octree"
  ([max-dim min-dim]
     (octree max-dim min-dim 0 0 0))
  ([max-dim min-dim x0 y0 z0]
     (let [max-dim (first (drop-while #(< % max-dim)
                                      (iterate #(* % 2) 1)))]
       {:max-dim max-dim
        :min-dim min-dim
        :x0 x0, :y0 y0, :z0 z0
        :size max-dim
        :nodes [nil nil nil nil nil nil nil nil]
        :contents nil})))

(defn- add-to-contents
  "Given an octree node and an object, returns an octree node with the
  object added directly at that node"
  ([oct o]
     (cast OctreeContents o)
     (assoc oct
       :contents (if-let [c (:contents oct)]
                   (conj c o)
                   [o]))))

(defn oct-assoc
  "Given an octree, an object, and two coordinates defining a cuboid
  region, returns an octree with the object added at that cuboid
  region"
  ([oct o x0 y0 z0 x1 y1 z1]
     (if (or (= x0 x1)
             (= y0 y1)
             (= z0 z1))
       (throw (RuntimeException. (str "attempted to add a zero-size box"
                                      " to an octree")))
       (oct-assoc oct o x0 y0 z0 x1 y1 z1
                  (:max-dim oct)
                  (:min-dim oct)
                  (:x0 oct)
                  (:y0 oct)
                  (:z0 oct))))
  ([oct o x0 y0 z0 x1 y1 z1 node-size min-dim node-x0 node-y0 node-z0]
     (let [oct (or oct
                   {:x0 node-x0, :y0 node-y0, :z0 node-z0,
                    :size node-size
                    :nodes [nil nil nil nil nil nil nil nil]
                    :contents nil})]
       (if (<= node-size min-dim)
         (add-to-contents oct (OctreeContents. o x0 y0 z0 x1 y1 z1))
         (let [half-size (quot node-size 2)
               nodes (:nodes oct)
               x-midpoint (+ node-x0 half-size)
               y-midpoint (+ node-y0 half-size)
               z-midpoint (+ node-z0 half-size)
               x-low  (< x0 x-midpoint)
               x-high (> x1 x-midpoint)
               y-low  (< y0 y-midpoint)
               y-high (> y1 y-midpoint)
               z-low  (< z0 z-midpoint)
               z-high (> z1 z-midpoint)
               ;; There has got to be a better way ...
               nodes (if (and x-low y-low z-low)
                       (assoc nodes
                         0 (oct-assoc (nodes 0) o x0 y0 z0 x1 y1 z1
                                      half-size min-dim
                                      node-x0 node-y0 node-z0))
                       nodes)
               nodes (if (and x-high y-low z-low)
                       (assoc nodes
                         1 (oct-assoc (nodes 1) o x0 y0 z0 x1 y1 z1
                                      half-size min-dim
                                      x-midpoint node-y0 node-z0))
                       nodes)
               nodes (if (and x-low y-high z-low)
                       (assoc nodes
                         2 (oct-assoc (nodes 2) o x0 y0 z0 x1 y1 z1
                                      half-size min-dim
                                      node-x0 y-midpoint node-z0))
                       nodes)
               nodes (if (and x-high y-high z-low)
                       (assoc nodes
                         3 (oct-assoc (nodes 3) o x0 y0 z0 x1 y1 z1
                                      half-size min-dim
                                      x-midpoint y-midpoint node-z0))
                       nodes)
               nodes (if (and x-low y-low z-high)
                       (assoc nodes
                         4 (oct-assoc (nodes 4) o x0 y0 z0 x1 y1 z1
                                      half-size min-dim
                                      node-x0 node-y0 z-midpoint))
                       nodes)
               nodes (if (and x-high y-low z-high)
                       (assoc nodes
                         5 (oct-assoc (nodes 5) o x0 y0 z0 x1 y1 z1
                                      half-size min-dim
                                      x-midpoint node-y0 z-midpoint))
                       nodes)
               nodes (if (and x-low y-high z-high)
                       (assoc nodes
                         6 (oct-assoc (nodes 6) o x0 y0 z0 x1 y1 z1
                                      half-size min-dim
                                      node-x0 y-midpoint z-midpoint))
                       nodes)
               nodes (if (and x-high y-high z-high)
                       (assoc nodes
                         7 (oct-assoc (nodes 7) o x0 y0 z0 x1 y1 z1
                                      half-size min-dim
                                      x-midpoint y-midpoint z-midpoint))
                       nodes)]
           (assoc oct :nodes nodes))))))

(defn oct-assoc-box
  "Convenience function; takes an octree and a box and returns the
  octree with the box added"
  ([oct box]
     (let [ {x0 :x0, y0 :y0, z0 :z0,
             xd :xd, yd :yd, zd :zd,} box]
       (oct-assoc oct box x0 y0 z0
                  (+ x0 xd)
                  (+ y0 yd)
                  (+ z0 zd)))))

(defn- contents-intersecting?
  ([oct x0 y0 z0 x1 y1 z1]
     (let [contents (:contents oct)]
       (some (fn [^OctreeContents c]
               (and (> (long x1) (.x0 c))
                    (> (long y1) (.y0 c))
                    (> (long z1) (.z0 c))
                    (< (long x0) (.x1 c))
                    (< (long y0) (.y1 c))
                    (< (long z0) (.z1 c))))
             contents))))

(defn oct-any-intersecting?
  "Given an octree and two coordinates defining a cuboid region,
  returns true if the octree contains anything that intersects the
  region"
  ([oct x0 y0 z0 x1 y1 z1]
     (when oct
       (if (or (<= x1 (:x0 oct))
               (<= y1 (:y0 oct))
               (<= z1 (:z0 oct)))
         nil
         (let [size (:size oct)
               oct-x1 (+ size (:x0 oct))
               oct-y1 (+ size (:y0 oct))
               oct-z1 (+ size (:z0 oct))]
           (if (or (>= x0 oct-x1)
                   (>= y0 oct-y1)
                   (>= z0 oct-z1))
             nil
             (or (contents-intersecting? oct x0 y0 z0 x1 y1 z1)
                 (some #(oct-any-intersecting? % x0 y0 z0 x1 y1 z1)
                       (:nodes oct)))))))))

(defn- intersecting-contents
  ([oct x0 y0 z0 x1 y1 z1]
     (let [contents (:contents oct)]
       (map :o
            (filter (fn [^OctreeContents c]
                      (and (> (long x1) (.x0 c))
                           (> (long y1) (.y0 c))
                           (> (long z1) (.z0 c))
                           (< (long x0) (.x1 c))
                           (< (long y0) (.y1 c))
                           (< (long z0) (.z1 c))))
                    contents)))))

(defn oct-intersecting
  "Given an octree and two coordinates defining a cuboid region,
  returns any contents of the octree intersecting the region"
  ([oct x0 y0 z0 x1 y1 z1]
     (let [lx0 (long x0) ly0 (long y0) lz0 (long z0)
           lx1 (long x1) ly1 (long y1) lz1 (long z1)]
       ( (fn oct-intersecting' [oct]
           (when oct
             (let [ox0 (long (:x0 oct))
                   oy0 (long (:y0 oct))
                   oz0 (long (:z0 oct))]
               (if (or (<= lx1 ox0)
                       (<= ly1 oy0)
                       (<= lz1 oz0))
                 nil
                 (let [size (long (:size oct))
                       oct-x1 (+ size ox0)
                       oct-y1 (+ size oy0)
                       oct-z1 (+ size oz0)]
                   (if (or (>= lx0 oct-x1)
                           (>= ly0 oct-y1)
                           (>= lz0 oct-z1))
                     nil
                     (if (:contents oct)
                       (concat (intersecting-contents oct x0 y0 z0 x1 y1 z1)
                               (mapcat oct-intersecting' (:nodes oct)))
                       (mapcat oct-intersecting' (:nodes oct)))))))))
         oct))))

(defn oct-lookup
  "Given an octree and a point, return a seq of all contents of the
  octree overlapping that point"
  ([oct x y z]
     (oct-intersecting oct x y z (inc x) (inc y) (inc z))))

