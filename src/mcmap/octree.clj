(ns mcmap.octree)

(defn octree
  "Returns a new empty octree"
  ([max-dim min-dim]
     (let [point-size (first (drop-while #(> % 1)
                                         (iterate #(/ % 2) max-dim)))]
       (when (not= 1 point-size)
         (throw (RuntimeException. (str "size " max-dim
                                        " is not a power of two")))))
     {:max-dim max-dim
      :min-dim min-dim
      :x0 0, :y0 0, :z0 0
      :size max-dim
      :nodes [nil nil nil nil nil nil nil nil]
      :contents nil}))

(defn- add-to-contents
  "Given an octree node and an object, returns an octree node with the
object added directly at that node"
  ([oct o]
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
                    :nodes [nil nil nil nil nil nil nil]
                    :contents nil})]
       (if (<= node-size min-dim)
         (add-to-contents oct {:o o,
                               :x0 x0, :y0 y0, :z0 z0,
                               :x1 x1, :y1 y1, :z1 z1})
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
       (some (fn [ {cx0 :x0, cy0 :y0, cz0 :z0,
                    cx1 :x1, cy1 :y1, cz1 :z1} ]
               (and (> x1 cx0)
                    (> y1 cy0)
                    (> z1 cz0)
                    (< x0 cx1)
                    (< y0 cy1)
                    (< z0 cz1)))
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
