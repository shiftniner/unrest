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
     (oct-assoc oct o x0 y0 z0 x1 y1 z1
                (:max-dim oct)
                (:min-dim oct)
                (:x0 oct)
                (:y0 oct)
                (:z0 oct)))
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
               x-low  (<  x0 x-midpoint)
               x-high (>= x1 x-midpoint)
               y-low  (<  y0 y-midpoint)
               y-high (>= y1 y-midpoint)
               z-low  (<  z0 z-midpoint)
               z-high (>= z1 z-midpoint)
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


