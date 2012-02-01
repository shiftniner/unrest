(ns mcmap.layout
  (:use mcmap.core))

(defn box
  "Returns a dungeon filled with the given zone element; default size
is 1x1x1"
  ([ze]
     (box ze 1 1 1))
  ([ze x-size y-size z-size]
     (let [z (promise)
           generator (if (and (> x-size 1)
                              ;; XXX 400 is a wild guess
                              (> (* y-size z-size) 400))
                       p-gen-mcmap-zone
                       gen-mcmap-zone)]
       [(fn [_]
          (deliver z
                   (generator x-size y-size z-size
                              (fn [_ _ _]
                                ze))))
        {:x0 0, :y0 0, :z0 0,
         :xd x-size, :yd y-size, :zd z-size
         :zone z}])))

(defn pad
  "Returns a dungeon of the given size filled with air"
  ([x y z]
     (box :air x y z)))

