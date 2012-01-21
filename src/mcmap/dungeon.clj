(ns mcmap.dungeon
  (:use mcmap.core
        mcmap.srand))

(def +hello-dungeon+
     [{:x0 6, :y0 0, :z0 0,
       :zone
         (gen-mcmap-zone 21 21 21
            (fn [x y z]
              (cond (and (= x 0)
                         (< 7 z 13)
                         (< 0 y 6))
                      :air
                    (some #{0 20} [x y z])
                      :mossy-cobblestone
                    (= [x y z] [10 1 10])
                      (mc-block :mob-spawner
                                :mob "Zombie" :delay 0)
                    :else
                      :air)))}
      {:x0 0, :y0 0, :z0 7,
       :zone
         (gen-mcmap-zone 6 6 6
            (fn [x y z]
              (cond (some #{0 5} [y z])
                      :mossy-cobblestone
                    (= [x y z] [3 3 1])
                      (mc-block :sign
                                :text ["" "Hello," "Dungeon"])
                    :else
                    :air)))}])

