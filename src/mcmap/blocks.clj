(ns mcmap.blocks)


(def +light-levels+
     {:fire 15
      :jack-o-lantern 15
      :lava-source 15
      :lava-flow 15
      :glowstone 15
      :locked-chest 15
      :torch 14
      :burning-furnace 13
      :portal 11
      :glowing-redstone-ore 9
      :redstone-repeater-on 9
      :redstone-torch-on 7
      :brown-mushroom 1
      :dragon-egg 1
      :brewing-stand 1})

(defn light-emitting-block-types
  ([]
     (keys +light-levels+)))

(def +opacity+
     ;; XXX partial list for now
     (apply hash-map
            (concat (interleave [:stone
                                 :wool
                                 :glowstone
                                 :bedrock
                                 :sandstone
                                 :moss-stone
                                 :wood
                                 :cobble
                                 :iron-block
                                 :diamond-block
                                 :stone-bricks]
                                (repeat 255))
                    (interleave [:piston-target
                                 :water-flow
                                 :water-source
                                 :lava-flow
                                 :lava-source]
                                (repeat 1)))))
