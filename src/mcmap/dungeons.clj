(ns mcmap.dungeons
  (:use mcmap.layout
        mcmap.blocks
        mcmap.core
        mcmap.srand))

;;; mcmap.dungeons contains definitions of various dungeons.
;;; mcmap.dungeon contains code for placing dungeons in a map.

(defn hello-dungeon
  ([y-level seed]
     (-> (stack (htable [(pad 7 2 19)]
                        [(spawners 5 5 5 (reseed seed 1))]
                        [(pad 6 2 19)]
                        [(prize-chest)])
                (pad 19 14 19))
         (surround :nether-brick)
         (surround :bedrock)
         (surround :ground)
         (add-entrance [3 2 0]
                       "Hello, Dungeon"
                       (reseed seed 2)))))

