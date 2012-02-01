(ns mcmap.dungeons
  (:use mcmap.layout
        mcmap.blocks
        mcmap.core
        mcmap.srand))

;;; mcmap.dungeons contains definitions of various dungeons.
;;; mcmap.dungeon contains code for placing dungeons in a map.

(defn hello-dungeon
  ([y-level seed]
     (-> (stack (pad 19 17 19)
                (htable [(pad 7 2 19)]
                        [(pad 5 2 7) (spawners 5 2 5)]
                        [(pad 6 2 19)]
                        [(pad 1 2 7) (box (prize-chest))]))
         (surround :nether-brick)
         (surround :bedrock)
         (add-entrance [2 2 10]
                       "Hello, Dungeon"))))

