(ns mcmap.blocks)


(def +light-levels+
     {:fire                 15
      :jack-o-lantern       15
      :lava-source          15
      :lava-flow            15
      :glowstone            15
      :locked-chest         15
      :torch                14
      :burning-furnace      13
      :portal               11
      :glowing-redstone-ore  9
      :redstone-repeater-on  9
      :redstone-torch-on     7
      :brown-mushroom        1
      :dragon-egg            1
      :brewing-stand         1})

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
                                 :stone-bricks
                                 :nether-brick]
                                (repeat 255))
                    (interleave [:piston-target
                                 :water-flow
                                 :water-source
                                 :lava-flow
                                 :lava-source]
                                (repeat 1)))))

(defn mc-block
  "Returns the specified block in mcmap's internal data format"
  ([type]
     (case type
           :blue-wool    {:type :wool :datum 0xB}
           :yellow-wool  {:type :wool :datum 0x4}
           (:white-wool :wool)
                         {:type :wool :datum 0x0}
           :wood         {:type :wood :datum 0x0}
           :spruce       {:type :wood :datum 0x1}
           :birch        {:type :wood :datum 0x2}
           :south-ladder {:type :ladder :face :south}
           :north-ladder {:type :ladder :face :north}
           :east-ladder  {:type :ladder :face :east}
           :west-ladder  {:type :ladder :face :west}
           :moss-brick    {:type :stone-bricks :datum 0x1}
           :cracked-brick {:type :stone-bricks :datum 0x2}
           ;; default:
           type))
  ([type & extra-data]
     (apply hash-map :type type extra-data)))

(defn block-id
  "Returns the byte block ID for the given zone element"
  ([ze]
     (if (map? ze)
       (block-id (or (:real-type ze)
                     (:type ze)))
       (or ( {:air                     0
              :stone                   1
              :cobble                  4
              :bedrock                 7
              :lava-flow              10
              :lava-source            11
              :wood                   17
              :glass                  20
              :sandstone              24
              :wool                   35
              :piston-target          36
              :iron-block             42
              :moss-stone             48
              :fire                   51
              :mob-spawner            52
              :monster-spawner        52
              :chest                  54
              :redstone-wire          55
              :diamond-block          57
              :ladder                 65
              :wall-sign              68
              :redstone-torch-off     75
              :redstone-torch-on      76
              :glowstone              89
              :redstone-repeater-off  93
              :redstone-repeater-on   94
              :stone-bricks           98
              :nether-brick          112
              }
             ze)
           (throw (RuntimeException. (str "Block ID unknown for " ze)))))))

(defn mc-item
  "Returns either an item ID, or [id damage], for the given inventory
item"
  ([type]
     (or (case type
               :arrow             262
               :coal              263
               :string            287
               :leather           334
               :redstone-repeater 356
               :steak             364
               nil)
         (let [block (mc-block type)
               dmg (when (map? block)
                     (:datum block))]
           (if dmg
             [ (block-id block)
               dmg]
             (block-id block))))))

(defn sign-wrap-text
  "Takes a single string or a seq of strings and/or keywords (:newsign
to force a sign wrap, and :nosign to leave a blank spot instead of a
sign); returns a seq of either nil for blank spots or seqs of strings
for signs"
  ([text]
     (let [text (if (string? text)
                  [text]
                  text)
           split-line
             (fn split-line [t]
               ;; returns a seq of strings and keywords
               (if (or (not (string? t))
                       (<= (.length ^String t) 15))
                 [t]
                 (let [t ^String t
                       break-point (.lastIndexOf t (int \space) 15)
                       break-point (if (= -1 break-point)
                                     15 break-point)]
                   (cons (.substring t 0 break-point)
                         (split-line (.substring t (inc break-point)))))))
           text-lines (mapcat split-line text)
           split-sign
             (fn split-sign [ts]
               ;; returns a seq of either nils or seqs of strings
               (cond (not (seq ts))
                       nil
                     (and (= :newsign (first ts))
                          (= :newsign (second ts)))
                       (lazy-seq (cons nil (split-sign (rest ts))))
                     (= :newsign (first ts))
                       (recur (rest ts))
                     (= :nosign (first ts))
                       (lazy-seq (cons nil (split-sign (rest ts))))
                     :else
                       (let [f (take 4 (take-while string? ts))
                             flen (count f)]
                         (lazy-seq (cons f
                                         (split-sign (drop flen ts)))))))]
       (split-sign text-lines))))

