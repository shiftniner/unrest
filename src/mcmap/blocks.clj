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
                                 ;; opaque
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
                                 :nether-brick
                                 :obsidian
                                 :gold-block
                                 :snow-block]
                                (repeat 255))
                                 ;; partially opaque
                    (interleave [:piston-target
                                 :water-flow
                                 :water-source
                                 :lava-flow
                                 :lava-source]
                                (repeat 1)))))

(def +color+
     {:white 0
      :orange 1
      :magenta 2
      :light-blue 3
      :yellow 4
      :lime 5
      :pink 6
      :gray 7
      :light-gray 8
      :cyan 9
      :purple 10
      :blue 11
      :brown 12
      :green 13
      :red 14
      :black 15})

(def +color-array+
     (apply assoc
            (vec (repeat 16 nil))
            (reverse (apply concat +color+))))

(defn- block-colors
  "Returns cases for mc-block covering all 16 Minecraft colors for the
  given kind of block"
  ([kind]
     (mapcat (fn [color-key]
               [(keyword (str (name color-key)
                              "-" kind))
                {:type (keyword kind)
                 :datum (+color+ color-key)}])
             (keys +color+))))

(defn mc-block
  "Returns the specified block in mcmap's internal data format"
  ([type]
     #=(eval
        (list*
         'case 'type
;         :wood          {:type :wood :datum 0x0}
;         :spruce        {:type :wood :datum 0x1}
;         :birch         {:type :wood :datum 0x2}
         :moss-brick    {:type :stone-bricks :datum 0x1}
         :cracked-brick {:type :stone-bricks :datum 0x2}
         :old-tnt       {:type :tnt :datum 0x1}
         (concat (block-colors "wool")
                 ;; default:
                 ['type]))))
  ([type & extra-data]
     (apply hash-map :type type extra-data)))

(def mem-mc-block (memoize mc-block))

(defn block-id
  "Returns the byte block ID for the given zone element"
  ([ze]
     (if (map? ze)
       (block-id (or (:real-type ze)
                     (:type ze)))
       (or ( {:air                     0
              :stone                   1
              :dirt                    3
              :cobble                  4
              :bedrock                 7
              :water-flow              8
              :water-source            9
              :lava-flow              10
              :lava-source            11
              :sand                   12
              :gravel                 13
;              :wood                   17
              :glass                  20
              :dispenser              23
              :sandstone              24
              :wool                   35
              :piston-target          36
              :gold-block             41
              :iron-block             42
              :tnt                    46
              :moss-stone             48
              :obsidian               49
              :torch                  50
              :fire                   51
              :mob-spawner            52
              :monster-spawner        52
              :wood-stairs            53
              :chest                  54
              :redstone-wire          55
              :diamond-block          57
              :sign-post              63
              :ladder                 65
              :cobble-stairs          67
              :wall-sign              68
              :wood-pressure-plate    72
              :redstone-torch-off     75
              :redstone-torch-on      76
              :button                 77
              :ice                    79
              :snow-block             80
              :pumpkin                86
              :glowstone              89
              :jack-o-lantern         91
              :redstone-repeater-off  93
              :redstone-repeater-on   94
              :stone-bricks           98
              :vines                 106
              :brick-stairs          108
              :stone-brick-stairs    109
              :nether-brick          112
              :nether-brick-stairs   114
              }
             ze)
           (throw (RuntimeException. (str "Block ID unknown for " ze)))))))

(let [base-potions (map #(do [%1 %2])
                        ["regeneration"
                         "swiftness"
                         "fire-resistance"
                         "poison"
                         "healing"
                         "8198"
                         "8199"
                         "weakness"
                         "strength"
                         "slowness"
                         "8203"
                         "harming"]
                        (iterate inc 8193))
      potion-list (mapcat (fn [ [base baseval]]
                            [ [(str "potion-of-" base) baseval]
                              [(str "potion-of-" base "-ii") (+ baseval 32)]])
                          base-potions)
      potion-list (mapcat (fn [ [base baseval]]
                            [ [base baseval]
                              [(str "ext-" base) (+ baseval 64)]])
                          potion-list)
      potion-list (mapcat (fn [ [base baseval]]
                            [ [base baseval]
                              [(str "splash-" base) (+ baseval 8192)]])
                          potion-list)
      potion-keyvals (mapcat (fn [ [base baseval]]
                               [(keyword base) baseval])
                             potion-list)
      potions (apply hash-map potion-keyvals)]
  (defn mc-item
    "Returns either an item ID, or [id damage], for the given
  inventory item"
    ([type]
       (or (case type
                 :iron-pickaxe          257
                 :flint-and-steel       259
                 :red-apple             260
                 :bow                   261
                 :arrow                 262
                 :coal                  263
                 :diamond               264
                 :iron-ingot            265
                 :gold-ingot            266
                 :iron-sword            267
                 :wood-sword            268
                 :wood-pickaxe          270
                 :stone-sword           272
                 :stone-pickaxe         274
                 :diamond-sword         276
                 :diamond-pickaxe       278
                 :stick                 280
                 :bowl                  281
                 :mushroom-soup         282
                 :gold-sword            283
                 :gold-pickaxe          285
                 :string                287
                 :feather               288
                 :gunpowder             289
                 :wheat                 296
                 :bread                 297
                 :leather-cap           298
                 :leather-helmet        298
                 :leather-tunic         299
                 :leather-chestplate    299
                 :leather-pants         300
                 :leather-leggings      300
                 :leather-boots         301
                 :chainmail-helmet      302
                 :chainmail-chestplate  303
                 :chainmail-leggings    304
                 :chainmail-boots       305
                 :iron-helmet           306
                 :iron-chestplate       307
                 :iron-leggings         308
                 :iron-boots            309
                 :diamond-helmet        310
                 :diamond-chestplate    311
                 :diamond-leggings      312
                 :diamond-boots         313
                 :gold-helmet           314
                 :gold-chestplate       315
                 :gold-leggings         316
                 :gold-boots            317
                 :flint                 318
                 :raw-porkchop          319
                 :cooked-porkchop       320
                 :painting              321
                 :golden-apple          322
                 :sign                  323
                 :wood-door             324
                 :bucket                325
                 :water-bucket          326
                 :lava-bucket           327
                 :minecart              328
                 :saddle                329
                 :iron-door             330
                 :redstone              331
                 :snowball              332
                 :boat                  333
                 :leather               334
                 :milk-bucket           335
                 :slimeball             341
                 :minecart-with-chest   342
                 :minecart-with-furnace 343
                 :chicken-egg           344
                 :compass               345
                 :fishing-rod           346
                 :clock                 347
                 :glowstone-dust        348
                 :raw-fish              349
                 :cooked-fish           350
                 :bone                  352
                 :sugar                 353
                 :cake                  354
                 :bed                   355
                 :redstone-repeater     356
                 :cookie                357
                 :shears                359
                 :melon-slice           360
                 :raw-beef              363
                 :steak                 364
                 :raw-chicken           365
                 :cooked-chicken        366
                 :rotten-flesh          367
                 :ender-pearl           368
                 :blaze-rod             369
                 :ghast-tear            370
                 :gold-nugget           371
                 :nether-wart           372
                 :potion                373
                 :glass-bottle          374
                 :spider-eye            375
                 :fermented-spider-eye  376
                 :blaze-powder          377
                 :magma-cream           378
                 :brewing-stand         379
                 :cauldron              380
                 :eye-of-ender          381
                 :glistering-melon      382
                 :spawn-egg             383
                 :bottle-o-enchanting   384
                 :fire-charge           385
                 :13-disc               2256
                 :cat-disc              2257
                 :blocks-disc           2258
                 :chirp-disc            2259
                 :far-disc              2260
                 :mall-disc             2261
                 :mellohi-disc          2262
                 :stal-disc             2263
                 :strad-disc            2264
                 :ward-disc             2265
                 :11-disc               2266
                 nil)
           (if-let [potion-damage (potions type)]
             [(mc-item :potion) potion-damage])
           (let [block (mc-block type)
                 dmg (when (map? block)
                       (:datum block))]
             (if dmg
               [ (block-id block)
                 dmg]
               (block-id block)))))))

(defn sign-wrap-text
  "Takes a single string or a seq of strings and/or keywords (:newsign
  to force a sign wrap, and :nosign to leave a blank spot instead of a
  sign); returns a seq of either nil for blank spots or seqs of
  strings for signs"
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
                       break-point (.lastIndexOf t (int \space) 15)]
                   (if (= -1 break-point)
                     (cons (.substring t 0 15)
                           (split-line (.substring t 15)))
                     (cons (.substring t 0 break-point)
                           (split-line (.substring t (inc break-point))))))))
           text-lines (if (= :nowrap (first text))
                        (rest text)
                        (mapcat split-line text))
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

