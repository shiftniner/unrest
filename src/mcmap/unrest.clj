(ns mcmap.unrest
  (:use mcmap.core
        mcmap.cavern
        mcmap.srand
        mcmap.blocks
        mcmap.util
        mcmap.dungeon.placement
        mcmap.dungeon.build
        mcmap.dungeons
        mcmap.layout
        mcmap.balance
        mcmap.toolkit)
  (:import java.io.File))

(set! *warn-on-reflection* true)

(def +min-start-height+ 63)

(def +default-n-dungeons+ 64)

(defn distribute-prizes
  "Takes a seq of one or more chest content seqs, a seq of hallways,
  and a maximum y coordinate, and returns a seq of :prize values to be
  used when rendering the dungeons corresponding to the given
  hallways"
  ([quest-chests hallways max-y]
     (when (seq hallways)
       (lazy-seq
        (let [n-hallways (count hallways)
              n-chests (count quest-chests)
              _ (when (zero? n-hallways)
                  (die "Ran out of dungeons to place chests in"))
              placement-probability (/ n-chests n-hallways)
              y (hallway-y (first hallways))
              height-frac (when y (/ (- y 8)
                                     (- max-y 8)))
              chest? (and height-frac
                          (pos? n-chests)
                          (<= height-frac placement-probability))]
          (if chest?
            (cons (first quest-chests)
                  (distribute-prizes (rest quest-chests)
                                     (rest hallways)
                                     max-y))
            (cons nil
                  (distribute-prizes quest-chests
                                     (rest hallways)
                                     max-y))))))))

(defn new-level-dat
  "Takes a start x, y, and z, and an options hash, and returns an
  uncompressed level.dat"
  ([x y z opts]
     (tag-compound ""
      [(tag-compound "Data"
         [ (tag-byte "thundering" 1)
           (tag-long "LastPlayed" (System/currentTimeMillis))
           (tag-compound "Player"
                         [ (tag-list "Motion" 6
                                     [ (tag-double 0)
                                       (tag-double 0)
                                       (tag-double 0)])
                           (tag-float "foodExhaustionLevel" 0)
                           (tag-int "foodTickTimer" 0)
                           (tag-int "XpLevel" 0)
                           (tag-short "Health" 20)
                           (tag-list "Inventory" 10 [])
                           (tag-short "AttackTime" 0)
                           (tag-byte "Sleeping" 0)
                           (tag-short "Fire" -20)
                           (tag-int "foodLevel" 20)
                           (tag-int "Score" 0)
                           (tag-short "DeathTime" 0)
                           (tag-float "XpP" 0)
                           (tag-short "SleepTimer" 0)
                           (tag-short "HurtTime" 0)
                           (tag-byte "OnGround" 1)
                           (tag-int "Dimension" 0)
                           (tag-short "Air" 300)
                           (tag-list "Pos" 6
                                     [ (tag-double x)
                                       (tag-double y)
                                       (tag-double z)])
                           (tag-float "foodSaturationLevel"
                                      (or (:food-saturation opts)
                                          20))
                           (tag-compound "Abilities"
                              [ (tag-byte "flying" 0)
                                (tag-byte "instabuild"
                                          (if (or (:instabuild opts)
                                                  (:creative opts))
                                            1 0))
                                (tag-byte "mayfly"
                                          (if (or (:mayfly opts)
                                                  (:creative opts))
                                            1 0))
                                (tag-byte "invulnerable"
                                          (if (or (:iddqd opts)
                                                  (:creative opts))
                                            1 0))])
                           (tag-float "FallDistance" 0)
                           (tag-int "XpTotal" 0)
                           (tag-list "Rotation" 5
                                     [ (tag-float 0)
                                       (tag-float 0)])])
           (tag-long "RandomSeed" (or (:minecraft-seed opts)
                                      (long (* Math/PI 2e18))))
           (tag-int "GameType" (if (or (:creative opts)
                                       (:semi-creative opts))
                                 1 0))
           (tag-byte "MapFeatures" 0)
           (tag-int "version" 19133)
           (tag-long "Time" 0)
           (tag-byte "raining" 1)
           (tag-int "thunderTime" 20)
           (tag-int "SpawnX" (int x))
           (tag-byte "hardcore" (if (:hardcore opts)
                                  1 0))
           (tag-int "SpawnY" (int y))
           (tag-int "SpawnZ" (int z))
           (tag-string "LevelName" (or (:level-name opts)
                                       "Unrest map"))
           (tag-string "generatorName" (or (:generator opts)
                                           "default"))
           (tag-long "SizeOnDisk" 0)
           (tag-int "rainTime" 20)
           (tag-int "generatorVersion" 1)])])))

(defn add-more-chests
  "Takes a maximum dimension and a seq of non-intersecting dunhalls,
  followed by any number of argument pairs each consisting of
  near-unlimited seqs of possibly-intersecting chest dunhalls and the
  number of non-intersecting ones to add from that seq, and returns a
  seq of non-intersecting dunhalls including chests"
  ([max-dim nid]
     nid)
  ([max-dim nid id n & more]
     (let [n-nid (count nid)
           n-tot (+ n-nid n)]
       (apply add-more-chests
              max-dim
              (take n-tot
                    (non-intersecting-dunhalls (concat nid id)
                                               max-dim))
              more))))

(defn bedrock-height
  "Given a block zone and x and z coordinates, returns the y
  coordinate of the highest bedrock block at that x and z"
  ([zone x z]
     (inc (or (first (filter #(= :bedrock (zone-lookup zone x % z))
                             (range (dec (zone-y-size zone))
                                    -1 -1)))
              -1))))

(defn quest-cavern-map
  "Given a seq of one or more chest content seqs describing quest
  items, a game seed, a cavern seed, a level, returns a generator for
  an epic cave network with dungeons in it, one or more of which have
  quest items"
  ([quest-chests seed cavern-seed level options]
     (let [n-caves    (or (:n-caves    options) 15)
           n-dungeons (or (:n-dungeons options) +default-n-dungeons+)
           max-x      (or (:map-side   options) 256)
           max-y      (or (:map-height options) 128)
           max-z      (or (:map-side   options) 256)
           map-difficulty (/ level 100)
           start-difficulty (- 1 map-difficulty)
           cavern-wall   (mc-block :sandstone)
           lower-wall    (mc-block :sandstone-half-slab)
           upper-wall    (mc-block :sandstone-upper-half-slab)
           cavern-fill   (mc-block :snow-block)
           uncommon-wall (mc-block :smooth-sandstone)
           rare-wall     (mc-block :creeper-sandstone)
           reward-frac  1.388406820377468
           reward-start (* 100 (dec (scale-pain reward-frac
                                                (max 0.01 map-difficulty))))

           cavern-size-factor (* (/ n-caves 15)
                                 (/ max-y 128)
                                 1.0)
           max-dim (max max-x max-y max-z)
           [epic-zone start-x start-z]
               (epic-cave-network n-caves max-x max-y max-z cavern-seed
                                  options)
           _ (println "Start is x=" start-x " z=" start-z)
           _ (msg 3 "Finding dungeons ...")
           excess-dunhalls (pmap pick-dungeon-place
                                 (repeat epic-zone)
                                 (map #(reseed seed 1 %)
                                      (vtake 1000 100
                                             "Tried %d dungeons ..."
                                             (range 1000)))
                                 (repeat new-air-finder)
                                 (repeat #{:ground})
                                 (transition 1000
                                             pick-hallway
                                             pick-complex-hallway)
                                 (repeat cave-hallway-accepter)
                                 (repeat (apply get-dungeons
                                                (concat
                                                 (repeat 6 :std)
                                                 (repeat 3 :uncommon)
                                                 (repeat 1 :rare))))
                                 (repeat nil))
           excess-dunhalls (filter identity excess-dunhalls)
           dunhalls (vtake n-dungeons 5 "Got %d dungeons ..."
                           (non-intersecting-dunhalls excess-dunhalls
                                                      max-dim))
           actual-dungeons dunhalls
           prizes (distribute-prizes quest-chests
                                     (map second dunhalls)
                                     max-y)
           _ (msg 3 "Rendering dungeons ...")
           dungeons (map first dunhalls)
           hallways (map second dunhalls)
           _ (dorun (pmap render-dungeon
                          (apply concat dunhalls)
                          (dup-seq
                           (map (fn [hallway dungeon prize]
                                  (let [y (or (hallway-y hallway)
                                              (:y0 (second dungeon))
                                              (/ max-y 2))
                                        y-frac (if prize
                                                 1
                                                 (- 1 (/ y max-y)))]
                                    {:pain (dungeon-pain
                                            y-frac
                                            map-difficulty
                                            start-difficulty)
                                     :reward (* (Math/pow 256.0
                                                          y-frac)
                                                reward-start)
                                     :prize prize}))
                                hallways dungeons (concat prizes
                                                          (repeat nil))))))
           torch-chests (add-chests epic-zone 1000
                                    (prize-items -1 :torch)
                                    ["" "Torches"]
                                    (rand-place-fn max-x (- max-y 5)
                                                   max-z)
                                    (reseed seed 2))
           rail-chests (add-chests epic-zone 1000
                                   (prize-items +chest-slots+
                                                -1/100 :minecart
                                                -4 :powered-rail
                                                -4 :redstone-torch-on
                                                -1/100 :button
                                                -1/100 :detector-rail
                                                -1/100 :redstone-dust
                                                -20 :rail)
                                   (str "Catch a riiiiiiiiiiiiiiiiiiiii"
                                        "iiiiide?")
                                   (rand-place-fn max-x (- max-y 15)
                                                  max-z)
                                   (reseed seed 3))
           worchbench-chests (add-chests epic-zone 1000
                                         (prize-items +chest-slots+
                                                      1 :air
                                                      4 :wood-double-slab)
                                         ["" "Worchbench" "level 40"]
                                         (rand-place-fn max-x (/ max-y 2)
                                                        max-z)
                                         (reseed seed 4))
           bucket-chests (add-chests epic-zone 1000
                                     (prize-items +chest-slots+
                                                  1 :air
                                                  2 :iron-ingot
                                                  1 :air
                                                  1 :iron-ingot)
                                     ["" "Free" "bucket"]
                                     (rand-place-fn max-x (/ max-y 2)
                                                    max-z)
                                     (reseed seed 5))
           sapling-chests (add-chests epic-zone 1000
                                      (prize-items +chest-slots+
                                                   1 :dead-bush
                                                   3 :air
                                                   1 :fire)
                                      ["" "Sapling" "incubator"]
                                      (rand-place-fn max-x (/ max-y 2)
                                                     max-z)
                                      (reseed seed 6))
           saddle-chests (add-chests epic-zone 1000
                                     (prize-items 1 :saddle)
                                     ["" "Win the game"]
                                     (rand-place-fn max-x (/ max-y 2)
                                                    max-z)
                                     (reseed seed 6))
           dunhalls (add-more-chests max-dim
                                     dunhalls
                                     torch-chests
                                     (sround (* 96 cavern-size-factor)
                                             seed cavern-seed 1)
                                     rail-chests
                                     (sround (* 32 cavern-size-factor)
                                             seed cavern-seed 2)
                                     worchbench-chests
                                     (sround (* 3 cavern-size-factor)
                                             seed cavern-seed 3)
                                     bucket-chests
                                     (sround (* 3 cavern-size-factor)
                                             seed cavern-seed 4)
                                     sapling-chests
                                     (sround (* 2 cavern-size-factor)
                                             seed cavern-seed 5)
                                     saddle-chests
                                     (sround (* 2 cavern-size-factor)
                                             seed cavern-seed 6))
           _ (msg 3 "Got " (count actual-dungeons) " dungeons")
           _ (dorun (pmap render-dungeon
                          (apply concat (drop (count actual-dungeons)
                                              dunhalls))
                          (repeat {})))
           _ (msg 3 "Placing dungeons and hallways ...")
           dungeons (map first dunhalls)
           hallways (map second dunhalls)
           epic-zone (place-dungeons epic-zone dungeons hallways)
           _ (msg 3 "Adding crisp bedrock crust ...")
           bedrock-generator (fn [x y z]
                               (let [ze (zone-lookup epic-zone x y z)
                                     neighbors (neighbors-of epic-zone
                                                             x y z)]
                                 (if (every? #{:ground :bedrock}
                                             (cons ze neighbors))
                                   :bedrock
                                   ze)))
           epic-zone (gen-mcmap-zone max-x max-y max-z bedrock-generator)
           _ (msg 3 "Adding creamy middle ...")
           x-bound (dec max-x)
           z-bound (dec max-z)
           int-start-x (int start-x)
           int-start-z (int start-z)
           height-at-spawn (map-height epic-zone int-start-x int-start-z)
           bedrock-height-at-spawn (bedrock-height epic-zone int-start-x
                                                   int-start-z)
           start-y (+ 2 (max +min-start-height+ height-at-spawn))
           generator (fn [x y z]
                       (let [ze (when (< y max-y)
                                  (zone-lookup epic-zone x y z))
                             neighbors (neighbors-of epic-zone x y z)]
                         (cond (and (>= y bedrock-height-at-spawn)
                                    (<= y +min-start-height+)
                                    (< y start-y)
                                    (= x int-start-x)
                                    (= z int-start-z))
                                 (mc-block :bedrock)
                               (>= y max-y)
                                 (mc-block :air)
                               (or (zero? x) (zero? z)
                                   (= x-bound x) (= z-bound z))
                                 (mc-block :bedrock)
                               :else
                                 (case ze
                                       :bedrock
                                         (if (and (> y (- max-y 10))
                                                  (every? #(= :bedrock %)
                                                          neighbors))
                                           (mc-block :lava-source)
                                           :bedrock)
                                       :air (mc-block :air)
                                       :half-ground lower-wall
                                       :upper-half-ground upper-wall
                                       (:ground :cavern-wall)
                                         (let [r  (srand 1 seed 51 x y z)
                                               r2 (srand 1 seed 52 x y z)
                                               p (dungeon-pain
                                                  (- 1 (/ y max-y))
                                                  map-difficulty
                                                  start-difficulty)]
                                           (cond
                                            (and (> r 0.96)
                                                 (< r2 (Math/pow 1e-18 p)))
                                              :glowstone
                                            (and (> r 0.96)
                                                 (< (- 1 r2)
                                                    (* p (Math/pow 1e-5
                                                                   (- 1 p)))))
                                              (mc-block
                                               :mob-spawner
                                               :mob (pick-mob
                                                     +standard-mobs+
                                                     1 seed 1 54 x y z)
                                               :delay (int (snorm
                                                            [(* 200 (- 1 p))
                                                             50 0]
                                                            seed 53 x y z)))
                                            (< r 0.002) rare-wall
                                            (< r 0.012) uncommon-wall
                                            :else       cavern-wall))
                                         ze))))]
       [generator
        max-x max-y max-z max-dim
        start-x start-y start-z])))

(defn write-quest-cavern-map
  "Given a seq of one or more chest content seqs describing quest
  items, a game seed, an optional separate cavern seed, a level, and a
  Minecraft save directory, creates an epic cave network and puts
  dungeons in it, one or more of which have quest items"
  ([quest-chests seed level save-dir options]
     (write-quest-cavern-map quest-chests seed seed level save-dir
                             options))
  ([quest-chests seed cavern-seed level save-dir options]
     (let [ [generator max-x max-y max-z max-dim start-x start-y start-z]
              (quest-cavern-map quest-chests seed cavern-seed level
                                options)
            main-mcmap (gen-mcmap max-x
                                  (* 16 (int (/ (+ max-y 16)
                                                16)))
                                  max-z generator)
            _ (msg 0 "Counting spawners ...")
            _ (count-spawners (:block-zone main-mcmap))
            buffer-mcmap (gen-mcmap 128 96 128, -128 -128
                                    (fn [x y z]
                                      (if (= y 95)
                                        (mem-mc-block
                                         :smooth-stone-half-slab)
                                        :bedrock)))
            buffer-mcmaps (list*
                           (dup-mcmap buffer-mcmap -128 -128)
                           (dup-mcmap buffer-mcmap -128 max-z)
                           (dup-mcmap buffer-mcmap max-x -128)
                           (dup-mcmap buffer-mcmap max-x max-z)
                           (forcat [n (range -128 (+ 127 max-dim) 128)]
                                   [ (dup-mcmap buffer-mcmap n -128)
                                     (dup-mcmap buffer-mcmap -128 n)
                                     (dup-mcmap buffer-mcmap n max-z)
                                     (dup-mcmap buffer-mcmap max-x n)]))
            mmcmap (mcmaps-to-mmcmap (cons main-mcmap buffer-mcmaps))]
       (.mkdirs (File. (str save-dir "/region")))
       (write-file (str save-dir "/parameters.txt")
                   (-> (str "quest chests: " (pr-str quest-chests) "\n"
                            "   game seed: " seed "\n"
                            " cavern seed: " cavern-seed "\n"
                            "       level: " level "\n"
                            "     options: " (pr-str options) "\n")
                       .getBytes
                       byte-buffer))
       (doseq [x0 [0 -1], z0 [0 -1]]
         (write-file (str save-dir "/region/r." x0 "." z0 ".mca")
                     (mmcmap-to-mca-binary mmcmap x0 z0)))
       (write-file (str save-dir "/level.dat")
                   (gzip-compress (new-level-dat start-x start-y start-z
                                                 options))))))

(defn record-quest-map
  "Given a game seed, optional separate cavern seed, level, and an
  existing Minecraft save directory (which will be overwritten)
  generates a record-quest game map"
  ([& args]
     (let [options (last args)
           n-dungeons (or (:n-dungeons options)
                          +default-n-dungeons+)
           [record min-spiral-radius]
             (cond (> n-dungeons 45) [:13-disc   8]
                   (> n-dungeons 20) [:cat-disc  12]
                   :else             [:mall-disc 16])]
       (binding [*min-spiral-radius* min-spiral-radius]
         (apply write-quest-cavern-map [(prize-items 1 record)]
                args)))))
