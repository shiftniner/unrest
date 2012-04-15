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
        mcmap.toolkit))

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
                           (tag-int "SpawnX" (int x))
                           (tag-short "Fire" -20)
                           (tag-int "SpawnY" (int y))
                           (tag-int "SpawnZ" (int z))
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
           (tag-int "thunderTime" 5)
           (tag-int "SpawnX" (int x))
           (tag-byte "hardcore" (if (:hardcore opts)
                                  1 0))
           (tag-int "SpawnY" (int y))
           (tag-int "SpawnZ" (int z))
           (tag-string "LevelName" (or (:level-name opts)
                                       "Unrest map"))
           (tag-string "generatorName" (or (:generator opts)
                                           "FLAT"))
           (tag-long "SizeOnDisk" 0)
           (tag-int "rainTime" 5)
           (tag-int "generatorVersion" 1)])])))

(defn quest-cavern-map
  "Given a seq of one or more chest content seqs describing quest
  items, a game seed, an optional separate cavern seed, a level, and a
  Minecraft save directory, creates an epic cave network and puts
  dungeons in it, one or more of which have quest items"
  ([quest-chests seed level save-dir options]
     (quest-cavern-map quest-chests seed seed level save-dir options))
  ([quest-chests seed cavern-seed level save-dir options]
     (let [n-caves 15
           n-dungeons 64
           chunks 16
           map-difficulty (/ level 100)
           start-difficulty 0.8
           cavern-wall   (mc-block :sandstone)
           cavern-fill   (mc-block :snow-block)
           uncommon-wall (mc-block :smooth-sandstone)
           rare-wall     (mc-block :creeper-sandstone)
           reward-frac  1.388406820377468
           reward-start (* 100 (dec (scale-pain reward-frac
                                                (max 0.01 map-difficulty))))

           max-x (* chunks +chunk-side+)
           max-y 128
           max-z (* chunks +chunk-side+)
           max-dim (max max-x max-y max-z)
           [epic-zone start-x start-z]
               (epic-cave-network n-caves max-x max-y max-z cavern-seed)
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
                                   (reseed seed 2))
           dunhalls (vtake (+ 128 n-dungeons)
                           64 "Dungeons/torch chests: %d"
                           (non-intersecting-dunhalls
                            (concat dunhalls
                                    (interleave-n 3 torch-chests
                                                  1 rail-chests))
                            max-dim))
           dungeons (map first dunhalls)
           hallways (map second dunhalls)
           _ (msg 3 "Rendering dungeons ...")
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
           _ (msg 3 (str "Got " (count actual-dungeons) " dungeons"))
           _ (msg 3 "Placing dungeons and hallways ...")
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
           generator (fn [x y z]
                       (let [ze (zone-lookup epic-zone x y z)
                             neighbors (neighbors-of epic-zone x y z)]
                         (cond (or (zero? x) (zero? z)
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
                                       ze))))
           mca-file (generic-map-maker chunks chunks generator)
           start-y (inc (map-height epic-zone
                                    (int start-x)
                                    (int start-z)))]
       (write-file (str save-dir "/region/r.0.0.mca")
                   mca-file)
       (write-file (str save-dir "/level.dat")
                   (gzip-compress (new-level-dat start-x start-y start-z
                                                 options))))))

(def record-quest-map
     "Given a game seed, optional separate cavern seed, level, and an
  existing Minecraft save directory (which will be overwritten)
  generates a gold-record-quest game map"
     (partial quest-cavern-map [(prize-items 1 :13-disc)]))
