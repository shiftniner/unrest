(ns unrest.test.core
  (:use [unrest core cavern util balance toolkit dungeons blocks srand
                quest-map layout]
        [unrest.dungeon build placement]
        [clojure.test]))

;;; I don't want to break anyone's favorite seed.  Unlike some people.
;;; Ahem.

(let [one (java.math.BigInteger/ONE)
      bitmask (.subtract (.shiftLeft one 256) one)
      multiplier (java.math.BigInteger. "127")]
  (defn checksum-seq
    "Takes a seq of numbers and returns a 256-bit checksum for them"
    ([ns]
       (reduce (fn [^BigInteger a ^BigInteger b]
                 (.and bitmask
                       (.add b
                             (.multiply a multiplier))))
               one
               (map #(if (instance? BigInteger %)
                       %
                       (BigInteger/valueOf (long %)))
                    ns)))))

(defn zone-checksum
  "Takes a zone and returns a 256-bit checksum for it"
  ([zone]
     (checksum-seq
      (for [x (range (zone-x-size zone))]
        (checksum-seq
         (for [z (range (zone-z-size zone))
               y (range (zone-y-size zone))]
           (hash (zone-lookup zone x y z))))))))

(deftest checksums
  (testing "Testing the underlying zone-checksum test infrastructure"
    (is (= (checksum-seq [])
           1))
    (is (= (checksum-seq [0])
           127))
    (is (= (checksum-seq [0 0 0])
           2048383))
    (is (= (checksum-seq [1000])
           1127))
    (is (= (checksum-seq [-1000])
           (bignum 1157920892373161954235709850086879078532699846656405
                   64039457584007913129639063)))
    (is (= (checksum-seq (range 1000))
           (bignum 8858038687629214766623955830174043534271365942043770
                   1600814724188529539976693)))
    (is (= (zone-checksum (gen-mcmap-zone 1 1 1 (fn [x y z] 1)))
           255))
    (is (= (zone-checksum (gen-mcmap-zone 2 2 2 (fn [x y z] 1)))
           33562804097))
    (is (= (zone-checksum (gen-mcmap-zone 4 4 4 (fn [x y z] 1)))
           9530968806614672378527701940318915854081))
    (is (= (zone-checksum (gen-mcmap-zone 20 20 20 (fn [x y z] 1)))
           (bignum 1110062776744555089975633303793695442599661758652158
                   "09788527157691703697406721")))
    (is (= (zone-checksum (gen-mcmap-zone 8 8 8 (fn [x y z] :a)))
           (bignum 7031538355156702668427685792414433255295429105897687
                   "0278235343144045588512257")))))

(deftest prng
  (testing "Testing the deterministic pseudorandom number generator"
    (is (= (srand 1 1 1)
           0.08155282211232773))
    (is (= (srand 1 1 1 1)
           0.35557215488548655))
    (is (= (checksum-seq
            (map #(hash (srand 1 2 %))
                 (range 1000)))
           (bignum 9824952054612774021870932819786049127345506123810497
                   7485335467017169485807721)))
    (is (= (checksum-seq
            (map #(reseed 1 2 %)
                 (range 1000)))
           (bignum 7398901432784089773031875579681418679263958856023163
                   1208839608935665604524691)))
    (is (= (checksum-seq
            (map #(hash (snorm [1.0 0.5 0.2 1.8] 1 2 %))
                 (range 1000)))
           (bignum 9180888462161956113035356052743695509081875512129894
                   1983551021239782922593507)))
    (is (= (checksum-seq
            (map #(hash (sranditem [:a :b :c :d :e :f :g :h :i :j :k]
                                   1 2 %))
                 (range 1000)))
           (bignum 3301869143399570609235029850189903263621321357006372
                   8117672329456545254462345)))
    (is (= (checksum-seq
            (map #(hash (sshuffle [:a :b :c :d :e :f :g :h :i :j :k]
                                  1 2 %))
                 (range 1000)))
           (bignum 4761875820561524687369556550649362716089672838199582
                   "0633501087769515193280553")))
    (is (= (checksum-seq
            (map #(sround 1.1 2 %)
                 (range 1000)))
           (bignum 1095129250671158472754078507424360796771977470809572
                   93054552138480518656813447)))
    (is (= (checksum-seq
            (map #(sround 1.8 2 %)
                 (range 1000)))
           (bignum 5173222064496105673860884757422854283652177484803608
                   1134236686480364203114755)))))

(deftest caverns
  (testing "Testing cavern generation"
    (binding [*map-gen-version* 1]
      (let [ [z sx sz]
               (noprint (epic-cave-network 4 64 64 64 1 {}))]
        (is (= (zone-checksum z)
               (bignum 352104113010423610463483464374975471656521070936
                       97051474295604563063687448593)))
        (is (= sx 15.057165838693523))
        (is (= sz 32.47947172579647)))
      (let [ [z sx sz]
               (noprint (epic-cave-network 4 128 49 128 2 {}))]
        (is (= (zone-checksum z)
               (bignum 553748111353760343377832565248239635722665942879
                       65748152525096807614694222277)))
        (is (= sx 68.67277716744078))
        (is (= sz 33.92345662985129)))
      (let [e (try (noprint (epic-cave-network 2 128 48 128 2 {}))
                   (catch Exception e e))]
        (is (instance? Exception e))))))

(defn dungeon-checksum
  "Takes a rendered dungeon and returns a checksum for its zones"
  ([d]
     (checksum-seq
      (map zone-checksum
           (map (fn [b] @(:zone b))
                (rest d))))))

(defn prize-seq-checksum
  "Takes values from prize-chest-contents and returns a checksum"
  ([ps]
     (checksum-seq (map hash ps))))

(deftest chests
  (testing "Testing prize chests"
    (binding [*balance-version* [1 2 5 :a]]
      (is (= (dungeon-checksum (render-dungeon (prize-chest 2)
                                               {:reward 100}))
             1039209778))
      (is (= (prize-seq-checksum (prize-chest-items {:reward 100}
                                                    1))
             106003260754663599673953220694531516397193010338))
      (is (= (dungeon-checksum (render-dungeon (prize-chest 2)
                                               {:reward 2000}))
             1113979714))
      (is (= (prize-seq-checksum (prize-chest-items {:reward 2000}
                                                    1))
             (bignum 11579208923731619542357098500865981914125008742520
                     6001325321982907900346511447)))
      (is (= (prize-seq-checksum (prize-chest-items {:reward 50000}
                                                    15))
             6330164902744085885507629764322235115771304428497351264)))))

(deftest dungeons
  (testing "Testing available dungeons"
    (binding [*map-gen-version* 1]
      (is (set= (dungeon-types)
                [:uncommon :home :std :rare]))
      (is (set= (get-dungeons :std)
                [:hello-dungeon :spite-and-malice]))
      (is (set= (get-dungeons :uncommon)
                [:back-and-forth :back-and-forth-backwards
                 :back-and-forth-ii]))
      (is (set= (get-dungeons :rare)
                [:stairs-and-flowers :a-hole-in-the-wall]))
      (is (set= (get-dungeons :home)
                [:life-in-a-glass-house]))))
  (testing "Testing rendered dungeons"
    (binding [*map-gen-version* 1
              *balance-version* [1 2 5 :a]]
      (is (= (dungeon-checksum (render-dungeon
                                (get-dungeon :hello-dungeon 64 1)
                                {:pain 0.3 :reward 1000}))
             (bignum 67561080368486769601746767309499942476895672061687
                     443655260839251659617966970)))
      (is (= (dungeon-checksum (render-dungeon
                                (get-dungeon :hello-dungeon 64 1)
                                {:pain 1.0 :reward 1000}))
             (bignum 10748729452598738305369691168819944509558283787650
                     7649063421380424861144338420)))
      (is (= (dungeon-checksum (render-dungeon
                                (get-dungeon :hello-dungeon 64 1)
                                {:pain 0.0 :reward 1000}))
             (bignum 49578957278297525849507159472188405017222530697244
                     970527634169695240248237351)))
      (is (= (dungeon-checksum (render-dungeon
                                (get-dungeon :spite-and-malice 64 1)
                                {:pain 0.3 :reward 1000}))
             (bignum 56085526336405932130891835176040005092844110004190
                     474095930191873944926072997)))
      (is (= (dungeon-checksum (render-dungeon
                                (get-dungeon :spite-and-malice 10 1)
                                {:pain 0.3 :reward 1000}))
             (bignum 92574915678685089953188172990331810184953296687397
                     254819725965137881943815121)))
      (is (= (dungeon-checksum (render-dungeon
                                (get-dungeon :back-and-forth 64 1)
                                {:pain 0.3 :reward 1000}))
             (bignum 28638488820246996265580367978870993015350346108780
                     60330262562952514703645766)))
      (is (= (dungeon-checksum (render-dungeon
                                (get-dungeon :back-and-forth-backwards
                                             64 1)
                                {:pain 0.3 :reward 1000}))
             (bignum 56757223853115810451443118480118015529950173643102
                     6658796408089673608962756)))
      (is (= (dungeon-checksum (render-dungeon
                                (get-dungeon :back-and-forth-ii 64 1)
                                {:pain 0.3 :reward 1000}))
             (bignum 68220471466778382886957170807444111126491127120834
                     374661005841526462839813733)))
      (is (= (dungeon-checksum (render-dungeon
                                (get-dungeon :stairs-and-flowers 41 1)
                                {:pain 0.4 :reward 1000}))
             (bignum 55434366082795534234964659168378461195225923430229
                     63784060838209603331981105)))
      (is (= (dungeon-checksum (render-dungeon
                                (get-dungeon :stairs-and-flowers 40 1)
                                {:pain 0.4 :reward 1000}))
             (bignum 37530287168388820958155067867574863422580098489918
                     166924954033464859688298092)))
      (is (= (dungeon-checksum (render-dungeon
                                (get-dungeon :a-hole-in-the-wall 64 1)
                                {:pain 0.3 :reward 1000}))
             (bignum 94943009034778704047507874466431577149029717867279
                     694624415191563638756246416)))
      (is (= (dungeon-checksum (render-dungeon
                                (get-dungeon :life-in-a-glass-house
                                             64 1 +wool-color-labels+)
                                {:pain 0.3 :reward 1000}))
             (bignum 88600566095165161654648427037754123071731850324372
                     982754055045373658789731048))))))

(defn checksum-dunhall-boxes
  "Takes a dunhall and returns a checksum based solely on the position
  of its boxes -- zones do not need to be rendered"
  ([dunhall]
     (checksum-seq
      (map #(checksum-seq
             (mapcat (fn [box]
                       (map (partial get box)
                            [:x0 :y0 :z0 :xd :yd :zd]))
                     (rest %)))
           dunhall))))

(deftest dungeon-placement
  (testing "Testing dungeon placement"
    (let [cave-zone (noprint (first (epic-cave-network 4 128 64 128 3 {})))
          excess-dunhalls (pmap pick-dungeon-place
                                (repeat cave-zone)
                                (map #(reseed 10 %)
                                     (range 1000))
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
                                (repeat nil))]
      (is (= (checksum-seq (map checksum-dunhall-boxes excess-dunhalls))
             (bignum 63647619842551145190250510626828066975794914467077
                     238304874969402304751578746))))))

(defn record-quest-map-zone
  "Takes a seed and level and returns a tiny record quest map zone,
  and the spawn coordinates for that map"
  ([seed level]
     (binding [*min-spiral-radius* 16
               *map-gen-version* 1
               *balance-version* [1 2 5 :a]]
       (let [ map-side 160
              map-height 70
              [generator max-x max-y max-z _ start-x start-y start-z]
                (noprint
                 (quest-cavern-map [(prize-items 1 :mall-disc)]
                                   seed seed level
                                   {:n-caves 3
                                    :n-dungeons 13
                                    :map-side map-side
                                    :map-height map-height}))]
         [ (gen-mcmap-zone map-side map-height map-side generator)
           start-x start-y start-z]))))

(deftest whole-map
  (testing (str "Integration test of full record-quest map generation"
                " (short of .mca binary formatting)")
    (let [ [zone start-x start-y start-z]
             (record-quest-map-zone 1 10)]
      (is (= (zone-checksum zone)
             (bignum 80664665432285799423717729215902083463071949606404
                     975224942305959473361608949)))
      (is (= start-x 43.43106540706475))
      (is (= start-y 69))
      (is (= start-z 83.32181557155305)))
    (let [ [zone start-x start-y start-z]
             (record-quest-map-zone 2 40)]
      (is (= (zone-checksum zone)
             (bignum 92152190526665064623783529009708473926093983761867
                     204010793134280752665604296)))
      (is (= start-x 91.431385278553))
      (is (= start-y 67))
      (is (= start-z 53.027816498064816)))))
