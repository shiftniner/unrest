(ns mcmap.core
  (:use mcmap.blocks
        mcmap.util)
  (:import java.nio.ByteBuffer
           java.util.zip.DeflaterOutputStream
           java.util.zip.GZIPOutputStream
           java.io.ByteArrayOutputStream
           java.io.FileOutputStream))

(set! *warn-on-reflection* true)

(def +region-side+ (* 16 32))
(def +chunk-side+ 16)
(def +old-chunk-height+ 128)
(def +max-map-height+ 256)
(def +byte-buffer-concat-threshold+ 16)
;;; Number of blocks per z slice at which it is worthwhile to use pmap
;;; instead of for in gen-mcmap-zone.  XXX 400 is a wild guess;
;;; experiment.
(def ^:dynamic *size-at-which-pmap-faster* 400)
(def Infinity (- (Math/log 0)))
(def +default-biome+ 2)

(defn zone-x-size
  ([zone]
     (count zone)))

(defn zone-y-size
  ([zone]
     (count ( (zone 0) 0 ))))

(defn zone-z-size
  ([zone]
     (count (zone 0))))

(defn zone-lookup
  ([zone x y z]
     ( ( (zone x) z) y )))

(defn all-zone-elements
  ([zone]
     (flatten zone)))

(defn gen-mcmap-zone
  "Takes x and z dimensions (or x, y, and z dimensions), and a
  function of x y and z returning a block, and returns a zone of the
  specified size; typically uses pmap, so if thread-local bindings to
  dynamic variables must be preserved, use ct-gen-mcmap-zone"
  ([x-size z-size f]
     (gen-mcmap-zone x-size +old-chunk-height+ z-size f))
  ([x-size y-size z-size f]
     (if (and (> x-size 1)
              (> (* y-size z-size)
                 *size-at-which-pmap-faster*))
       (vec (pmap #(vec (for [z (range z-size)]
                          (mapv (fn [y] (f % y z))
                                (range y-size))))
                (range x-size)))
       (vec (for [x (range x-size)]
              (vec (for [z (range z-size)]
                     (mapv (fn [y] (f x y z))
                           (range y-size)))))))))

(defn ct-gen-mcmap-zone
  "Like gen-mcmap-zone, but guaranteed to evaluate completely within
  the current thread (\"CT\") to allow the use of (binding ...)"
  ([x-size z-size f]
     (ct-gen-mcmap-zone x-size +old-chunk-height+ z-size f))
  ([x-size y-size z-size f]
     (binding [*size-at-which-pmap-faster* Infinity]
       (gen-mcmap-zone x-size y-size z-size f))))

(defn p-gen-mcmap-zone
  "Like gen-mcmap-zone, but guaranteed to try to spread the job across
  threads; useful when generating relatively computationally intensive
  flat zones that are functions of large 3-D zones"
  ([x-size z-size f]
     (ct-gen-mcmap-zone x-size +old-chunk-height+ z-size f))
  ([x-size y-size z-size f]
     (binding [*size-at-which-pmap-faster* 0]
       (gen-mcmap-zone x-size y-size z-size f))))

(defn- rising-mcmap-column
  ([x z y-size f]
     (loop [y 1
            prev-block (f x 0 z nil)
            ret [prev-block]]
       (if (= y y-size)
         ret
         (let [next-block (f x y z prev-block)]
           (recur (inc y)
                  next-block
                  (conj ret next-block)))))))

(defn rising-recursive-gen-mcmap-zone
  "Takes x and z dimensions (or x, y, and z dimensions), and a
  function of x, y, z, and the result of calling the function on the
  next lower block (or nil for y=0), and returns a zone of the
  specified size"
  ([x-size z-size f]
     (rising-recursive-gen-mcmap-zone x-size +old-chunk-height+ z-size
                                      f))
  ([x-size y-size z-size f]
     (vec (pmap #(vec (for [z (range z-size)]
                        (rising-mcmap-column % z y-size f)))
                (range x-size)))))

(defn- falling-mcmap-column
  ([x z y-size f]
     (loop [y (- y-size 2)
            prev-block (f x (dec y-size) z nil)
            ret (list prev-block)]
       (if (= y -1)
         ret
         (let [next-block (f x y z prev-block)]
           (recur (dec y)
                  next-block
                  (cons next-block ret)))))))

(defn falling-recursive-gen-mcmap-zone
  "Takes x and z dimensions (or x, y, and z dimensions), and a
  function of x, y, z, and the result of calling the function on the
  next higher block (or nil for y=max), and returns a zone of the
  specified size"
  ([x-size z-size f]
     (falling-recursive-gen-mcmap-zone x-size +old-chunk-height+ z-size
                                       f))
  ([x-size y-size z-size f]
     (vec (pmap #(vec (for [z (range z-size)]
                        (vec (falling-mcmap-column % z y-size f))))
                (range x-size)))))

(defn southward-recursive-gen-mcmap-zone
  "Takes x and z dimensions (or x, y, and z dimensions), and a
  function of x, y, z, and the result of calling the function on the
  next block to the north (or nil for z=0), and returns a zone of the
  specified size"
  ([x-size z-size f]
     (southward-recursive-gen-mcmap-zone x-size +old-chunk-height+
                                         z-size f))
  ([x-size y-size z-size f]
     ; Swap z and y, call rising-recursive-gen-mcmap-zone, then swap z
     ; and y back
     (let [tmp-zone
             (rising-recursive-gen-mcmap-zone
                x-size z-size y-size
                (fn [x y z prev]
                  (f x z y prev)))]
       (gen-mcmap-zone x-size y-size z-size
                       (fn [x y z]
                         (zone-lookup tmp-zone x z y))))))

(defn northward-recursive-gen-mcmap-zone
  "Takes x and z dimensions (or x, y, and z dimensions), and a
  function of x, y, z, and the result of calling the function on the
  next block to the south (or nil for z=max), and returns a zone of
  the specified size"
  ([x-size z-size f]
     (northward-recursive-gen-mcmap-zone x-size +old-chunk-height+
                                         z-size f))
  ([x-size y-size z-size f]
     ; Swap z and y, call falling-recursive-gen-mcmap-zone, then swap
     ; z and y back
     (let [tmp-zone
             (falling-recursive-gen-mcmap-zone
                x-size z-size y-size
                (fn [x y z prev]
                  (f x z y prev)))]
       (gen-mcmap-zone x-size y-size z-size
                       (fn [x y z]
                         (zone-lookup tmp-zone x z y))))))

;;; TODO these next two are most in need of rewriting for better
;;; performance.

(defn eastward-recursive-gen-mcmap-zone
  "Takes x and z dimensions (or x, y, and z dimensions), and a
  function of x, y, z, and the result of calling the function on the
  next block to the west (or nil for x=0), and returns a zone of the
  specified size"
  ([x-size z-size f]
     (eastward-recursive-gen-mcmap-zone x-size +old-chunk-height+ z-size
                                        f))
  ([x-size y-size z-size f]
     ; Swap x and y, call rising-recursive-gen-mcmap-zone, then swap x
     ; and y back
     (let [tmp-zone
             (rising-recursive-gen-mcmap-zone
                y-size x-size z-size
                (fn [x y z prev]
                  (f y x z prev)))]
       (gen-mcmap-zone x-size y-size z-size
                       (fn [x y z]
                         (zone-lookup tmp-zone y x z))))))

(defn westward-recursive-gen-mcmap-zone
  "Takes x and z dimensions (or x, y, and z dimensions), and a
  function of x, y, z, and the result of calling the function on the
  next block to the east (or nil for x=max), and returns a zone of the
  specified size"
  ([x-size z-size f]
     (westward-recursive-gen-mcmap-zone x-size +old-chunk-height+ z-size
                                        f))
  ([x-size y-size z-size f]
     ; Swap x and y, call falling-recursive-gen-mcmap-zone, then swap
     ; x and y back
     (let [tmp-zone
             (falling-recursive-gen-mcmap-zone
                y-size x-size z-size
                (fn [x y z prev]
                  (f y x z prev)))]
       (gen-mcmap-zone x-size y-size z-size
                       (fn [x y z]
                         (zone-lookup tmp-zone y x z))))))

(defn maybe-zone-lookup
  "Like zone-lookup, but returns nil if the coordinates are out of
  bounds instead of throwing an exception"
  ([zone x y z]
     (if (or (neg? x) (neg? y) (neg? z)
             (>= x (zone-x-size zone))
             (>= y (zone-y-size zone))
             (>= z (zone-z-size zone)))
       nil
       (zone-lookup zone x y z))))

(defn neighbors-of
  "Returns a seq of up to six blocks adjacent to the given coordinates
  in the given zone"
  ([zone x y z]
     (lazy-seq
      (filter identity
              [(maybe-zone-lookup zone (inc x) y z)
               (maybe-zone-lookup zone (dec x) y z)
               (maybe-zone-lookup zone x (inc y) z)
               (maybe-zone-lookup zone x (dec y) z)
               (maybe-zone-lookup zone x y (inc z))
               (maybe-zone-lookup zone x y (dec z))]))))

(defn chunk-in-mcmap?
  "Returns true if and only if the given region and chunk coordinates
  include any part of the given zone"
  ([mcmap region-x region-z chunk-x chunk-z]
     (let [zone (:block-zone mcmap)
           mcmap-x0 (:x0 mcmap)
           mcmap-z0 (:z0 mcmap)
           chunk-x0 (+ (* +region-side+ region-x)
                       (* +chunk-side+  chunk-x))
           chunk-z0 (+ (* +region-side+ region-z)
                       (* +chunk-side+  chunk-z))]
       (and (<= mcmap-x0 chunk-x0 (+ mcmap-x0 -1 (zone-x-size zone)))
            (<= mcmap-z0 chunk-z0 (+ mcmap-z0 -1 (zone-z-size zone)))))))

(defn sub-zone
  "Returns a zone within the given zone, for the given coordinate
  ranges"
  ([zone x0 x1 y0 y1 z0 z1]
     (gen-mcmap-zone (- x1 x0)
                     (- y1 y0)
                     (- z1 z0)
                     #(zone-lookup zone
                                   (+ %1 x0)
                                   (+ %2 y0)
                                   (+ %3 z0)))))

(defn byte-buffer
  "Given a seq of bytes (or ints between -128 and 255; values over 127
  will be folded), returns a byte buffer"
  ([bs]
     ;; XXX try adding a lazy-seq here; see if it's faster
     (let [ba (into-array Byte/TYPE
                          (map #(if (> % 127)
                                  (bit-xor % -256)
                                  %)
                               bs))]
       (list (count ba)
             (ByteBuffer/wrap ba)))))

(defn concat-bytes
  "Takes any number of byte buffers and concatenates them into a
  single byte buffer"
  ;; XXX try a recursive approach to concatenate more buffers?
  ([& byte-buffers]
     (if (= 1 (count byte-buffers))
       byte-buffers
       (let [buffer-length-sums (reductions +
                                            (map first byte-buffers))
             num-short-buffers
               (count (take-while
                         (partial > +byte-buffer-concat-threshold+)
                         buffer-length-sums))
             short-buffers (take num-short-buffers byte-buffers)
             long-buffers (drop num-short-buffers byte-buffers)
             new-len (last buffer-length-sums)
             new-buffers (cond (= 0 num-short-buffers)
                               nil
                               (= 1 num-short-buffers)
                               (rest (first short-buffers))
                               :else
                               (rest (byte-buffer
                                      (mapcat #(.array ^ByteBuffer %)
                                              (mapcat rest short-buffers)))))
             new-buffers (concat new-buffers
                                 (mapcat rest long-buffers))]
         (cons new-len new-buffers)))))

(defn byte-buffer-size
  "Returns the length in bytes of the given byte buffer"
  ([b]
     (first b)))

(defn write-file
  "Writes the given byte buffer to the given filename"
  ([^String filename byte-buffer]
     (with-open [out (FileOutputStream. filename)]
       (doseq [^ByteBuffer bb (rest byte-buffer)]
         (.write ^FileOutputStream out (.array bb))))))

(defn compress
  "Takes a byte buffer and a fn that calls a DeflaterOutputStream
  constructor with one OutputStream argument, and returns a byte
  buffer with compressed data"
  ([buf deflater-fn]
     (let [baos (ByteArrayOutputStream.)
           ^DeflaterOutputStream out (deflater-fn baos)]
       (doseq [^ByteBuffer bb (rest buf)]
         (.write out (.array bb)))
       (.finish out)
       (.close out)
       (byte-buffer (seq (.toByteArray baos))))))

(defn zlib-compress
  "Returns a byte buffer containing a zlib compressed version of the
  data in the given byte buffer"
  ([buf]
     (compress buf #(DeflaterOutputStream. %))))

(defn gzip-compress
  "Returns a byte buffer containing a gzip compressed version of the
  data in the given byte buffer"
  ([buf]
     (compress buf #(GZIPOutputStream. %))))

(defn utf8-bytes
  ([s]
     (.getBytes ^String s "UTF-8")))

(defn str-to-byte-buffer
  "Encodes the given string using UTF-8"
  ([s]
     (byte-buffer (utf8-bytes s))))

(defn tag-end
  "Returns a binary-formatted TAG_End"
  ([]
     (byte-buffer [0])))

(defn tag-string
  "Returns a binary-formatted TAG_String"
  ;; The long form doesn't appear to be necessary.
  ([tag-name s]
     (concat-bytes (byte-buffer [8])
                   (tag-string tag-name)
                   (tag-string s)))
  ([s]
     (let [bs (utf8-bytes s)
           n  (count bs)
           n-high (int (/ n 256))
           n-low  (int (mod n 256))]
       (byte-buffer (concat [n-high n-low] ; tag-short
                            bs)))))

(defn tag-compound
  "Returns a binary-formatted TAG_Compound for any number of elements"
  ([payloads]
     (apply concat-bytes
            (concat payloads (list (tag-end)))))
  ([tag-name payloads]
     (concat-bytes (byte-buffer [10])
                   (tag-string tag-name)
                   (tag-compound payloads))))

(defn tag-byte
  "Returns a binary-formatted TAG_Byte"
  ([tag-name n]
     (concat-bytes (byte-buffer [1])
                   (tag-string tag-name)
                   (tag-byte n)))
  ([n]
     (byte-buffer [n])))

(defn tag-short
  "Returns a binary-formatted TAG_Short"
  ;; I should really copy fixnum-to-bytes from the other project.  Maybe
  ;; that should go in its own library.
  ([tag-name n]
     (concat-bytes (byte-buffer [2])
                   (tag-string tag-name)
                   (tag-short n)))
  ([n]
     (byte-buffer [(bit-and 255 (bit-shift-right n 8))
                   (bit-and 255 n)])))

(defn int-to-bytes
  "Takes an int and returns the big-endian representation of that int
  as a vector of four bytes"
  ([n]
     [(bit-shift-right n 24)
      (bit-and 255 (bit-shift-right n 16))
      (bit-and 255 (bit-shift-right n 8))
      (bit-and 255 n)]))

(defn tag-int
  "Returns a binary-formatted TAG_Int"
  ;; I should really copy fixnum-to-bytes-4 from the other project.  Maybe
  ;; that should go in its own library.
  ([tag-name n]
     (concat-bytes (byte-buffer [3])
                   (tag-string tag-name)
                   (tag-int n)))
  ([n]
     (byte-buffer (int-to-bytes n))))

(defn tag-long
  "Returns a binary-formatted TAG_Long"
  ([tag-name n]
     (concat-bytes (byte-buffer [4])
                   (tag-string tag-name)
                   (tag-long n)))
  ([n]
     (concat-bytes (tag-int (bit-shift-right n 32))
                   (tag-int (bit-and (dec (bit-shift-left 1 32))
                                     n)))))

(defn tag-float
  "Returns a binary-formatted TAG_Float"
  ([tag-name f]
     (concat-bytes (byte-buffer [5])
                   (tag-string tag-name)
                   (tag-float f)))
  ([f]
     (tag-int (Float/floatToIntBits f))))

(defn tag-double
  "Returns a binary-formatted TAG_Double"
  ([tag-name d]
     (concat-bytes (byte-buffer [6])
                   (tag-string tag-name)
                   (tag-double d)))
  ([d]
     (tag-long (Double/doubleToLongBits d))))

(defn tag-byte-array
  "Returns a binary-formatted TAG_Byte_Array"
  ;; This does not appear as part of the payload of any other type, so
  ;; no short form is needed.
  ([tag-name bs]
     (concat-bytes (byte-buffer [7])
                   (tag-string tag-name)
                   (tag-int (count bs))
                   (byte-buffer bs))))

(defn tag-list
  "Returns a binary-formatted TAG_List for the given type and seq of
  tagged data"
  ;; This does not appear as part of the payload of any other type, so
  ;; no short form is needed.
  ([tag-name tag-type s]
     (apply concat-bytes
            (byte-buffer [9])
            (tag-string tag-name)
            (tag-byte tag-type)
            (tag-int (count s))
            s)))

(defn tag-int-array
  "Returns a binary-formatted TAG_Int_Array"
  ;; This does not appear as part of the payload of any other type, so
  ;; no short form is needed.
  ([tag-name bs]
     (concat-bytes (byte-buffer [11])
                   (tag-string tag-name)
                   (tag-int (count bs))
                   (byte-buffer (mapcat int-to-bytes bs)))))

(defn merge-nybbles
  ([ [a b] ]
     (doseq [n [a b]]
       (if (or (> n 15)
               (< n 0)
               (not= n (int n)))
         (throw (RuntimeException. (str "Invalid value for nybble: " n)))))
     (bit-or (bit-shift-left b 4)
             a)))

(defn nybbles-to-bytes
  ([nybseq]
     (map merge-nybbles
          (partition 2 2 nybseq))))

(defn block-datum
  ([ze]
     (if (map? ze)
       (or (:datum ze)
           (case (:type ze)
                 (:ladder :wall-sign :furnace :dispenser :chest)
                   ( {:south 0x2 :north 0x3 :east 0x4 :west 0x5 nil 0}
                     (:face ze))
                 (:wood-stairs :cobble-stairs :stone-brick-stairs
                  :brick-stairs :nether-brick-stairs)
                   (bit-or ( {:east 0x0 :west 0x1 :south 0x2 :north 0x3}
                             (:face ze))
                           (if (:upside-down ze)
                             4 0))
                 (:redstone-repeater-on :redstone-repeater-off)
                   (bit-or ( {:north 0x0 :east 0x1 :south 0x2 :west 0x3}
                             (:face ze))
                           (* 4 (:delay ze)))
                 :sign-post
                   ( {:north            0x0
                      :north-northeast  0x1
                      :northeast        0x2
                      :east-northeast   0x3
                      :east             0x4
                      :east-southeast   0x5
                      :southeast        0x6
                      :south-southeast  0x7
                      :south            0x8
                      :south-southwest  0x9
                      :southwest        0xA
                      :west-southwest   0xB
                      :west             0xC
                      :west-northwest   0xD
                      :northwest        0xE
                      :north-northwest  0xF}
                     (:face ze))
                 :vines
                   (reduce bit-or
                           (map {:south 0x1 :west 0x2 :north 0x4 :east 0x8}
                                (:faces ze)))
                 0))
       0)))

(defn xzy-seq
  ([zone]
     (for [x (range (zone-x-size zone))
           z (range (zone-z-size zone))
           y (range (zone-y-size zone))]
       (zone-lookup zone x y z))))

(defn yzx-seq
  ([zone yrange]
     (for [y yrange
           z (range (zone-z-size zone))
           x (range (zone-x-size zone))]
       (zone-lookup zone x y z))))

(defn block-ids
  "Returns a seq of bytes with block IDs for the given seq of blocks"
  ([zone-seq]
     (map block-id zone-seq)))

(defn block-data
  "Returns a seq of bytes with extra block data for the given seq of
  blocks"
  ([zone-seq]
     (nybbles-to-bytes
        (map block-datum zone-seq))))

(defn sky-light
  "Returns a seq of bytes with sky light data for the given seq of
  precalculated skylight levels"
  ([skylight-zone-seq]
     (nybbles-to-bytes skylight-zone-seq)))

(defn block-light-bytes
  "Returns a seq of bytes with block light data for the given seq of
  precalculated light levels"
  ([light-zone-seq]
     (nybbles-to-bytes light-zone-seq)))

(defn flat-map-vals
  "Returns a seq of bytes or ints of per-vertical-column data from the
  given one-block-high zone"
  ([zone]
     (for [z (range (zone-z-size zone))
           x (range (zone-x-size zone))]
       (zone-lookup zone x 0 z))))

(defn height-map-vals
  "Returns a seq of bytes or ints of heightmap data for the given
  heightmap zone"
  ([height-zone]
     (flat-map-vals height-zone)))

(defn biome-bytes
  "Returns a seq of bytes of biome data for the given biome zone"
  ([biome-zone]
     (flat-map-vals biome-zone)))

(defn enchantment
  "Takes a seq of enchantments in {:id n, :lvl n} form and returns a
  TAG_Compound called \"tag\", which can be used as a property on an
  inventory item"
  ([enchs]
     (tag-compound "tag"
        [ (tag-list "ench" 10
                    (map #(tag-compound [ (tag-short "id" (:id %))
                                          (tag-short "lvl" (:lvl %))])
                         enchs))])))

(defn inventory-list
  "Takes a seq of items defined as {:id block-or-item-id, :damage
  n, :count n, :slot n, (and optionally :ench (enchantment ...))} and
  returns a seq of UTAG_Compounds, suitable for use as the :items slot
  in a Furnace, Chest, Trap, Cauldron, or Minecart (with chest)"
  ([items]
     (map #(tag-compound
             (list*
               (tag-short "id"     (:id     %))
               (tag-short "Damage" (or (:damage %) 0))
               (tag-byte  "Count"  (or (:count  %) 1))
               (tag-byte  "Slot"   (:slot   %))
               (if-let [ench (:ench %)]
                 [ (enchantment ench) ])))
          items)))

(defn tile-entity
  ([ [ze x y z] ]
     (when (map? ze)
       (let [t (:type ze)
             fields
               (case t
                     (:monster-spawner :mob-spawner)
                       ["MobSpawner"
                        (tag-string "EntityId" (:mob ze))
                        (tag-short "Delay" (or (:delay ze)
                                               200))]
                     :chest
                       ["Chest"
                        (tag-list "Items" 10
                                  (:items ze))]
                     :dispenser
                       ["Trap"
                        (tag-list "Items" 10
                                  (:items ze))]
                     (:wall-sign :sign-post)
                       ["Sign"
                        (tag-string "Text1" (-> ze :text (get 0) (or "")))
                        (tag-string "Text2" (-> ze :text (get 1) (or "")))
                        (tag-string "Text3" (-> ze :text (get 2) (or "")))
                        (tag-string "Text4" (-> ze :text (get 3) (or "")))]
                     ;; default:
                     nil)]
         (when-let [ [id & additional-fields] fields]
           [ (tag-compound
                (list* (tag-string "id" id)
                       (tag-int "x" x)
                       (tag-int "y" y)
                       (tag-int "z" z)
                       additional-fields))])))))

(defn tile-entities
  "Returns a seq of TAG_Compounds defining the tile entities within
  the given zone"
  ([zone x0 z0]
     (mapcat tile-entity
             (for [x (range (zone-x-size zone))
                   z (range (zone-z-size zone))
                   y (range (zone-y-size zone))]
               [ (zone-lookup zone x y z) (+ x0 x) y (+ z0 z) ]))))

(def mob-entity-name
     {:blaze "Blaze"
      :cave-spider "CaveSpider"
      :chicken "Chicken"
      :cow "Cow"
      :creeper "Creeper"
      :ender-dragon "EnderDragon"
      :enderman "Enderman"
      :ghast "Ghast"
      :lava-slime "LavaSlime"
      :mushroom-cow "MushroomCow"
      :ozelot "Ozelot"
      :pig "Pig"
      :pig-zombie "PigZombie"
      :sheep "Sheep"
      :silverfish "Silverfish"
      :skeleton "Skeleton"
      :slime "Slime"
      :snow-man "SnowMan"
      :spider "Spider"
      :squid "Squid"
      :villager "Villager"
      :wolf "Wolf"
      :zombie "Zombie"})

(defn mob-extra-fields
  ([type ent]
     (case type
           :pig
           [(tag-byte "Saddle"  (if (:saddle  ent) 1 0))]
           :sheep
           [(tag-byte "Sheared" (if (:sheared ent) 1 0))
            (tag-byte "Color" (or (:color-num ent)
                                  (if (:color ent)
                                    (+color+ (:color ent))
                                    0)))]
           :creeper
           [(tag-byte "powered" (if (:powered ent) 1 0))]
           :slime
           [(tag-int "Size" (or (:size ent) 1))]
           :wolf
           [(tag-string "Owner" (or (:owner ent) ""))
            (tag-byte "Sitting" (if (:sitting ent) 1 0))
            (tag-byte "Angry"   (if (:angry   ent) 1 0))]
           :pig-zombie
           [(tag-short "Anger" (or (:anger ent) 0))]
           :enderman
           (if-let [ce (:carried ent)]
             [(tag-short "carried" (block-id ce))
              (tag-short "carriedData" (:datum ce))])
           ;; default:
           nil)))

(defn full-health
  ([mob-entity]
     (case (:type mob-entity)
           ;; todo: distinguish between tamed and wild animals
           (:chicken :cow :ozelot :pig :sheep :snow-man :squid
            :villager :wolf)
           10
           ;; default - all hostile mobs:
           20)))

(defn entity
  "Returns nil, or a collection containing zero or more (any number)
  of entities contained within the given block"
  ([ [ze x y z] ]
     (when-let [ents (:ents ze)]
       (for [ent ents]
         (let [t (:type ent)
               fields
                 (case t
                       (:blaze :cave-spider :chicken :cow :creeper
                        :ender-dragon :enderman :ghast :lava-slime
                        :mushroom-cow :ozelot :pig :pig-zombie :sheep
                        :silverfish :skeleton :slime :snow-man :spider
                        :squid :villager :wolf :zombie)
                       (list* (mob-entity-name t)
                              (tag-short "AttackTime"
                                         (or (:attack-time ent) 0))
                              (tag-short "DeathTime"
                                         (or (:death-time ent) 0))
                              (tag-short "Health"
                                         (or (:health ent)
                                             (full-health ent)))
                              (tag-short "HurtTime"
                                         (or (:hurt-time ent) 0))
                              (mob-extra-fields t ent)))
               [id & additional-fields] fields]
           (tag-compound
            (list* (tag-string "id" id)
                   (tag-list "Pos" 6
                      (map tag-double
                           [(+ x (or (:xd ent) 0.0))
                            (+ y (or (:yd ent) 0.0))
                            (+ z (or (:zd ent) 0.0))]))
                   (tag-list "Motion" 6
                      (map tag-double
                           [(or (:vx ent) 0.0)
                            (or (:vy ent) 0.0)
                            (or (:vz ent) 0.0)]))
                   (tag-list "Rotation" 5
                      (map tag-float
                           [(or (:yaw ent) 0.0)
                            (or (:pitch ent) 0.0)]))
                   (tag-float "FallDistance"
                              (or (:fall-distance ent) 0.0))
                   (tag-short "Fire" (or (:fire ent) 0))
                   (tag-short "Air"  (or (:air  ent) 0))
                   (tag-byte "OnGround" (if (:on-ground ent) 1 0))
                   additional-fields)))))))

(defn entities
  "Returns a seq of TAG_Compounds defining the entities within the
  given zone"
  ([zone x0 z0]
     (mapcat entity
             (for [x (range (zone-x-size zone))
                   z (range (zone-z-size zone))
                   y (range (zone-y-size zone))]
               [ (zone-lookup zone x y z) (+ x0 x) y (+ z0 z) ]))))

(defn extract-chunk
  "Returns a binary chunk {:x <chunk-x> :z <chunk-z> :compressed-data
  <byte-buffer>} at the given coordinates within the given mcmap"
  ([mcmap region-x region-z chunk-x chunk-z]
     (let [x0 (+ (* +region-side+ region-x)
                 (* +chunk-side+ chunk-x))
           z0 (+ (* +region-side+ region-z)
                 (* +chunk-side+ chunk-z))
           zone-x0 (- x0 (:x0 mcmap))
           zone-z0 (- z0 (:z0 mcmap))
           [blocks skylight-subzone light-subzone]
             (map #(sub-zone (% mcmap)
                             zone-x0 (+ zone-x0 +chunk-side+)
                             0 +old-chunk-height+
                             zone-z0 (+ zone-z0 +chunk-side+))
                  [:block-zone :skylight-zone :light-zone])
           height-subzone (sub-zone (:height-zone mcmap)
                                    zone-x0 (+ zone-x0 +chunk-side+)
                                    0 1
                                    zone-z0 (+ zone-z0 +chunk-side+))
           data (tag-compound ""
                  [ (tag-compound "Level"
                      [ (tag-byte-array "Blocks"
                                        (block-ids (xzy-seq blocks)))
                        (tag-byte-array "Data"
                                        (block-data (xzy-seq blocks)))
                        (tag-byte-array "SkyLight"
                                        (sky-light (xzy-seq
                                                    skylight-subzone)))
                        (tag-byte-array "BlockLight"
                                        (block-light-bytes
                                         (xzy-seq light-subzone)))
                        (tag-byte-array "HeightMap"
                                        (height-map-vals height-subzone))
                        (tag-list "Entities" 10
                                  (entities blocks x0 z0))
                        (tag-list "TileEntities" 10
                                  (tile-entities blocks x0 z0))
                        (tag-list "TileTicks" 10 [])
                        (tag-long "LastUpdate" 1656407)
                        (tag-int "xPos" (/ x0 +chunk-side+))
                        (tag-int "zPos" (/ z0 +chunk-side+))
                        (tag-byte "TerrainPopulated" 1) ]) ])]
       {:x chunk-x
        :z chunk-z
        :compressed-data (zlib-compress data)})))

(defn anvil-sections
  "Returns a seq of unnamed TAG_Compounds with sections of the chunk
  described by the given sub-zone"
  ([block-zone skylight-zone light-zone]
     (for [y-section (range (/ (zone-y-size block-zone) 16))]
       (let [yrange (range (* 16 y-section)
                           (* 16 (inc y-section)))]
         (tag-compound
            [ (tag-byte-array "Data"
                              (block-data (yzx-seq block-zone yrange)))
              (tag-byte-array "SkyLight"
                              (sky-light (yzx-seq skylight-zone yrange)))
              (tag-byte-array "BlockLight"
                              (block-light-bytes
                               (yzx-seq light-zone yrange)))
              (tag-byte "Y" y-section)
              (tag-byte-array "Blocks"
                              (block-ids (yzx-seq block-zone yrange)))])))))

(defn extract-anvil-chunk
  "Returns a binary chunk {:x <chunk-x> :z <chunk-z> :compressed-data
  <byte-buffer>} at the given coordinates within the given mcmap"
  ([mcmap region-x region-z chunk-x chunk-z]
     (let [x0 (+ (* +region-side+ region-x)
                 (* +chunk-side+ chunk-x))
           z0 (+ (* +region-side+ region-z)
                 (* +chunk-side+ chunk-z))
           zone-x0 (- x0 (:x0 mcmap))
           zone-z0 (- z0 (:z0 mcmap))
           map-height (zone-y-size (:block-zone mcmap))
           [blocks skylight-subzone light-subzone]
             (map #(sub-zone (% mcmap)
                             zone-x0 (+ zone-x0 +chunk-side+)
                             0  map-height
                             zone-z0 (+ zone-z0 +chunk-side+))
                  [:block-zone :skylight-zone :light-zone])
           [height-subzone biome-subzone]
             (map #(sub-zone (% mcmap)
                             zone-x0 (+ zone-x0 +chunk-side+)
                             0  1
                             zone-z0 (+ zone-z0 +chunk-side+))
                  [:height-zone :biome-zone])
           data (tag-compound ""
                  [ (tag-compound "Level"
                      [ (tag-list "Entities" 10
                                  (entities blocks x0 z0))
                        (tag-byte-array "Biomes"
                                        (biome-bytes biome-subzone))
                        (tag-long "LastUpdate" 1656407)
                        (tag-int "xPos" (/ x0 +chunk-side+))
                        (tag-int "zPos" (/ z0 +chunk-side+))
                        (tag-list "TileEntities" 10
                                  (tile-entities blocks x0 z0))
                        (tag-byte "TerrainPopulated" 1)
                        (tag-int-array "HeightMap"
                                       (height-map-vals height-subzone))
                        (tag-list "Sections" 10
                                  (anvil-sections blocks skylight-subzone
                                                  light-subzone))
                        ;(tag-list "TileTicks" 10 [])
                        ]) ])]
       (write-file (str "/tmp/anv-chunk-data-" x0 "-" z0)
                   data)
       {:x chunk-x
        :z chunk-z
        :compressed-data (zlib-compress data)})))

(defn mmcmap-to-chunks
  "Returns a seq of chunks for the given multi-mcmap and region
  coordinates, where each chunk is {:x <chunk-x> :z <chunk-z> :data
  <byte-buffer>"
  ([mmcmap x z]
     (pfor [chunk-x (range 32) chunk-z (range 32)
            mcmap (:mcmaps mmcmap)
            :when (chunk-in-mcmap? mcmap x z chunk-x chunk-z)]
       (extract-chunk mcmap x z chunk-x chunk-z))))

(defn mmcmap-to-anvil-chunks
  "Returns a seq of chunks for the given multi-mcmap and region
  coordinates, where each chunk is {:x <chunk-x> :z <chunk-z> :data
  <byte-buffer>"
  ([mmcmap x z]
     (pfor [chunk-x (range 32) chunk-z (range 32)
            mcmap (:mcmaps mmcmap)
            :when (chunk-in-mcmap? mcmap x z chunk-x chunk-z)]
       (extract-anvil-chunk mcmap x z chunk-x chunk-z))))

(defn locations
  "Returns a seq of chunk file locations and sector counts (in 4KiB
  sectors) that will fit the given chunks; an optional second argument
  gives an offset (in sectors) at which to begin placing chunks"
  ([chunks]
     (locations chunks 2))              ; Leave room for timestamps
  ([chunks offset]
     (when (seq chunks)
       (let [chunk (first chunks)
             chunk-compressed-size (byte-buffer-size
                                    (:compressed-data chunk))
             chunk-size-in-sectors (quot (+ chunk-compressed-size 4101)
                                         4096)
             room-to-leave (+ chunk-size-in-sectors 6)]
         (when (> chunk-size-in-sectors 255)
           (throw (RuntimeException. (str "Chunk too big ("
                                          chunk-size-in-sectors
                                          ") at x="
                                          (:x chunk)
                                          ", y="
                                          (:y chunk)))))
         (when (>= offset (bit-shift-left 1 24))
           (throw (RuntimeException. (str "Offset " offset " too big"))))
         (lazy-seq (cons {:x (:x chunk),
                          :z (:z chunk),
                          :offset offset,
                          :count chunk-size-in-sectors}
                         (locations (rest chunks)
                                    (+ offset room-to-leave))))))))

(defn locations-to-bytes
  "Returns a 4096-byte block of mcr chunk file locations for the given
  seq of locations"
  ([locs]
     (let [pos-to-loc (apply hash-map
                             (mapcat (fn [loc]
                                       [ [ (:x loc)
                                           (:z loc) ]
                                         loc ])
                                     locs))
           locs-in-order (for [z (range 32)
                               x (range 32)]
                           (pos-to-loc [x z]))
           loc-bytes (mapcat #(if %
                                [(bit-shift-right (:offset %) 16)
                                 (bit-and 255
                                          (bit-shift-right (:offset %)
                                                           8))
                                 (bit-and 255 (:offset %))
                                 (:count %)]
                                [0 0 0 0])
                             locs-in-order)]
       (byte-buffer loc-bytes))))

(defn timestamps
  "Returns a 4096-byte block of timestamps sized appropriately for the
  number of chunks in the given seq of chunks"
  ([chunks]
     ;; encoding scheme is not documented in the wiki; why do we even
     ;; need chunk timestamps?  0 for everything should be safe.

     ;; I have a theory that this might be why my stone sometimes gets
     ;; replaced with dirt, gravel, and ores, though -- part of some
     ;; old landscape upgrade process.
     (byte-buffer (repeat 4096 0))))

(defn pad-chunk
  "Returns byte buffers containing pads and data for the given chunk"
  ([chunk loc prev-loc]
     (let [offset (:offset loc)
           count (:count loc)
           data (:compressed-data chunk)
           len (byte-buffer-size data)
           compression-type 2
           pad-needed (* (- offset
                            (+ (:offset prev-loc)
                               (:count prev-loc)))
                         4096)
           post-pad-needed (mod (- (+ len 5))
                                4096)]
       (when (neg? pad-needed)
         (throw (RuntimeException. (str "place-chunk can only handle"
                                        " appending chunks; loc="
                                        loc ", prev-loc=" prev-loc
                                        ", pad-needed=" pad-needed))))
       [ (byte-buffer (repeat pad-needed 0))
         (tag-int (inc len))
         (tag-byte compression-type)
         data
         (byte-buffer (repeat post-pad-needed 0)) ])))

(defn place-chunks
  "Given seqs of chunks and locations, returns a byte buffer
  containing the chunks for an mcr file"
  ([chunks locs]
     (msg 1 "Placing chunks ...")
     (let [shifted-locs (cons {:offset 0, :count 2} locs)
           chunks-and-pads (apply concat (pmap pad-chunk chunks locs
                                               shifted-locs))]
       (apply concat-bytes chunks-and-pads))))

(defn block-opacity
  ([ze]
     (if (map? ze)
       (or (:opacity ze)
           (block-opacity (:type ze)))
       (or (+opacity+ ze)
           0))))

(defn opaque?
  "Returns true if the given block is at all opaque to light (which
  includes water and lava)"
  ([ze]
     (pos? (block-opacity ze))))

(defn map-height
  "Given a block zone and x and z coordinates, returns the y
  coordinate of the highest block at that x and z that receives the
  full light of the sun"
  ([zone x z]
     (inc (or (first (filter #(opaque? (zone-lookup zone x % z))
                             (range (dec (zone-y-size zone))
                                    -1 -1)))
              -1))))

(def +biome-codes+
     {:ocean 0,
      :plains 1,
      :desert 2,
      :extreme-hills 3,
      :forest 4,
      :taiga 5,
      :swampland 6,
      :river 7,
      :hell 8,
      :sky 9,
      :frozen-ocean 10,
      :frozen-river 11,
      :these-lions-that-breathe 11,
      :ice-plains 12,
      :ice-mountains 13,
      :mushroom-island 14,
      :mushroom-island-shore 15,
      :beach 16,
      :desert-hills 17,
      :forest-hills 18,
      :taiga-hills 19,
      :extreme-hills-edge 20,
      :jungle 21,
      :jungle-hills 22})

(defn map-biome
  "Given a block zone and x and z coordinates, returns the biome value
  of the first block encountered that has a :biome attribute, or else
  the default biome"
  ([zone x z]
     (let [height (zone-y-size zone)
           biome-type (or (first
                           (filter identity
                                   (map #(:biome (zone-lookup zone x % z))
                                        (range (dec height)
                                               -1 -1))))
                          +default-biome+)]
       (if (keyword? biome-type)
         (+biome-codes+ biome-type)
         biome-type))))

(defn block-light
  "Given a block zone element, returns the base block light
  level (before spreading light from neighboring blocks) for that
  block"
  ([ze]
     (if (map? ze)
       (or (if-let [l (:force-nospread-light ze)]
             (- -256 l))
           (if-let [l (:force-light ze)]
             (- -1 l))
           (if-let [l (:light ze)]
             l)
           (block-light (:type ze)))
       (or (+light-levels+ ze)
           0))))

(defn block-skylight
  "Given a block zone element, its coordinates, and a height map,
  returns the base sky light level (before spreading light from
  neighboring blocks) for that location"
  ([ze x y z height-zone]
     (or (if (map? ze)
           (or (if-let [l (:force-nospread-skylight ze)]
                 (- -256 l))
               (if-let [l (:force-skylight ze)]
                 (- -1 l))
               (if-let [l (:skylight ze)]
                 l)))
         (if (>= y (zone-lookup height-zone x 0 z))
           15
           0))))

(defn recalc-light
  "Recalculates light for one block based on its opacity and the light
  levels of all neighboring blocks"
  ([cur-light total-opacity & neighbor-lights]
     (if (neg? cur-light)
       cur-light
       (apply max cur-light
              (map #(- (if (neg? %)
                         (- -1 %)
                         %)
                       total-opacity)
                   (filter #(and % (> % -256))
                           neighbor-lights))))))

(defn recalc-light-1
  "Recalculates light for one block based on its opacity and the light
  levels of ONE neighboring block"
  ([cur-light total-opacity neighbor-light]
     (cond (neg? cur-light)
             cur-light
           (and neighbor-light (> neighbor-light -256))
             (max (- (if (neg? neighbor-light)
                       (- -1 neighbor-light)
                       neighbor-light)
                     total-opacity)
                  cur-light)
           :else
             cur-light)))

(defn iterate-light-zone
  ([light-zone opacity-zone]
     (let [gen-fn #(fn [x y z prev]
                     (recalc-light-1
                         (zone-lookup % x y z)
                         (inc (zone-lookup opacity-zone x y z))
                         prev))]
       (reduce #(%2 (zone-x-size %1)
                    (zone-y-size %1)
                    (zone-z-size %1)
                    (gen-fn %1))
               light-zone
               [falling-recursive-gen-mcmap-zone
                westward-recursive-gen-mcmap-zone
                southward-recursive-gen-mcmap-zone
                rising-recursive-gen-mcmap-zone
                northward-recursive-gen-mcmap-zone
                eastward-recursive-gen-mcmap-zone]))))

(defn translate-light-zone
  "Takes a light zone with values from -511 to 256, and returns a zone
  with Minecraft light values from 0 to 15"
  ([light-zone]
     (gen-mcmap-zone (zone-x-size light-zone)
                     (zone-y-size light-zone)
                     (zone-z-size light-zone)
           (fn [x y z]
             (let [l (zone-lookup light-zone x y z)
                   l (cond (<= l -256)
                             (- -256 l)
                           (neg? l)
                             (- -1 l)
                           :else
                             l)]
               (if (> l 15)
                 15
                 l))))))

(defn spread-light
  ([light-zone opacity-zone]
     (loop [light-zone light-zone]
       (let [new-light-zone (iterate-light-zone light-zone
                                                opacity-zone)]
         (if (= new-light-zone light-zone)
           (translate-light-zone light-zone)
           (recur new-light-zone))))))

(defn compute-block-light
  ([block-zone opacity-zone]
     (let [light-zone (gen-mcmap-zone (zone-x-size block-zone)
                                      (inc (zone-y-size block-zone))
                                      (zone-z-size block-zone)
                            (fn [x y z]
                              (block-light
                                  (maybe-zone-lookup block-zone x y z))))
           tall-light-zone (spread-light light-zone opacity-zone)]
       (gen-mcmap-zone (zone-x-size block-zone)
                       (zone-y-size block-zone)
                       (zone-z-size block-zone)
         (fn [x y z]
           (zone-lookup tall-light-zone x y z))))))

(defn compute-skylight
  ([block-zone opacity-zone height-zone]
     (let [skylight-zone (gen-mcmap-zone (zone-x-size block-zone)
                                         (inc (zone-y-size block-zone))
                                         (zone-z-size block-zone)
                               (fn [x y z]
                                 (block-skylight
                                     (maybe-zone-lookup block-zone x y z)
                                     x y z height-zone)))
           tall-skylight-zone (spread-light skylight-zone opacity-zone)]
       (gen-mcmap-zone (zone-x-size block-zone)
                       (zone-y-size block-zone)
                       (zone-z-size block-zone)
         (fn [x y z]
           (zone-lookup tall-skylight-zone x y z))))))

(defn gen-mcmap
  "Given x, y, and z sizes, x and z offsets, and a function of x y and
  z that returns a block, returns an mcmap complete with computed
  light levels"
  ([x-size z-size f]
     (gen-mcmap x-size +old-chunk-height+ z-size f))
  ([x-size y-size z-size f]
     (gen-mcmap x-size y-size z-size 0 0 f))
  ([x-size y-size z-size x0 z0 f]
     (when (or (pos? (mod x-size 16))
               (pos? (mod y-size 16))
               (pos? (mod z-size 16)))
       (die "Illegal map dimensions: " x-size "x" y-size "x" z-size
            "; must be multiples of 16"))
     (when (or (pos? (mod x0 16))
               (pos? (mod z0 16)))
       (die "Illegal map offset: " x0 "," z0 "; must be multiples of 16"))
     (let [_ (msg 1 "Placing blocks ...")
           block-zone (gen-mcmap-zone x-size y-size z-size f)
           _ (msg 1 "Mapping opaque and transparent blocks ...")
           opacity-zone (gen-mcmap-zone x-size (inc y-size) z-size
                          (fn [x y z]
                            (block-opacity
                             (maybe-zone-lookup block-zone x y z))))
           _ (msg 1 "Computing height and biome maps ...")
           height-zone (p-gen-mcmap-zone x-size 1 z-size
                         (fn [x _ z]
                           (map-height block-zone x z)))
           biome-zone (p-gen-mcmap-zone x-size 1 z-size
                         (fn [x _ z]
                           (map-biome block-zone x z)))
           light-zone (promise)
           skylight-zone (promise)
           _ (msg 1 "Starting block-source light calcs ...")
           _ (send (agent nil)
                   (fn [_]
                     (deliver light-zone
                              (let [v (compute-block-light block-zone
                                                           opacity-zone)]
                                (msg 1 "Done with block-source light calcs")
                                v))))
           _ (msg 1 "Starting sky light calcs ...")
           _ (send (agent nil)
                   (fn [_]
                     (deliver skylight-zone
                              (let [v (compute-skylight block-zone
                                                        opacity-zone
                                                        height-zone)]
                                (msg 1 "Done with sky light calcs")
                                v))))]
       {:block-zone block-zone
        :light-zone @light-zone
        :skylight-zone @skylight-zone
        :height-zone height-zone
        :biome-zone biome-zone
        :x0 x0
        :z0 z0})))

(defn dup-mcmap
  "Takes an mcmap and a new x0 and z0, and returns an mcmap translated
  to that position"
  ([mcmap x0 z0]
     (when (or (pos? (mod x0 16))
               (pos? (mod z0 16)))
       (die "Illegal map offset: " x0 "," z0 "; must be multiples of 16"))
     (assoc mcmap
       :x0 x0
       :z0 z0)))

(defn mcmaps-to-mmcmap
  "Takes a seq of mcmaps and returns an mmcmap"
  ([mcmaps]
     {:mcmaps mcmaps}))

(defn gen-mmcmap
  "Given x, y, and z sizes, x and z offsets, and a function of x y and
  z that returns a block, returns a multi-mcmap complete with computed
  light levels"
  ([& args]
     (mcmaps-to-mmcmap [(apply gen-mcmap args)])))

(defn mmcmap-to-mcr-binary
  "Takes a multi-mcmap and two region coordinates, and returns a
  region extracted from that zone, in Minecraft beta .mcr format"
  ([mmcmap x z]
     (msg 1 "Extracting chunks for region " x "." z " ...")
     (let [chunks (mmcmap-to-chunks mmcmap x z)
           locs (locations chunks)]
       (concat-bytes (locations-to-bytes locs)
                     (timestamps chunks)
                     (place-chunks chunks locs)))))

(defn mmcmap-to-mca-binary
  "Takes a multi-mcmap and two region coordinates, and returns a
  region extracted from that zone, in Minecraft Anvil .mca format"
  ([mmcmap x z]
     (msg 1 "Extracting chunks for Anvil region " x "." z " ...")
     (let [chunks (mmcmap-to-anvil-chunks mmcmap x z)
           locs (locations chunks)]
       (concat-bytes (locations-to-bytes locs)
                     (timestamps chunks)
                     (place-chunks chunks locs)))))

(defn mcmap-to-mca-binary
  "Takes an mcmap and two region coordinates, and returns a region
  extracted from that zone, in Minecraft Anvil .mca format"
  ([mcmap x z]
     (mmcmap-to-mca-binary (mcmaps-to-mmcmap [mcmap])
                           x z)))

(defn mcmap-to-mcr-binary
  "Takes an mcmap and two region coordinates, and returns a region
  extracted from that zone, in Minecraft Anvil .mca format"
  ([mcmap x z]
     (mmcmap-to-mcr-binary (mcmaps-to-mmcmap [mcmap])
                           x z)))

(defn map-exercise-1
  "Returns the .mca binary data for a single region made up of
  sandwiches of glowstone and air with columns of stone, or writes it
  to a file if given a filename; generates two chunks by two chunks if
  not given two dimension arguments"
  ([x-chunks z-chunks]
     (let [generator (fn [x y z]
                       (if (and (zero? (mod z 4))
                                (zero? (mod x 4)))
                         (mc-block :stone)
                         (if (zero? (mod y 4))
                           (mc-block :glowstone)
                           (mc-block :air))))
           mcmap (gen-mcmap (* x-chunks +chunk-side+)
                            (* z-chunks +chunk-side+)
                            generator)]
       (mcmap-to-mca-binary mcmap 0 0)))
  ([filename]
     (write-file filename (map-exercise-1)))
  ([]
     (map-exercise-1 2 2))
  ([filename x-chunks z-chunks]
     (write-file filename (map-exercise-1 x-chunks z-chunks))))


(defn map-exercise-2
  "Like map-exercise-1, but randomly scatters mob spawners"
  ([x-chunks z-chunks]
     (let [generator (fn [x y z]
                       (cond (> 0.001 (rand))
                               (mc-block :mob-spawner
                                         :mob "Chicken" :delay 0)
                             (> 0.001 (rand))
                               (mc-block :mob-spawner
                                         :mob "Zombie" :delay 200)
                             (and (zero? (mod z 4))
                                  (zero? (mod x 4)))
                               (mc-block :stone)
                             (zero? (mod y 4))
                               (mc-block :glowstone)
                             :else (mc-block :air)))
           mcmap (gen-mcmap (* x-chunks +chunk-side+)
                            (* z-chunks +chunk-side+)
                            generator)]
       (mcmap-to-mcr-binary mcmap 0 0)))
  ([filename]
     (write-file filename (map-exercise-2)))
  ([]
     (map-exercise-2 2 2))
  ([filename x-chunks z-chunks]
     (write-file filename (map-exercise-2 x-chunks z-chunks))))


(defn map-exercise-3
  "Like map-exercise-2, but replaces most of the glowstone with stone,
  and adds some blue and yellow wool and chests on the floors"
  ([x-chunks z-chunks]
     (let [generator (fn [x y z]
                       (cond (> 0.001 (rand))
                               (mc-block :mob-spawner
                                         :mob "Chicken" :delay 0)
                             (> 0.001 (rand))
                               (mc-block :mob-spawner
                                         :mob "Zombie" :delay 200)
                             (and (zero? (mod z 4))
                                  (zero? (mod x 4)))
                               (mc-block :stone)
                             (zero? (mod y 4))
                               (if (> 0.01 (rand))
                                 (mc-block :glowstone)
                                 (mc-block :stone))
                             (= 1 (mod y 4))
                               (cond (> 0.05 (rand)) (mc-block :blue-wool)
                                     (> 0.01 (rand)) (mc-block :yellow-wool)
                                     (> 0.01 (rand))
                                       (mc-block :chest,
                                                 :force-skylight 10,
                                                 :items
                                                 (inventory-list
                                                  [ {:id 57, :slot 13} ]))
                                     :else (mc-block :air))
                             :else (mc-block :air)))
           mcmap (gen-mcmap (* x-chunks +chunk-side+)
                            (* z-chunks +chunk-side+)
                            generator)]
       (mcmap-to-mcr-binary mcmap 0 0)))
  ([filename]
     (write-file filename (map-exercise-3)))
  ([]
     (map-exercise-3 2 2))
  ([filename x-chunks z-chunks]
     (write-file filename (map-exercise-3 x-chunks z-chunks))))


(defn map-exercise-4
  "Like map-exercise-3, but raises the ceilings, gets rid of the wool,
  and fills the chests with speed splash potions for testing speedy
  mobs"
  ([x-chunks z-chunks]
     (let [generator (fn [x y z]
                       (cond (> 0.001 (rand))
                               (mc-block :mob-spawner
                                         :mob "Creeper" :delay 200)
                             (> 0.001 (rand))
                               (mc-block :mob-spawner
                                         :mob "Zombie" :delay 200)
                             (and (zero? (mod z 8))
                                  (zero? (mod x 8)))
                               (mc-block :stone)
                             (zero? (mod y 8))
                               (if (> 0.01 (rand))
                                 (mc-block :glowstone)
                                 (mc-block :stone))
                             (= 1 (mod y 8))
                               (cond (> 0.002 (rand))
                                       (mc-block :chest,
                                                 :skylight 10,
                                                 :items
                                                 (inventory-list
                                                  (map (fn [slot [id dmg]]
                                                         {:id id
                                                          :count 64,
                                                          :slot slot,
                                                          :damage dmg})
                                                       (range 0 27)
                                                       [ [90 0]
                                                         [119 0]
                                                         [120 0x0]
                                                         [120 0x1]
                                                         [120 0x2]
                                                         [120 0x3]
                                                         [120 0x4]
                                                         [120 0x5]
                                                         [120 0x6]
                                                         [120 0x7]])))
                                     :else (mc-block :air))
                             :else (mc-block :air)))
           mcmap (gen-mcmap (* x-chunks +chunk-side+)
                            (* z-chunks +chunk-side+)
                            generator)]
       (mcmap-to-mcr-binary mcmap 0 0)))
  ([filename]
     (write-file filename (map-exercise-4)))
  ([]
     (map-exercise-4 2 2))
  ([filename x-chunks z-chunks]
     (write-file filename (map-exercise-4 x-chunks z-chunks))))

(defn map-exercise-5
  "Makes a one-chunk world with stone below y=64 and a single mob at
  [8 65 8]"
  ([]
     (let [generator (fn [x y z]
                       (cond (= [x y z] [8 65 8])
                               (mc-block :air
                                         :ents
                                         [{:type :creeper
                                           :powered true
                                           ;:death-time -400
                                           }])
                             (< y 64)
                               (mc-block :stone)
                             :else
                               (mc-block :air)))]
       (mcmap-to-mcr-binary (gen-mcmap 16 16 generator)
                            0 0))))

(defn map-exercise-6
  "Makes a one-chunk world in mca format with stone below y=64 and a
  block of diamond at [8 65 8]"
  ([]
     (let [generator (fn [x y z]
                       (cond (= [x y z] [8 65 8])
                               (mc-block :diamond-block)
                             (< y 64)
                               (mc-block :stone)
                             :else
                               (mc-block :air)))]
       (mcmap-to-mca-binary (gen-mcmap 16 16 generator)
                            0 0))))

(defn map-exercise-7
  "Makes a one-chunk world in mcr format with stone below y=64 and a
  block of gold at [8 65 8]"
  ([]
     (let [generator (fn [x y z]
                       (cond (= [x y z] [8 65 8])
                               (mc-block :gold-block)
                             (< y 64)
                               (mc-block :stone)
                             :else
                               (mc-block :air)))]
       (mcmap-to-mcr-binary (gen-mcmap 16 16 generator)
                            0 0))))

(defn map-exercise-8
  "Generates a 16x16 grid of 5x5 areas with each of the possible 256
  biome types, each with a sign in the center containing the biome
  number"
  ([]
     (let [generator (fn [x y z]
                       (let [bx (quot x 5)
                             bz (quot z 5)
                             b (-> bx (* 16) (+ bz))]
                         (cond (= y 127)
                                 (mc-block :air :biome b)
                               (= [2 64 2]
                                  [(mod x 5) y (mod z 5)])
                                 (mc-block :sign-post
                                           :text ["" (str b)]
                                           :face :north)
                               (< y 64)
                                 :stone
                               :else
                                 :air)))
           mcmap (gen-mcmap 80 80 generator)]
       (mcmap-to-mca-binary mcmap 0 0))))

(defn map-exercise-9
  "Returns a seq of the .mca regions 0,0, -1,0, 0,-1, and -1,-1, for a
  single zone centered on x=0,z=0 made up of sandwiches of glowstone
  and air with columns of stone"
  ([x-chunks z-chunks]
     (let [generator (fn [x y z]
                       (if (and (zero? (mod z 4))
                                (zero? (mod x 4)))
                         (mc-block :stone)
                         (if (zero? (mod y 4))
                           (mc-block :glowstone)
                           (mc-block :air))))
           mcmap (gen-mcmap (* x-chunks +chunk-side+)
                            128
                            (* z-chunks +chunk-side+)
                            (* x-chunks +chunk-side+ -1/2)
                            (* z-chunks +chunk-side+ -1/2)
                            generator)]
       [ (mcmap-to-mca-binary mcmap 0 0)
         (mcmap-to-mca-binary mcmap 0 -1)
         (mcmap-to-mca-binary mcmap -1 0)
         (mcmap-to-mca-binary mcmap -1 -1)]))
  ([]
     (map-exercise-1 2 2)))
