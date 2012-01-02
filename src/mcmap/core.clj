(ns mcmap.core
  (:import java.nio.ByteBuffer
           java.util.zip.Deflater
           java.io.FileOutputStream))

(def +region-side+ (* 16 32))
(def +chunk-side+ 16)
(def +chunk-height+ 128)

(defn mc-block
  "Returns the specified block in mcmap's internal data format"
  ([type]
     (case type
           :blue-wool    [:wool 0xB]
           :yellow-wool  [:wool 0x4]
           :wool         [:wool 0x0]
           ;; default:
           type))
  ([type & extra-data]
     (vec (cons type extra-data))))

(defn gen-mcmap-zone
  "Takes x and z dimensions, and a function of x y and z returning a
block, and returns a zone of the specified size"
  ;; Probably want to make this a single vector later, in a structure
  ;; with coordinates.
  ([x-size z-size f]
     (gen-mcmap-zone x-size +chunk-height+ z-size f))
  ([x-size y-size z-size f]
     (vec (for [x (range x-size)]
            (vec (for [y (range y-size)]
                   (vec (for [z (range z-size)]
                          (f x y z)))))))))

(defn zone-z-size
  ([zone]
     (count ( (zone 0) 0 ))))

(defn zone-y-size
  ([zone]
     (count (zone 0))))

(defn zone-x-size
  ([zone]
     (count zone)))

(defn zone-lookup
  ([zone x y z]
     ( ( (zone x) y) z )))

(defn chunk-in-zone?
  "Returns true if and only if the given region and chunk coordinates
include any part of the given zone"
  ([zone region-x region-z chunk-x chunk-z]
     (let [chunk-x0 (+ (* +region-side+ region-x)
                       (* +chunk-side+  chunk-x))
           chunk-z0 (+ (* +region-side+ region-z)
                       (* +chunk-side+  chunk-z))]
       (and (not (neg? chunk-x0))
            (not (neg? chunk-z0))
            (< chunk-x0 (zone-x-size zone))
            (< chunk-z0 (zone-z-size zone))))))

(defn sub-zone
  "Returns a zone within the given zone, for the given coordinate ranges"
  ([zone x0 x1 y0 y1 z0 z1]
     (gen-mcmap-zone (- x1 x0)
                     (- y1 y0)
                     (- z1 z0)
                     #(zone-lookup zone
                                   (+ %1 x0)
                                   (+ %2 y0)
                                   (+ %3 z0)))))

(defn block-id
  "Returns the byte block ID for the given zone element"
  ;; XXX just supporting air and glowstone for right now
  ([ze]
     (if (vector? ze)
       (block-id (first ze))
       (or ( {:air              0
              :stone            1
              :wool             35
              :fire             51
              :mob-spawner      52
              :monster-spawner  52
              :glowstone        89
              }
             ze )
           (throw (RuntimeException. (str "Block ID unknown for " ze)))))))

(defn block-ids
  "Returns a seq of bytes with block IDs for the given zone; assumes zone
contains at least one block"
  ([zone]
     (for [x (range (zone-x-size zone))
           z (range (zone-z-size zone))
           y (range (zone-y-size zone))]
       (block-id (zone-lookup zone x y z)))))

(defn byte-buffer
  "Given a seq of bytes (or ints between -128 and 255; values over 127
will be folded), returns a byte buffer"
  ([bs]
     (ByteBuffer/wrap
        (into-array Byte/TYPE
                    (map #(if (> % 127)
                            (- % 256)
                            %)
                         bs)))))

(defn concat-bytes
  "Takes any number of byte buffers and concatenates them into a
single byte buffer"
  ([& byte-buffers]
     (byte-buffer (mapcat #(.array %)
                          byte-buffers))))

(defn utf8-bytes
  ([s]
     (.getBytes s "UTF-8")))

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

(defn tag-int
  "Returns a binary-formatted TAG_Int"
  ;; I should really copy fixnum-to-bytes-4 from the other project.  Maybe
  ;; that should go in its own library.
  ([tag-name n]
     (concat-bytes (byte-buffer [3])
                   (tag-string tag-name)
                   (tag-int n)))
  ([n]
     (byte-buffer [(bit-shift-right n 24)
                   (bit-and 255 (bit-shift-right n 16))
                   (bit-and 255 (bit-shift-right n 8))
                   (bit-and 255 n)])))

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
     (if (vector? ze)
       (let [t (ze 0)]
         (case t
               (:wool)
                 (or (ze 1) 0)
               ;; default:
               0))
       0)))

(defn block-data
  "Returns a seq of bytes with extra block data for the given zone"
  ([zone]
     (nybbles-to-bytes
        (map block-datum
             (for [x (range (zone-x-size zone))
                   z (range (zone-z-size zone))
                   y (range (zone-y-size zone))]
               (zone-lookup zone x y z))))))

(defn sky-light
  "Returns a seq of bytes with sky light data for the given zone"
  ;; XXX hardcoded as 0 for every block for now
  ([zone]
     (repeat 16384 (byte 0))))

(defn block-light
  "Returns a seq of bytes with block light data, which must have been
precomputed with compute-block-light, for the given zone"
  ;; XXX hardcoded as 15 for every block for now
  ([zone]
     (repeat 16384 (byte -1))))

(defn height-map
  "Returns a seq of bytes of heightmap data for the given zone"
  ;; XXX hardcoded as 127 for everything for now
  ([zone]
     (repeat 256 (byte 127))))

(defn tile-entity
  ([ [ze x y z] ]
     (when (vector? ze)
       (let [t (ze 0)]
         (case t
               (:monster-spawner :mob-spawner)
                 [ (tag-compound
                      [ (tag-string "id" "MobSpawner")
                        (tag-int "x" x)
                        (tag-int "y" y)
                        (tag-int "z" z)
                        (tag-string "EntityId" (ze 1))
                        (tag-short "Delay" (or (ze 2)
                                               200))])]
               ;; default:
               nil)))))

(defn tile-entities
  "Returns a seq of TAG_Compounds"
  ([zone x0 z0]
     (mapcat tile-entity
             (for [x (range (zone-x-size zone))
                   z (range (zone-z-size zone))
                   y (range (zone-y-size zone))]
               [ (zone-lookup zone x y z) (+ x0 x) y (+ z0 z) ]))))

(defn zlib-compress
  "Returns a byte buffer containing a zlib compressed version of the
data in the given byte buffer"
  ([buf]
     (let [bs (.array buf)
           out (byte-array (count bs))
           c (doto (Deflater.)
               (.setInput bs)
               (.finish))
           c-len (.deflate c out)]
       (byte-buffer (take c-len out)))))

(defn extract-chunk
  "Returns a binary chunk {:x <chunk-x> :z <chunk-z> :data
<byte-buffer>} at the given coordinates within the given zone"
  ([zone region-x region-z chunk-x chunk-z]
     (let [x0 (+ (* +region-side+ region-x)
                 (* +chunk-side+ chunk-x))
           z0 (+ (* +region-side+ region-z)
                 (* +chunk-side+ chunk-z))
           blocks (sub-zone zone
                            x0 (+ x0 +chunk-side+)
                            0 +chunk-height+
                            z0 (+ z0 +chunk-side+))
           data (tag-compound ""
                  [ (tag-compound "Level"
                      [ (tag-byte-array "Blocks"
                                        (block-ids blocks))
                        (tag-byte-array "Data"
                                        (block-data blocks))
                        (tag-byte-array "SkyLight"
                                        (sky-light blocks))
                        (tag-byte-array "BlockLight"
                                        (block-light blocks))
                        (tag-byte-array "HeightMap"
                                        (height-map blocks))
                        (tag-list "Entities" 10 [])
                        (tag-list "TileEntities" 10
                                  (tile-entities blocks x0 z0))
                        (tag-list "TileTicks" 10 [])
                        (tag-long "LastUpdate" 1)
                        (tag-int "xPos" (/ x0 +chunk-side+))
                        (tag-int "zPos" (/ z0 +chunk-side+))
                        (tag-byte "TerrainPopulated" 1) ]) ])]
       {:x chunk-x
        :z chunk-z
        :data data
        :compressed-data (zlib-compress data)})))

(defn zone-to-chunks
  "Returns a seq of chunks for the given zone and region coordinates,
where each chunk is {:x <chunk-x> :z <chunk-z> :data <byte-buffer>"
  ([zone x z]
     (for [chunk-x (range 32) chunk-z (range 32)
           :when (chunk-in-zone? zone x z chunk-x chunk-z)]
       (extract-chunk zone x z chunk-x chunk-z))))

(defn byte-buffer-size
  "Returns the length in bytes of the given byte buffer"
  ([b]
     (count (seq (.array b)))))

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
     ;; need chunk timestamps?  0 for everything should be safe
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
       [(byte-buffer (repeat pad-needed 0))
        (tag-int (inc len))
        (tag-byte compression-type)
        data
        (byte-buffer (repeat post-pad-needed 0))])))

(defn place-chunks
  "Given seqs of chunks and locations, returns a byte buffer
containing the chunks for an mcr file"
  ([chunks locs]
     (let [shifted-locs (cons {:offset 0, :count 2} locs)
           chunks-and-pads (mapcat pad-chunk chunks locs shifted-locs)]
       (apply concat-bytes chunks-and-pads))))

(defn zone-to-region
  "Takes a zone and two region coordinates, and returns a region
extracted from that zone, in Minecraft beta .mcr format"
  ([zone x z]
     (let [chunks (zone-to-chunks zone x z)
           locs (locations chunks)]
       (concat-bytes (locations-to-bytes locs)
                     (timestamps chunks)
                     (place-chunks chunks locs)))))

(defn write-file
  "Writes the given byte buffer to the given filename"
  ([filename byte-buffer]
     (with-open [out (FileOutputStream. filename)]
       (.write out (.array byte-buffer)))))

(defn map-exercise-1
  "Returns the .mcr binary data for a single region made up of
sandwiches of glowstone and air with columns of stone, or writes it to
a file if given a filename; generates two chunks by two chunks if not
given two dimension arguments"
  ([x-chunks z-chunks]
     (let [generator (fn [x y z]
                       (if (and (zero? (mod z 4))
                                (zero? (mod x 4)))
                         (mc-block :stone)
                         (if (zero? (mod y 4))
                           (mc-block :glowstone)
                           (mc-block :air))))
           region (gen-mcmap-zone (* x-chunks +chunk-side+)
                                  (* z-chunks +chunk-side+)
                                  generator)]
       (zone-to-region region 0 0)))
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
                               (mc-block :monster-spawner "Chicken" 0)
                             (> 0.001 (rand))
                               (mc-block :monster-spawner "Zombie" 200)
                             (and (zero? (mod z 4))
                                  (zero? (mod x 4)))
                               (mc-block :stone)
                             (zero? (mod y 4))
                               (mc-block :glowstone)
                             :else (mc-block :air)))
           region (gen-mcmap-zone (* x-chunks +chunk-side+)
                                  (* z-chunks +chunk-side+)
                                  generator)]
       (zone-to-region region 0 0)))
  ([filename]
     (write-file filename (map-exercise-2)))
  ([]
     (map-exercise-2 2 2))
  ([filename x-chunks z-chunks]
     (write-file filename (map-exercise-2 x-chunks z-chunks))))


(defn map-exercise-3
  "Like map-exercise-2, but adds some blue and yellow wool on the
glowstone"
  ([x-chunks z-chunks]
     (let [generator (fn [x y z]
                       (cond (> 0.001 (rand))
                               (mc-block :monster-spawner "Chicken" 0)
                             (> 0.001 (rand))
                               (mc-block :monster-spawner "Zombie" 200)
                             (and (zero? (mod z 4))
                                  (zero? (mod x 4)))
                               (mc-block :stone)
                             (zero? (mod y 4))
                               (mc-block :glowstone)
                             (= 1 (mod y 4))
                               (cond (> 0.05 (rand)) (mc-block :blue-wool)
                                     (> 0.01 (rand)) (mc-block :yellow-wool)
                                     :else           (mc-block :air))
                             :else (mc-block :air)))
           region (gen-mcmap-zone (* x-chunks +chunk-side+)
                                  (* z-chunks +chunk-side+)
                                  generator)]
       (zone-to-region region 0 0)))
  ([filename]
     (write-file filename (map-exercise-3)))
  ([]
     (map-exercise-3 2 2))
  ([filename x-chunks z-chunks]
     (write-file filename (map-exercise-3 x-chunks z-chunks))))

