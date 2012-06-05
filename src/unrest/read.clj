(ns unrest.read
  (:use unrest.core
        unrest.blocks
        unrest.srand  ; XXX for bytes-to-int; move to util.clj
        unrest.util)
  (:import java.nio.ByteBuffer
           java.io.FileInputStream
           java.util.zip.Inflater))

(set! *warn-on-reflection* true)

;;; Functions for reading existing Minecraft save files

;;; Currently all that exists here is what was needed, temporarily, to
;;; aid manually reverse-engineering .mca files.  I expect it to be
;;; useful in generating larger maps.

(def +sector-length+ 4096)

(defn read-file
  "Takes a filename and returns a byte-buffer with the file's
  contents"
  ([^String filename]
     (with-open [in (FileInputStream. filename)]
       (loop [ret []
              total-bytes 0]
         (let [ba (byte-array 4096)
               bytes-read (.read in ba)
               total-bytes (+ total-bytes bytes-read)]
           (if (pos? bytes-read)
             (recur (conj ret (ByteBuffer/wrap ba))
                    total-bytes)
             (cons total-bytes ret)))))))

(defn get-bytes
  "Takes an mcmap byte-buffer and returns a seq of bytes"
  ([b]
     (mapcat #(seq (.array ^ByteBuffer %))
             (rest b))))

(defn read-locations
  "Takes a seq of 4096 bytes and returns a map of [x,z] => [filepos,
  length]"
  ([bs]
     (apply hash-map
            (mapcat (fn [ [off0 off1 off2 sectors]
                          x z]
                      (let [offset (bytes-to-int [0 off0 off1 off2])]
                        (when (pos? offset)
                          [ [x z]
                            [ (* +sector-length+ offset)
                              (* +sector-length+ sectors)]])))
                    (partition 4 bs)
                    (apply concat (repeat (range 32)))
                    (mapcat #(repeat 32 %) (range 32))))))

(defn trim-chunk
  "Takes a seq of bytes starting with the number of bytes in the given
  chunk, and returns the (compressed) chunk as a seq of bytes"
  ([bs]
     (let [n-bytes (bytes-to-int (cons 0 (take 4 bs)))
           ret (take (dec n-bytes)
                     (drop 4 bs))]
       (case (int (first ret))
             2 (rest ret)
             1 (throw (RuntimeException. "gzipped chunk encountered!"))
             (throw (RuntimeException. (str "unknown compression type "
                                            (first ret) " encountered!")))))))

(defn read-chunks
  "Takes an mcmap byte-buffer (a seq of a total length and one or more
  ByteBuffer objects) containing a whole McRegion or Anvil file and
  returns a lazy seq of chunks in the order in which they appear in
  the file, in {:x chunk-x, :z chunk-z, :compressed-data byte-buffer}
  form"
  ([region]
     (let [region-bytes (get-bytes region)
           region nil
           locations (read-locations (take 4096 region-bytes))
           region-bytes (drop 8192 region-bytes)
           locs-in-order (sort-by locations (keys locations))]
       (when (seq locs-in-order)
         ( (fn next-chunk
             ([pos region-bytes locs-in-order]
                (lazy-seq
                 (let [ [x z] (first locs-in-order)
                        [chunk-pos chunk-length] (locations [x z])
                        skip-ahead (+ chunk-length (- chunk-pos pos))]
                   (cons {:x x
                          :z z
                          :compressed-data (byte-buffer
                                            (trim-chunk
                                             (take chunk-length
                                                   (drop (- chunk-pos pos)
                                                         region-bytes))))}
                         (if (next locs-in-order)
                           (next-chunk (+ pos skip-ahead)
                                       (drop skip-ahead region-bytes)
                                       (next locs-in-order))))))))
           8192
           region-bytes
           locs-in-order)))))

(defn zlib-decompress
  "Takes a byte-buffer of zlib-compressed data and returns a
  byte-buffer of uncompressed data"
  ([b]
     (zlib-decompress b 131072))
  ([b maxlen]
     (when (not= (count b) 2)
       (throw (RuntimeException. (str "zlib-decompress currently"
                                      " requires a single-object"
                                      " byte-buffer"))))
     (when (> maxlen #=(* 8 1024 1024))
       (throw (RuntimeException. "max decompressed length exceeded")))
     (let [inf (Inflater.)
           ret (byte-array maxlen)
           ret-len (do
                     (.setInput inf (.array ^ByteBuffer (second b)))
                     (.inflate inf ret))]
       (.end inf)
       (if (< ret-len maxlen)
         (byte-buffer (take ret-len ret))
         (recur b (* 4 maxlen))))))
