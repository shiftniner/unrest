(ns mcmap.srand
  (:import java.util.Random))

;;; CAUTION: I threw this together pretty quickly on a seat-of-the-
;;; pants design, so it is probably deeply flawed as a PRNG, but it
;;; seems to produce at least superficially decent-looking results
;;; when used correctly.  (And in fact I have found that consecutive
;;; outputs of #(srand 1 s1...sN %) for any series of seeds and salts
;;; s1...sN have a slight but statistically significant correlation.
;;; I intend not to fix this, because the results still look more than
;;; good enough for my purposes, and I have already picked out some
;;; epic-cave-network seeds I like.)

;;; Correct usage is to generate pseudorandom sequences by
;;; incrementing a salt, NOT by using value n as the seed to produce
;;; value n+1.  The latter likely leads to any number of short cycles.

;;; Sequences may be branched off of other sequences by using the
;;; trunk sequence to generate a random seed to be used for the branch
;;; sequence, with the caveat that numerous such branches at the same
;;; intervals can lead to the same cycling problem as producing
;;; sequences by recursively reseeding.

(def +seed-max+ (bit-shift-left 1 48))

(def entropy-pool (java.security.SecureRandom.))

(defn- srand-1-long
  ([seed]
     (let [r (Random. seed)]
       (.nextLong r))))

(defn- salt-seed
  ([seed salts]
     (if (seq salts)
       (recur (unchecked-add (long (srand-1-long seed))
                             (first salts))
              (rest salts))
       seed)))

(defn- srand-1
  ([seed salts]
     (let [r (Random. (salt-seed seed salts))]
       (.nextDouble r))))

(defn srand
  "Returns a uniform deviate n, such that 0 <= n < max, as a pure
function of the seed and salt arguments, which should all be longs"
  ([max seed & salts]
     (* max (srand-1 seed (concat salts [1])))))

(defn make-seed
  "Takes any arbitrary string or seq of hashable values and returns a
long seed for srand"
  ([s]
     (bit-xor (bit-shift-left (hash (partition 1 2 s))
                              16)
              (hash (partition 1 2 (drop 1 s))))))

(defn reseed
  "Takes a seed and some salts and returns a new seed"
  ([seed & salts]
     (long (apply srand +seed-max+ seed salts))))

(defn snorm
  "Returns a Gaussian deviate with the given mean, standard deviation,
and optional minimum and maximum (by rejecting values outside the
range), as a pure function of the seed and salt arguments, which
should all be longs"
  ([ [mean std-dev min-val max-val] seed & salts]
     (let [r (Random. (salt-seed seed (concat salts [1])))]
       (loop []
         (let [n (+ mean (* std-dev (.nextGaussian r)))]
           (if (or (and min-val
                        (< n min-val))
                   (and max-val
                        (>= n max-val)))
             (recur)
             n))))))

(defn sranditem
  "Returns an item from the given vector chosen at random"
  ([v seed & salts]
     (let [index (int (apply srand (count v)
                             seed salts))]
       (v index))))

;;; The below functions are for generating non-deterministic random
;;; seeds; there is no need to preserve results, so any flaws in the
;;; algorithm may be corrected.

(defn add-entropy-ints
  "Takes a seq of ints or longs and adds their entropy to the entropy
pool"
  ([is]
     (doseq [i is]
       (.setSeed entropy-pool (long i)))))

(defn bits-to-int
  "Takes a seq of bits and returns the unsigned integer represented by
that big-endian seq"
  ([bs]
     (reduce (fn [i b]
               (+' b (*' 2 i)))
             bs)))

(defn bytes-to-int
  "Takes a seq of bytes and returns the signed integer represented by
that big-endian seq"
  ([bs]
     (reduce (fn [i b]
               (+' (bit-and b 255)
                   (*' 256 i)))
             bs)))

(defn read-urandom-bytes
  "Takes a number of bytes to read, n, and if the host OS has a
/dev/urandom file, reads n bytes from /dev/urandom and converts them
to a signed integer; if there is no /dev/urandom, always returns 0"
  ([n]
     (let [f (java.io.File. "/dev/urandom")]
       (if (.exists f)
         (with-open [fis (java.io.FileInputStream. f)]
           (let [ba (byte-array n)]
             (.read fis ba)
             (bytes-to-int (seq ba))))
         0))))

(defn scheduler-entropy
  "Takes a number of seconds and gathers scheduler entropy for that
many seconds, returning a seq of unpredictable ints; known to produce
good entropy on Mac OS X and Linux -- needs to be tested in Windows"
  ([secs]
     (let [start (System/nanoTime)
           finish (+ start (long (* secs 1000000000)))]
       (take-while (fn [_] (< (System/nanoTime) finish))
         (map (comp hash bits-to-int)
              (partition 256
                         (map #(bit-and 1 (mod (apply - %) 3))
                              (partition 2 1
                                (for [n (range)]
                                  (do (Thread/sleep 0)
                                      (System/nanoTime)))))))))))

(defn choose-random-seed
  "Returns a 48-bit seed chosen at random in such a way as to
be (hopefully) hard to guess, such that a YouTube LPer could use a
seed as returned by this function and not need to worry about a viewer
guessing the seed and posting spoilers"
  ([]
     (add-entropy-ints
      (list* (hash (Object.))
             (hash (Object.))
             (hash (System/getProperties))
             (hash (Object.))
             (hash (Object.))
             (read-urandom-bytes 8)
             (read-urandom-bytes 8)
             (read-urandom-bytes 8)
             (read-urandom-bytes 8)
             (concat (scheduler-entropy 0.2)
                     [(hash (Object.))
                      (hash (Object.))])))
     (bytes-to-int (.generateSeed entropy-pool 6))))

