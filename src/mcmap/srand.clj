(ns mcmap.srand
  (:import java.util.Random))

;;; CAUTION: I threw this together pretty quickly on a seat-of-the-
;;; pants design, so it is probably deeply flawed as a PRNG, but it
;;; seems to produce at least superficially decent-looking results
;;; when used correctly.

;;; Correct usage is to generate pseudorandom sequences by
;;; incrementing a salt, NOT by using value n as the seed to produce
;;; value n+1.  The latter likely leads to any number of short cycles.

;;; Sequences may be branched off of other sequences by using the
;;; trunk sequence to generate a random seed to be used for the branch
;;; sequence, with the caveat that numerous such branches at the same
;;; intervals can lead to the same cycling problem as producing
;;; sequences by recursively reseeding.

(def +seed-max+ (bit-shift-left 1 48))

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
