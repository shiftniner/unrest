(ns mcmap.util
  (:import java.text.SimpleDateFormat
           java.util.Date
           [java.io BufferedReader InputStreamReader File]))

(set! *warn-on-reflection* true)

(defmacro forcat
  "Returns the result of applying concat to the result of the list
  comprehension (for args...)"
  ([& args]
     `(apply concat (for ~@args))))

(defn vec-if-not-coll
  "Returns the argument if it is already a collection, or a one-item
  vector containing the argument if it is not"
  ([arg]
     (if (coll? arg)
       arg
       [arg])))

(defn mapcat?
  "Returns the result of applying concat to the results of applying
  map to f and colls, replacing any of those results that are not
  collections with one-item vectors"
  ([f & colls]
     (apply concat (map vec-if-not-coll
                        (apply map f colls)))))

(let [date-formatter (SimpleDateFormat. "yyyy-MM-dd HH:mm:ss.SSS - ")]
  (defn msg
    "Prints a timestamp and a message"
    ;; XXX Make this print only if current *msg-level* >= level.  I.e.,
    ;; higher level implies chattier, less important messages.
    ([level & atoms]
       (let [time-str (.format date-formatter (Date.))]
         (print (apply str time-str (concat atoms ["\n"])))
         (.flush *out*)))))

(defn tmsg
  "Like msg, but returns true"
  ([& args]
     (apply msg args)
     true))

(defn v
  "Prints the given arg with msg and returns it"
  ([arg]
     (msg 0 arg)
     arg))

(def onemsg (memoize msg))

(defmacro if-let*
  "bindings => (binding-form test)*

  If all tests are true, evaluates then with each binding-form bound
  to the value of its respective test; if not, yields else"
  ([bindings then]
     `(if-let* ~bindings ~then nil))
  ([bindings then else]
     (when (odd? (count bindings))
       (throw (IllegalArgumentException.
               (str "if-let* requires an even number of forms in"
                    " binding vector"))))
     (let [false-sym (gensym "falseval__")]
       (letfn [(expand-if-lets
                ([form tst & more]
                   `(if-let [~form ~tst]
                      ~(apply expand-if-lets more)
                      ~false-sym))
                ([form]
                   form))]
         `(let [~false-sym (Object.)
                result# ~(apply expand-if-lets (concat bindings [then]))]
            (if (= result# ~false-sym)
              ~else
              result#))))))

(defmacro pfor
  "Parallelized list comprehension.  Takes a vector of one or more
  binding-form/collection-expr pairs, each followed by zero or more
  modifiers, and yields a lazy parallel sequence of evaluations of
  expr.  Collections are iterated in a nested fashion, rightmost
  fastest, and nested coll-exprs can refer to bindings created in
  prior binding-forms.  Supported modifiers are: :let [binding-form
  expr ...], :while test, :when test.

  (take 10 (pfor [y (range)
                  x (range)
                  :while (> y x)]
              (doall
               (clojure-contrib.combinatorics/permutations (range x y)))))"
  ([seq-exprs body-expr]
     (let [symbols (filter symbol? (take-nth 2 seq-exprs))]
       (if (= 1 (count symbols))
         (let [sym (first symbols)]
           `(pmap (fn [~sym] ~body-expr)
                  (for ~seq-exprs ~sym)))
         `(pmap (fn [ [~@symbols] ] ~body-expr)
                (for ~seq-exprs [~@symbols]))))))

(defn duplicates
  "Returns a seq of all items that occur more than once in the given
  collection; items that occur more than twice in s will be returned
  more than once"
  ([s]
     (lazy-seq
      ( (fn duplicates' [s seen]
          (when (seq s)
            (let [f (first s)
                  r (rest s)]
              (if (seen f)
                (cons f
                      (lazy-seq
                       (duplicates' r seen)))
                (recur r
                       (conj seen f))))))
        s #{} ))))

(defn vtake
  "Like take, but prints progress messages, formatted using format, at
  msg level 10"
  ([n print-step format-msg s]
     (map #(do
             (when (and (pos? %2)
                        (zero? (mod %2 print-step)))
               (msg 10 (format format-msg %2)))
             %1)
          s
          (range n))))

(defn dup-seq
  "Takes a seq and returns a seq consisting of n consecutive
  repetitions (2 if no n parameter is given) of each member of the
  seq"
  ([s]
     (dup-seq s 2))
  ([s n]
     (mapcat #(repeat n %)
             s)))

(defn uniq
  "Analogous to the Unix utility of the same name, takes a seq and
  returns it with any consecutive runs of equal items replaced with
  one of that item"
  ([s]
     (lazy-seq
      (when (seq s)
        (let [f (first s)]
          (cons f
                (uniq (drop-while #(= f %)
                                  s))))))))

(defn die
  "Just an abbreviation for applying str and throwing a runtime
  exception"
  ([& args]
     (throw (RuntimeException. ^String (apply str args)))))

(defn transition
  "Returns a seq of n duplicates of the two given items, starting at
  100% item a, and gradually including increasing amounts of item b,
  ending at 100% item b; seq is typically not symmetric and will
  include more than half of item a whether n is even or odd"
  ;; Yes, the latter caveat could easily be improved upon, but it
  ;; would change map generation, and is just not necessary
  ([n a b]
     (transition n a b n 0))
  ([n a b n-max balance]
     (lazy-seq
      (when (pos? n)
        (let [choice (if (< balance 1/2)
                       a b)
              balance (+ balance (- 1 (/ n n-max))
                         (if (identical? choice a)
                           0 -1))]
          (cons choice
                (transition (dec n)
                            a b n-max balance)))))))

(defn scale-ints
  "Takes a target number and a seq of numbers (not necessarily
  integers), and returns a seq of nonzero integers closely
  proportional to the given seq, and summing to target"
  ([target is]
     (scale-ints target (reduce + is) is))
  ([target sum is]
     (lazy-seq
      (when (seq is)
        (let [factor (/ target (if (zero? sum)
                                 1 sum))
              first-ret (int (+ 0.5 (* factor (first is))))
              first-ret (if (zero? first-ret)
                          1 first-ret)
              first-ret (if (> first-ret
                               (inc (- target (count is))))
                          (inc (- target (count is)))
                          first-ret)]
          (cons first-ret
                (scale-ints (- target first-ret)
                            (- sum (first is))
                            (rest is))))))))

(defn interleave-n
  "Interleaves n1 of items from c1, then n2 of items in c2, etc."
  ([n1 c1 n2 c2]
     (lazy-seq
      (let [s1 (seq c1) s2 (seq c2)]
        (when (and s1 s2)
          (concat (take n1 s1)
                  (take n2 s2)
                  (interleave-n n1 (drop n1 s1)
                                n2 (drop n2 s2)))))))
  ([n1 c1 n2 c2 & colls]
     (lazy-seq
      (let [ss (map seq (conj (take-nth 2 (rest colls))
                              c2 c1))
            ns (conj (take-nth 2 colls)
                     n2 n1)]
        (when (every? identity ss)
          (concat (mapcat take ns ss)
                  (apply interleave-n (interleave ns (map drop ns ss)))))))))

(defn square
  "Returns n*n"
  ([n]
     (* n n)))

(defmacro memo
  "Abbreviation for memoizing functions, so the function name need not
  be duplicated; use this as an adverb, before a defn or defn-"
  ([& defn-forms]
     (let [defn-pos (inc (count (take-while #(not (#{'defn 'defn-}
                                                   %))
                                            defn-forms)))
           fn-name (first (drop defn-pos defn-forms))]
       `(do
          ~defn-forms
          (def ~fn-name (memoize ~fn-name))))))

(defn nonzero?
  "Analogous to zero? and not=, equivalent to (not (zero? n))"
  ([n]
     (not (zero? n))))

(defn sum-counts
  "Takes a seq of maps of values to numbers (and nils, which are
  ignored), and returns a map of values to the sums of all the numbers
  the values map to in all the maps in the seq"
  ([ms]
     (reduce (fn [sums m]
               (reduce (fn [sums k]
                         (assoc sums k
                                (+ (or (sums k)
                                       0)
                                   (m k))))
                       sums
                       (keys m)))
             {}
             ms)))

(defn run-cmd
  "Runs the given command synchronously, printing its output, and
  returning the exit code"
  ([& strs]
     (apply println "WARNING: non-portable run-cmd used, with args:" strs)
     (let [cmd (apply str strs)
           rt (Runtime/getRuntime)
           pr (.exec rt ^"[Ljava.lang.String;" (into-array ["sh" "-c" cmd]))
           input (java.io.BufferedReader.
                  (java.io.InputStreamReader.
                   (.getInputStream pr)))
           stderr (java.io.BufferedReader.
                   (java.io.InputStreamReader.
                    (.getErrorStream pr)))]
       (loop []
         (let [line (.readLine input)]
           (when line
             (println line)
             (recur))))
       (loop []
         (let [line (.readLine stderr)]
           (when line
             (println line)
             (recur))))
       (.waitFor pr))))

(defn exhaustive-interleave
  "Returns a lazy seq of the first item in each coll, then the second
  etc., eventually returning all items in all given seqs"
  ([c] c)
  ([c1 c2]
     (lazy-seq
      (let [s1 (seq c1)
            s2 (seq c2)]
        (cond (and s1 s2)
              (cons (first s1)
                    (cons (first s2)
                          (exhaustive-interleave (rest s1)
                                                 (rest s2))))
              s1 s1
              :else s2))))
  ([c1 c2 & colls]
     (lazy-seq
      (let [ss (filter identity (map seq (conj colls c2 c1)))]
        (when (seq ss)
          (concat (map first ss)
                  (apply exhaustive-interleave
                         (map rest ss))))))))

(defn dir-exists
  "Returns true if a directory with the given pathname exists"
  ([p]
     (.isDirectory (File. ^String p))))

(defn mac-hint
  "Type-hints a symbol in a macro"
  ([sym c]
     (with-meta sym {:tag c})))

(defn objects
  "Casts to Object[]"
  {:inline (fn [xs]
             (mac-hint xs "[Ljava.lang.Object;"))}
  (^"[Ljava.lang.Object;" [xs]
    xs))

;;; Borrowed from the Wikipedia page on Clojure; I can't take credit
;;; for this -- hope the Wikipedia license is compatible with the EPL

(def bit-bucket-writer
     (proxy [java.io.Writer] []
       (write [buf] nil)
       (close []    nil)
       (flush []    nil)))

(defmacro noprint
  "Evaluates the given expressions with all printing to *out* silenced."
  [& forms]
  `(binding [*out* bit-bucket-writer]
     ~@forms))

(defmacro bignum
  "Returns the number whose decimal representation is the
  concatenation of the decimal representations of the given numbers,
  which must be compile-time constants"
  ([& smallnums]
     `(BigInteger. ~(apply str smallnums))))

(defn set=
  "Takes two collections and returns true if they have the same sets
  of elements"
  ([a b]
     (= (set a)
        (set b))))
