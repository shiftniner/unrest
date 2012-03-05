(ns mcmap.util
  (:import java.text.SimpleDateFormat
           java.util.Date))

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
    ;; XXX Make this print only if current *msg-level* >= level.  I.e.,
    ;; higher level implies chattier, less important messages.
    ([level & atoms]
       (let [time-str (.format date-formatter (Date.))]
         (println (apply str time-str atoms))))))

(defn tmsg
  ([& args]
     (apply msg args)
     true))

(defmacro if-let*
  "bindings => (binding-form test)*

  If all tests are true, evaluates then with each binding-form bound
  to the value of its respective test; if not, yields else"
  ([bindings then]
     `(if-let* ~bindings ~then nil))
  ([bindings then else]
     (when (= 1 (mod (count bindings)
                     2))
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
