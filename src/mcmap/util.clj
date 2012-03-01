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

