(ns mcmap.dungeons
  (:use mcmap.layout
        mcmap.toolkit
        mcmap.blocks
        mcmap.core
        mcmap.srand
        mcmap.util))

;;; mcmap.dungeons contains definitions of various dungeons.
;;; mcmap.dungeon contains code for placing dungeons in a map.

(let [dungeon-type-match (fn dungeon-type-match
                           ;; returns true if d either is, or is a
                           ;; collection that contains, dtype
                           ([dtype d]
                              (some (partial = dtype)
                                    (vec-if-not-coll d))))]
  (defmacro defdungeons
    "Defines dungeons using the following syntax:

  (defdungeons
   optional-docstring
   (type-or-list-of-types
    name
    fn) ...)

  Where type-or-list-of-types is either a keyword of a collection of
  keywords, name is a symbol, bindings is a binding vector with two
  names (the first for the y coordinate of the dungeon, and the second
  for the seed), and body is one or more expressions, the last of
  which must return a dungeon."
    ([& dungeons]
       (let [dungeons (filter #(not (string? %)) dungeons)
             dungeon-types (distinct (mapcat? first dungeons))
             types-and-dungeons
               (forcat [dungeon-type dungeon-types]
                 [dungeon-type
                  (vec (map (comp keyword second)
                            (filter #(dungeon-type-match dungeon-type %)
                                    dungeons)))])
             names-and-dungeons
               (forcat [dungeon dungeons]
                 (let [dungeon-name (second dungeon)
                       dungeon-types (vec-if-not-coll (first dungeon))
                       dungeon-fn (nth dungeon 2)]
                   [(keyword dungeon-name)
                    {:types dungeon-types
                     :fn dungeon-fn}]))]
         `(do
            (def +dungeon-type-map+
                 (hash-map ~@types-and-dungeons))
            (def +dungeon-map+
                 (hash-map ~@names-and-dungeons)))))))

(defdungeons

  "Simple box with spawners in the middle and a chest on the edge,
  reminiscent of the vanilla Minecraft dungeon"
  (:std
   hello-dungeon
   (fn [_ seed]
     (-> (stack (htable [(pad 7 2 19)]
                        [(spawners 5 5 5 (reseed seed 1))]
                        [(pad 6 2 19)]
                        [(prize-chest :east (reseed seed 3))])
                (pad 19 14 19))
         (surround :nether-brick)
         (surround :bedrock)
         (surround :ground)
         (add-entrance [3 1 0]
                       ["" "Hello," "Dungeon"]
                       (reseed seed 2)))))

  "An inconvenient (especially on harder levels) victory monument
  room"
  (:home
   life-in-a-glass-house
   (fn [_ seed labels]
     (-> (stack (htable [(victory-monument labels)
                         (pad 1 1 10)
                         (supply-chest (reseed seed 2))])
                (pad 10 10 10))
         (align :y :low)
         (surround :glass)
         (surround-fn (fn [x y z params & _]
                        (if (> (:pain params)
                               (srand 1 seed x y z))
                          :lava-source
                          :sandstone)))
         (surround :bedrock)
         (surround :ground)
         (add-entrance [4 -3 0]
                       ["" "Life in a Glass House"]
                       (reseed seed 1))))))

(defn get-dungeons
  "Returns a vector of keywords naming all dungeons of the given
  type"
  ([t]
     (+dungeon-type-map+ t))
  ([t1 t2 & ts]
     (vec (mapcat get-dungeons (list* t1 t2 ts)))))

(defn get-dungeon
  "Given a dungeon name (keyword), a y coordinate, a seed, and any
  extra arguments required by that dungeon, returns a dungeon"
  ([dungeon-name y seed & extra-args]
     (let [dungeon (+dungeon-map+ dungeon-name)
           f (:fn dungeon)]
       (apply f y seed extra-args))))

(defn hello-dungeon
  "Temporary replacement for the former hello-dungeon fn, for testing"
  ([y-level seed]
     (msg -10 "Deprecated hello-dungeon fn called")
     (get-dungeon :hello-dungeon y-level seed)))
