(ns mcmap.gui
  (:use mcmap.util
        mcmap.gui-util
        mcmap.unrest
        mcmap.srand)
  (:gen-class))

(set! *warn-on-reflection* true)

(def +old-seed-min+ (- (bit-shift-left 1 (dec +seed-bits+))))

(defn numericize-seed
  "Takes a string and returns it as a long if it is formatted as an
  integer between 0 and +seed-max+, or else converts it using
  make-seed"
  ([s]
     (if-let* [n (try-format Long s)
               _ (< (dec +old-seed-min+) n +seed-max+)]
        (if (neg? n)
          (mod n +seed-max+)
          n)
        (make-seed s))))

(def +host-os+
     (let [os-name (.toLowerCase (System/getProperty "os.name"))]
       (cond (.indexOf os-name "mac os")  :mac
             (.indexOf os-name "linux")   :linux
             (.indexOf os-name "windows") :windows
             :else                        :misc)))

(def +root-save-dir+
     (case +host-os+
           :linux   (str (System/getenv "HOME")
                         "/.minecraft/saves")
           :mac     (str (System/getenv "HOME")
                         "/Library/Application Support/minecraft/saves")
           :windows (str (System/getenv "APPDATA")
                         "/.minecraft/saves")
           :misc    (str (or (System/getenv "HOME") "")
                         "/unrest-maps")))

(defn save-name->dir
  ([sn]
     (let [munged-name (-> ^String sn
                           ;; From
                           ;; http://support.microsoft.com/kb/177506,
                           ;; it looks like Windows' forbidden
                           ;; filename character list is a superset of
                           ;; Linux/BSD's
                           (.replaceAll "\\."  "_")
                           (.replaceAll "/"    "_")
                           (.replaceAll "\\\\" "_")
                           (.replaceAll ":"    "_")
                           (.replaceAll "\\?"  "!")
                           (.replaceAll "\""   "'")
                           (.replaceAll "\\<"  "[")
                           (.replaceAll "\\>"  "]")
                           (.replaceAll "\\|"  "!"))]
       (str +root-save-dir+ "/" munged-name))))

(defn save-game-exists
  ([sn]
     (dir-exists (save-name->dir sn))))

(defn pick-new-save-game
  "Returns a name for a save game that maps to a directory that does
  not currently exist"
  ([]
     (loop [n 1]
       (let [sg (str "Unrest map " n)]
         (if (save-game-exists sg)
           (recur (inc n))
           sg)))))

(defn map-form
  ([]
     (let [seed-entry {:type :live-label-entry
                       :class String
                       :buttons [{:label "randomize"
                                  :swap-val (fn [_]
                                              (str (choose-random-seed)))}]
                       :live-text #(str "Numeric value: "
                                        (numericize-seed %))
                       :width 250}]
       (form "Unrest map generator v1.0.0"
             "generate map"
             700 450
             :game-seed (assoc seed-entry
                          :label "Game seed (text or number)"
                          :default (str (choose-random-seed)))
             :cave-seed (assoc seed-entry
                          :label "Cavern seed (text or number)"
                          :default (str (choose-random-seed)))
             nil {:type :horizontal-line
                  :width 400}
             nil {:label (str "Recommended level range: 15-20 for"
                              " hardcore mode, 18-25 for non-hardcore mode")
                  :type :label}
             :level {:label "Level (0-100)"
                     :type :live-label-entry
                     :class Double
                     :validator #(<= 0 % 100)
                     :default 23.0
                     :width 250
                     :live-text (fn [v]
                                  (cond (<= v 10)
                                          (str "Note: Level " v
                                               " is extremely easy")
                                        (>= v 75)
                                          (str "Note: Level " v
                                               " is unplayable")
                                        (>= v 45)
                                          (str "Note: Level " v
                                               " is extremely hard")
                                        :else
                                        " "))}
             :size {:label "Map size"
                    :type :combo-box
                    :choices ["Normal (2-8 hours)"   :normal
                              "Small (1-3 hours)"    :small
                              "Tiny (30-90 minutes)" :tiny]
                    :default "Normal (2-8 hours)"}
             :hardcore {:label "Hardcore mode"
                        :type :checkbox}
             :creative {:label "Creative mode"
                        :type :checkbox}
             :save-name {:label "Save game name"
                         :type :live-label-entry
                         :class String
                         :validator #(not= % "")
                         :default (pick-new-save-game)
                         :width 250
                         :live-text #(if (save-game-exists %)
                                       "Existing game will be overwritten"
                                       " ")}
             nil {:type :space
                  :width 1
                  :height 30}))))

(defmacro with-mapgen-progress-bar
  "Evaluates the given forms with *out* passing through a parser that
  finds signs of map-generation progress, estimates the time
  remaining, and provides a cancel button that immediately exits the
  application"
  ([& forms]
     ;; TODO
     `(do ~@forms)))

(defn -main
  ([& args]
     (if-let [form-data (map-form)]
       (let [gen-fn record-quest-map
             [n-caves n-dungeons map-side map-height]
               ( {:normal [15 64 256 128]
                  :small  [6  25 192  96]
                  :tiny   [3  13 160  70]}
                 (:size form-data))
             opts {:level-name (:save-name form-data)
                   :creative   (:creative  form-data)
                   :hardcore   (:hardcore  form-data)
                   :n-caves    n-caves
                   :n-dungeons n-dungeons
                   :map-side   map-side
                   :map-height map-height}]
         (with-mapgen-progress-bar
           (gen-fn (numericize-seed (:game-seed form-data))
                   (numericize-seed (:cave-seed form-data))
                   (:level form-data)
                   (save-name->dir (:save-name form-data))
                   opts))))))

