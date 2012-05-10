(ns mcmap.gui
  (:use [mcmap util gui-util unrest srand])
  (:import [java.io PipedReader PipedWriter BufferedReader]
           [javax.swing ProgressMonitor JOptionPane])
  (:gen-class))

(set! *warn-on-reflection* true)

(def +old-seed-min+ (- (bit-shift-left 1 (dec +seed-bits+))))

(defn numericize-seed
  "Takes a string and returns it as a long if it is formatted as an
  integer between the lowest (deprecated) negative signed seed and
  +seed-max+, or else converts it using make-seed"
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

(defn recalc-progress-normal
  "Takes a line of output as produced by request-quest-map for a
  normal-sized map and returns the fraction, from 0.0 to 1.0, of the
  way through map generation that the process is when it outputs that
  line"
  ([s]
     (let [trimmed-line (second (re-find #" - (.*)" s))]
       ( {"Carving ..." (/ 0.3 280)
          "Finding dungeons ..." (/ 19.0 280)
          "Placing dungeons and hallways ..." (/ 65.0 280)
          "Adding crisp bedrock crust ..." (/ 107.0 280)
          "Adding creamy middle ..." (/ 143.0 280)
          "Counting spawners ..." (/ 209.0 280)
          "Extracting chunks for Anvil region 0.0 ..." (/ 215.0 280)
          "Extracting chunks for Anvil region 0.-1 ..." (/ 255.0 280)
          "Extracting chunks for Anvil region -1.0 ..." (/ 266.0 280)
          "Extracting chunks for Anvil region -1.-1 ..." (/ 276.0 280)}
         trimmed-line))))

(defn recalc-progress-small
  "Takes a line of output as produced by request-quest-map for a small
  map and returns the fraction, from 0.0 to 1.0, of the way through
  map generation that the process is when it outputs that line"
  ([s]
     (let [trimmed-line (second (re-find #" - (.*)" s))]
       ( {"Carving ..." (/ 0.24 138.2)
          "Finding dungeons ..." (/ 3.8 138.2)
          "Placing dungeons and hallways ..." (/ 12.7 138.2)
          "Adding crisp bedrock crust ..." (/ 34.3 138.2)
          "Adding creamy middle ..." (/ 50.0 138.2)
          "Counting spawners ..." (/ 78.9 138.2)
          "Extracting chunks for Anvil region 0.0 ..." (/ 84.8 138.2)
          "Extracting chunks for Anvil region 0.-1 ..." (/ 112.7 138.2)
          "Extracting chunks for Anvil region -1.0 ..." (/ 123.8 138.2)
          "Extracting chunks for Anvil region -1.-1 ..." (/ 134.8 138.2)}
         trimmed-line))))

(defn recalc-progress-tiny
  "Takes a line of output as produced by request-quest-map for a tiny
  map and returns the fraction, from 0.0 to 1.0, of the way through
  map generation that the process is when it outputs that line"
  ([s]
     (let [trimmed-line (second (re-find #" - (.*)" s))]
       ( {"Carving ..." (/ 0.13 104)
          "Finding dungeons ..." (/ 2.5 104)
          "Placing dungeons and hallways ..." (/ 12.0 104)
          "Adding crisp bedrock crust ..." (/ 22.9 104)
          "Adding creamy middle ..." (/ 31.4 104)
          "Counting spawners ..." (/ 46.0 104)
          "Extracting chunks for Anvil region 0.0 ..." (/ 52.0 104)
          "Extracting chunks for Anvil region 0.-1 ..." (/ 77.0 104)
          "Extracting chunks for Anvil region -1.0 ..." (/ 88.5 104)
          "Extracting chunks for Anvil region -1.-1 ..." (/ 100.1 104)}
         trimmed-line))))

(defn rough-time
  "Takes a number of minutes and returns an English phrase
  approximating that amount of time"
  ([n]
     (cond (> n 65) "more than an hour"
           (> n 50) "about an hour"
           (> n 15) (str "about " (* 5 (inc (int (/ n 5))))
                         " minutes")
           (> n 2.25) (str "about " (inc (int n))
                         " minutes")
           (> n 1.75) "about 2 minutes"
           (> n 1.25) "less than 2 minutes"
           (> n 0.85) "about a minute"
           (> n 0.6)  "less than a minute"
           (> n 0.3)  "about 30 seconds"
           (> n 0.2)  "about 15 seconds"
           (> n 0.1)  "about ten seconds"
           :else      "a few seconds")))

(defn mapgen-progress-bar-fn
  ([recalc-progress f]
     (let [rdr (PipedReader.)
           pipe (PipedWriter. rdr)
           line-rdr (BufferedReader. rdr)
           bar (progress-bar "Generating Map" 150)
           start-time (System/currentTimeMillis)
           orig-out *out*
           main-mapgen-thread (Thread/currentThread)
           finished (atom false)]
       (send-off (agent nil)
                 (fn [_]
                   (loop []
                     (Thread/sleep 1000)
                     (when (.isCanceled bar)
                       (.stop main-mapgen-thread))
                     (when-not @finished
                       (recur)))))
       (send-off (agent nil)
                 (fn [_]
                   (loop []
                     (let [line (.readLine line-rdr)]
                       (when line
                         (binding [*out* orig-out]
                           (println line))
                         (if-let [progress (recalc-progress line)]
                           (let [cur-time (System/currentTimeMillis)
                                 elapsed (- cur-time start-time)
                                 est-total (/ elapsed progress)
                                 eta-mins (/ (- est-total elapsed)
                                             60000)
                                 rough-est (rough-time eta-mins)]
                             (msg 0 "eta: " eta-mins " (" rough-est
                                  ")")
                             (when (< elapsed 200)
                               (Thread/sleep (int (- 200 elapsed))))
                             (in-swing-thread
                              (.setNote bar
                                        (str "Time remaining: "
                                             rough-est
                                             "                "))
                              (.setProgress bar (int (* 1e6 progress))))))
                         (recur))))))
       (binding [*out* pipe]
         (try
           (f)
           (finally (.close bar)
                    (.close pipe)
                    (.close rdr)
                    (swap! finished (fn [_] true))))))))

(defmacro with-mapgen-progress-bar
  "Evaluates the given forms with *out* passing through a parser that
  finds signs of map-generation progress, estimates the time
  remaining, and provides a cancel button that immediately exits the
  application"
  ([recalc-progress & forms]
     `(mapgen-progress-bar-fn
       ~recalc-progress
       (fn []
         (do ~@forms)))))

(defn -main
  ([& args]
     (if-let [form-data (map-form)]
       (let [gen-fn record-quest-map
             [n-caves n-dungeons map-side map-height recalc-progress]
               ( {:normal [15 64 256 128 recalc-progress-normal]
                  :small  [6  25 192  96 recalc-progress-small]
                  :tiny   [3  13 160  70 recalc-progress-tiny]}
                 (:size form-data))
             opts {:level-name (:save-name form-data)
                   :creative   (:creative  form-data)
                   :hardcore   (:hardcore  form-data)
                   :n-caves    n-caves
                   :n-dungeons n-dungeons
                   :map-side   map-side
                   :map-height map-height}]
         (with-mapgen-progress-bar recalc-progress
           (gen-fn (numericize-seed (:game-seed form-data))
                   (numericize-seed (:cave-seed form-data))
                   (:level form-data)
                   (save-name->dir (:save-name form-data))
                   opts))
         (JOptionPane/showMessageDialog nil "Map generated successfully")))))

