(ns unrest.gui
  (:use [unrest util gui-util quest-map srand])
  (:import [java.io PipedReader PipedWriter BufferedReader]
           [javax.swing ProgressMonitor JOptionPane])
  (:gen-class))

(set! *warn-on-reflection* true)

(def +old-seed-min+ (- (bit-shift-left 1 (dec +seed-bits+))))

(def +unrest-version+ (System/getProperty "unrest.version"))

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

(memo defn root-save-dir
  ([]
     (case (host-os)
           :linux   (str (System/getenv "HOME")
                         "/.minecraft/saves")
           :mac     (str (System/getenv "HOME")
                         "/Library/Application Support/minecraft/saves")
           :windows (str (System/getenv "APPDATA")
                         "/.minecraft/saves")
           :misc    (str (or (System/getenv "HOME") ".")
                         (System/getProperty "file.separator")
                         "unrest-maps"))))

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
       (str (root-save-dir) "/" munged-name))))

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

(defn a-href
  "Takes a URL and returns an HTML \"A\" tag linking to that URL, with
  an optional second argument specifying the link text, defaulting to
  the URL"
  ([url]
     (a-href url url))
  ([url txt]
     (let [clean-url (if (.startsWith ^String url "http://")
                       url
                       (str "http://" url))]
       ;; TODO: add quoting/escaping
       (str "<a href=\"" clean-url "\">" txt "</a>"))))

(def +map-form-readme-text+
     (str "<font face=\"Verdana,Helvetica,Arial,sans-serif\">Were you
disappointed by how easy Nightmare Realms was?  Are you tired of
inventing challenges and artificial restrictions just to try to make
vanilla Minecraft hard?  Would a bucket be a game-changer right about
now?  If you answered \"yes\" to any of these questions, you have come
to the right place.

<p>The Unrest map generator makes cave-and-dungeon maps for Minecraft
that are as hard as you could possibly want them to be.  At level 100,
you will be spawn camped by cave spiders, ghasts, and blazes, and you
won't find a single wood sword or pair of leather boots on the map.
At level 0, the entire cavern system is lit with glowstone, and every
dungeon is overflowing with armor, food, and weapons.  In between,
there is sure to be just the right difficulty level (decimals are
allowed) to stretch your melee skills and stomach for zombie flesh to
the limit.

<p>In one chest, somewhere on the map, there is a record.  The goal:
find the record and remove it from its dungeon, or else lock that
dungeon down so that no mobs can spawn.

<p>The rules: don't leave the map boundaries and enter vanilla
Minecraft terrain, and remember: creeper records are piracy.  (You
wouldn't download a minecart.)

<p>Choose your parameters and click the \"generate map\" button, and
then run Minecraft, where you should find the newly generated map at
the top of your single-player game list.

<p>WARNING: As with any external map generator or editor, you
<b>MUST</b> exit Minecraft completely (not just exit to the title
screen, but actually quit the program) if you are going to overwrite
or modify an existing save game, or else that save game will become
corrupted.

<p>Note that the Unrest map generator will use all your cores, so you
may hear your CPU fans do things you've never heard them do before.
This is normal.

<p>The Unrest map generator is distributed under the Eclipse Public
License, version 1.0: " (a-href "www.eclipse.org/legal/epl-v10.html")
". Source code is on github: " (a-href "https://github.com/") " Share,
hack, and enjoy.

<p><center>Dedicated to sweetjuices.</center>
<br><br>
</font>"))

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
       (form (str "Unrest map generator v" +unrest-version+)
             "generate map"
             700 450
             nil {:type :readme
                  :label "README"
                  :title (str "Unrest map generator " +unrest-version+
                              " README")
                  :text +map-form-readme-text+}
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
                     :width 100
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
             :size {:label "Map size (gameplay time)"
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
          "Adding crisp bedrock crust ..." (/ 133.0 280)
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
          "Adding crisp bedrock crust ..." (/ 45 138.2)
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
          "Adding crisp bedrock crust ..." (/ 29.0 104)
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
     (cond (> n 90) (str "about " (int (/ (+ n 45) 60))
                         " hours")
           (> n 65) "more than an hour"
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
           bar (progress-bar "Generating Map" 100)
           start-time (System/currentTimeMillis)
           orig-out *out*
           main-mapgen-thread (Thread/currentThread)
           finished (atom false)]
       (in-async-thread
        (loop []
          (Thread/sleep 1000)
          (when (.isCanceled bar)
            (.stop main-mapgen-thread))
          (when-not @finished
            (recur))))
       (in-async-thread
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
                  (msg 0 "eta: " eta-mins " (" rough-est ")")
                  ;; XXX Updating the progress bar with new text too
                  ;; soon after it is created sometimes causes the
                  ;; initial whitepace text, used for sizing the
                  ;; window, never to render, causing the window to be
                  ;; too narrow.  A hardcoded 200 ms is not ideal; on
                  ;; a very slow or overloaded computer, that might
                  ;; not be enough time to render the initial window.
                  ;; On a very fast computer, it might theoretically
                  ;; be enough time for the pipe buffer to fill up.
                  (when (< elapsed 200)
                    (Thread/sleep (int (- 200 elapsed))))
                  (in-swing-thread
                   (.setNote bar (str "Time remaining: " rough-est
                                      "                "))
                   (.setProgress bar (int (* 1e6 progress))))))
              (recur)))))
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

(defn unrest-map-generator
  ([]
     (if-let [form-data (map-form)]
       (let [gen-fn record-quest-map
             game-seed (numericize-seed (:game-seed form-data))
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
                   :map-height map-height
                   :minecraft-seed game-seed}]
         (with-mapgen-progress-bar recalc-progress
           (gen-fn game-seed
                   (numericize-seed (:cave-seed form-data))
                   (:level form-data)
                   (save-name->dir (:save-name form-data))
                   opts))
         (JOptionPane/showMessageDialog nil "Map generated successfully")))))

(defn -main
  ([& args]
     (let [t (Thread. ^Runnable unrest-map-generator)]
       (.start t)
       (.join t))
     (System/exit 0)))
