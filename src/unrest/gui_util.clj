(ns unrest.gui-util
  (:use unrest.util)
  (:import [java.awt.event ActionListener ActionEvent
                           WindowListener WindowEvent]
           [javax.swing JButton JPanel JFrame JFormattedTextField
                        text.DefaultFormatter text.JTextComponent
                        text.Document
                        JFormattedTextField$AbstractFormatter
                        JFormattedTextField$AbstractFormatterFactory
                        JLabel JSeparator JCheckBox JComboBox
                        event.DocumentListener event.DocumentEvent
                        SwingUtilities ProgressMonitor
                        JEditorPane JScrollPane
                        SpringLayout Spring]
           [javax.swing.event HyperlinkListener HyperlinkEvent
                              HyperlinkEvent$EventType]
           [java.awt FlowLayout Component Dimension GridBagLayout
                     GridBagConstraints Window]
           [java.text Format]
           [java.net URL]))

;;; Plan: hit Swing and AWT with a hammer until they start to look
;;; sort of halfway functional

(set! *warn-on-reflection* true)

(def ^:dynamic *in-swing-thread* false)

(defmacro in-swing-thread
  "Evaluates the given forms in the Swing event dispatching thread and
  returns the result; catches exceptions from the Swing thread and
  rethrows them in the current thread

  See
  http://docs.oracle.com/javase/6/docs/api/javax/swing/package-summary.html#threading
  for rationale"
  ([& forms]
     `(if *in-swing-thread*
        (do ~@forms)
        (let [result# (promise)]
          (SwingUtilities/invokeLater
           #(try
              (binding [*in-swing-thread* true]
                (let [r# (do ~@forms)]
                  (deliver result# {:return r#})))
              (catch Exception e#
                (deliver result# {:exception e#}))))
          (when-let [e# (:exception @result#)]
            (throw e#))
          (:return @result#)))))

(defn action-listener-fn
  "Takes a fn of one argument (a java.awt.event.ActionEvent) and
  returns a java.awt.event.ActionListener with that fn as its
  actionPerformed method"
  ([f]
     (proxy [Object ActionListener]
         []
       (actionPerformed [x] (f x)))))

(defmacro action-listener
  "Takes a one-variable binding vector and code, and returns a
  java.awt.event.ActionListener"
  ([bindings & body]
     (when-not (and (vector? bindings)
                    (= 1 (count bindings)))
       (die "action-listener requires a binding vector with a single"
            " variable"))
     (let [ [varname] bindings]
       `(action-listener-fn (fn [~(with-meta varname
                                    {:tag `ActionEvent})]
                              ~@body)))))

(defn document-listener-fn
  "Takes a fn of two arguments (a javax.swing.event.DocumentEvent and
  a javax.swing.JFormattedTextField) and returns a fn of one
  argument (the JFormattedTextField) that will return a
  javax.swing.event.DocumentListener with the given fn as its method
  for insertUpdate, removeUpdate, and changedUpdate"
  ([f]
     (fn [ftf]
       (proxy [Object DocumentListener]
           []
         (insertUpdate  [x] (f x ftf))
         (removeUpdate  [x] (f x ftf))
         (changedUpdate [x] (f x ftf))))))

(defmacro document-listener
  "Takes a binding vector for a DocumentEvent and the
  JFormattedTextField on which the event occurred, and code, and
  returns a javax.swing.event.DocumentListener that evaluates the code
  for every change to the JFormattedTextField"
  ([bindings & body]
     (when-not (and (vector? bindings)
                    (= 2 (count bindings)))
       (die "document-listener requires a binding vector with two"
            " variables"))
     (let [ [de-varname ftf-varname] bindings]
       `(document-listener-fn (fn [~(with-meta de-varname
                                      {:tag `DocumentEvent})
                                   ~(with-meta ftf-varname
                                      {:tag `JFormattedTextField})]
                                ~@body)))))

(defn window-close-listener-fn
  "Takes a fn of two arguments (a java.awt.event.WindowEvent and a
  java.awt.Window) and returns a fn of one argument (the
  java.awt.Window) that will return a java.awt.event.WindowListener
  with the given fn as its method for windowClosing"
  ([f]
     (fn [w]
       (proxy [Object WindowListener]
           []
         (windowClosing [x] (f x w))
         (windowOpened [_])
         (windowActivated [_])
         (windowDeactivated [_])
         (windowClosed [_])))))

(defmacro window-close-listener
  "Takes a one-variable binding vector and code, and returns a
  java.awt.event.WindowListener that evaluates the given code when the
  window is closed by the user (not by having .dispose called on it)"
  ([bindings & body]
     (when-not (and (vector? bindings)
                    (= 2 (count bindings)))
       (die "window-close-listener requires a binding vector with two"
            " variables"))
     (let [ [we-varname w-varname] bindings]
       `(window-close-listener-fn (fn [~(with-meta we-varname
                                          {:tag `WindowEvent})
                                       ~(with-meta w-varname
                                          {:tag `Window})]
                                    ~@body)))))

(defn button
  "Takes a label and a java.awt.event.ActionListener and returns a
  JButton"
  (^JButton [l al]
     (doto (JButton. ^String l)
       (.addActionListener al))))

(defn checkbox
  "Takes a fn of one argument that will be called with the pressed
  state (true/false) any time it changes, and an initial pressed
  state, and returns a JCheckBox"
  ([action-fn pressed?]
     (let [cb (JCheckBox.)]
       (action-fn (#'boolean pressed?))
       (.setSelected cb (#'boolean pressed?))
       (.addActionListener cb
                           (action-listener [_]
                             (action-fn (.isSelected cb))))
       cb)))

(defn combo-box
  "Takes choices in the form of a vector of an even number of elements
  (strings to show in the combo-box alternating with values to
  return), a fn of one argument that will be called with the selected
  value any time it changes, and an initial selection (which must be
  present in the given map), and returns a JComboBox"
  ([choices action-fn init-str]
     (let [cb (JComboBox.)
           strs (take-nth 2 choices)
           str->val (apply hash-map choices)]
       (doseq [s strs]
         (.addItem cb s))
       (.addActionListener cb
                           (action-listener [_]
                             (action-fn (str->val (.getSelectedItem cb)))))
       (.setSelectedItem cb init-str)
       cb)))

(defn flow-layout
  "Takes a keyword (:left, :right, :center, :leading, or :trailing),
  and returns the corresponding java.awt.FlowLayout"
  ([align]
     (FlowLayout. ( {:left     FlowLayout/LEFT,
                     :right    FlowLayout/RIGHT,
                     :center   FlowLayout/CENTER,
                     :leading  FlowLayout/LEADING,
                     :trailing FlowLayout/TRAILING}
                    align))))

(defn grid-bag-pos
  "Takes x and y cell coordinates, a number of columns, and an anchor
  position (:west or :east), and returns a GridBagConstraints
  for that position in a GridBagLayout"
  ([x y cols anchor]
     (let [gbc (GridBagConstraints.)]
       (set! (.gridx gbc) x)
       (set! (.gridy gbc) y)
       (set! (.gridwidth gbc) cols)
       (set! (.anchor gbc)
             (case anchor
                   :west   (GridBagConstraints/WEST)
                   :center (GridBagConstraints/CENTER)
                   :east   (GridBagConstraints/EAST)))
       gbc)))

(defn panel
  "Takes a java.awt.LayoutManager and any number of JComponents and
  returns a JPanel"
  (^JPanel [lm & components]
     (let [p (JPanel.)]
       (.setLayout p lm)
       (doseq [c components]
         (.add p ^Component c))
       p)))

(defn frame
  "Takes a name, x size, y size, a JPanel (or other component,
  preferably opaque), and a java.awt.event.WindowListener, and returns
  a JFrame"
  (^JFrame [n x y c wl]
     (in-swing-thread
      (let [fr (JFrame. ^String n)]
        (.setContentPane fr c)
        (let [preferred-size (.getPreferredSize fr)
              x (or x (.width  preferred-size))
              y (or y (.height preferred-size))]
          (.setSize fr x y))
        (.setVisible fr true)
        (when wl
          (.addWindowListener fr (wl fr)))
        fr))))

(defn progress-bar
  "Takes a title and a width (measured in spaces) to allocate for a
  note field, and returns a new ProgressMonitor, with minimum 0 and
  maximum 1e6"
  (^ProgressMonitor [title width]
     (doto (ProgressMonitor. nil title
                             (apply str (repeat width \space))
                             0 (int 1e6))
       (.setMillisToDecideToPopup 0)
       (.setMillisToPopup 0)
       (.setProgress 1))))

(defn formatted-text
  "Takes a java.text.Format or a JFormattedTextField.AbstractFormatter,
  an x-width, a javax.swing.event.DocumentListener, and an optional
  initial value, and returns a JFormattedTextField"
  ([format x dl]
     (formatted-text x dl nil))
  ([format x dl init-val]
     (let [^JFormattedTextField ftf
             (cond (instance? Format format)
                     (JFormattedTextField. ^Format format)
                   (instance? JFormattedTextField$AbstractFormatter format)
                     (JFormattedTextField.
                      ^JFormattedTextField$AbstractFormatter format)
                   (instance? JFormattedTextField$AbstractFormatterFactory
                              format)
                     (JFormattedTextField.
                      ^JFormattedTextField$AbstractFormatterFactory format)
                   :else (die "bad format class: " format))
           preferred-size (.getPreferredSize ftf)
           preferred-y-size (.height preferred-size)
           new-size (Dimension. x preferred-y-size)]
       (.setPreferredSize ftf new-size)
       (.setMinimumSize   ftf new-size)
       (.addDocumentListener (.getDocument ftf)
                             (dl ftf))
       (when init-val
         (.setValue ftf init-val))
       ftf)))

(defn live-formatter
  "Takes a Class, which must have a one-String-argument constructor
  and an inverse toString method, and returns a formatter that updates
  values with each keystroke that leads to a valid format"
  ([c]
     (doto (DefaultFormatter.)
       (.setValueClass c)
       (.setCommitsOnValidEdit true)
       (.setOverwriteMode false))))

(defn validated-live-formatter-factory
  "Takes a Class, which must have a one-String-argument constructor
  and an inverse toString method, a fn that returns a boolean value
  indicating whether the passed-in string-constructed value is valid,
  and an optional fn that takes a value and returns a
  possibly-different value to use, and returns a formatter factory
  returning formatters that update values with each keystroke that
  leads to a valid format"
  ([c v-fn]
     (validated-live-formatter-factory c v-fn nil))
  ([c v-fn mod-fn]
     (proxy [JFormattedTextField$AbstractFormatterFactory]
         []
       (getFormatter [tf]
          (doto (proxy [DefaultFormatter]
                    []
                  (stringToValue [s]
                     (let [this ^DefaultFormatter this
                           v (proxy-super stringToValue s)]
                       (if (or (not v-fn)
                               (v-fn v))
                         (if mod-fn
                           (mod-fn v)
                           v)
                         (do
                           (throw (java.text.ParseException.
                                   "Value out of range"
                                   0)))))))
            (.setValueClass c)
            (.setCommitsOnValidEdit true)
            (.setOverwriteMode false))))))

(defn label
  "Takes a string and returns a JLabel, which is always left-justified"
  (^JLabel [s]
     (JLabel. ^String s JLabel/LEFT)))

(defn horizontal-line
  "Takes a length and returns a JSeparator"
  (^JSeparator [l]
     (let [sep (JSeparator.)
           h (.height (.getPreferredSize sep))]
       (.setSize sep l h)
       (.setPreferredSize sep (Dimension. l h))
       sep)))

(defn spacer
  "Takes width and height and returns a blank component of those
  dimensions"
  ([x y]
     (doto (panel (flow-layout :left))
       (.setSize x y)
       (.setPreferredSize (Dimension. x y)))))

(defn all-text-of-document
  "Takes a javax.swing.text.Document and returns its full text"
  ([^Document doc]
     (let [len (.getLength doc)]
       (.getText doc 0 len))))

(defn try-format
  "Tries to parse the given string into an object of the given class;
  returns nil if the constructor failed, or else an object of class c"
  ([^Class c s]
     ;; In case the clojure.lang.Compiler hack breaks, use the eval,
     ;; which is equivalent but approximately 1000 times slower.
     (try (.eval (clojure.lang.Compiler$NewExpr. c
                    [(clojure.lang.Compiler/analyze nil s)] 0))
          #_(eval (list 'new (symbol (.getName c))
                        s))
          (catch Exception e))))

(defn ftf-with-buttons
  "Takes a JFormattedTextField and a field spec that may or may not
  have :buttons, and returns a JPanel with the JFormattedTextField and
  JButtons"
  ([^JFormattedTextField ftf spec]
     (let [buttons (map (fn [b]
                          (button
                           (:label b)
                           (action-listener [ae]
                             (let [new-val ( (:swap-val b)
                                             (.getValue ftf))]
                               (.setValue ftf new-val)))))
                        (:buttons spec))]
       (apply panel (flow-layout :left)
              ftf buttons))))

(defn text-display
  "Takes a string, dimensions, and a
  javax.swing.event.HyperlinkListener, and returns a scrollable
  text-display component"
  ([^String text x y hl]
     (let [textarea (JEditorPane. "text/html" text)
           scrollpane (JScrollPane. textarea)]
       (doto textarea
         (.setEditable false)
         (.addHyperlinkListener hl))
       (doto scrollpane
         (.setPreferredSize (Dimension. x y)))
       scrollpane)))

(defn dynamic-get-service-object
  "Returns either an object implementing the javax.jnlp.BasicService
  interface (with its showDocument method -- the one we care about),
  or nil if the platform (i.e., Mac OS X) does not provide one, in
  which case the \"open\" command must be used to launch a browser
  instead"
  ([]
     (try
       (let [srv-mgr-class (Class/forName "javax.jnlp.ServiceManager")
             lookup-method (.getMethod srv-mgr-class "lookup"
                                       (into-array [String]))]
         (.invoke lookup-method
                  nil
                  (to-array ["javax.jnlp.BasicService"])))
       (catch Exception e
         nil))))

(def service-object (dynamic-get-service-object))

(defn open-url
  "Opens the given URL using the system default browser; returns nil"
  ([url]
     (do
       (if service-object
         (no-warn-reflection (.showDocument service-object url))
         (run-cmd "open" url))
       nil)))

(defn hyperlink-listener-fn
  "Takes a fn of one argument (a URL), and returns a
  javax.swing.event.HyperlinkListener that calls the fn for each
  ACTIVATED HyperlinkEvent it receives"
  ([f]
     (proxy [Object HyperlinkListener]
         []
       (hyperlinkUpdate [^HyperlinkEvent e]
                        (when (= HyperlinkEvent$EventType/ACTIVATED
                                 (.getEventType e))
                          (let [u (.getURL e)]
                            (f u)))))))

(defmacro hyperlink-listener
  "Takes a one-variable binding vector (for a URL variable) and code,
  and returns a javax.swing.event.HyperlinkListener that evaluates the
  code for each ACTIVATED HyperlinkEvent it receives"
  ([bindings & body]
     (when-not (and (vector? bindings)
                    (= 1 (count bindings)))
       (die "hyperlink-listener requires a binding vector with a"
            " single variable"))
     (let [ [varname] bindings]
       `(hyperlink-listener-fn (fn [~varname] ~@body)))))

(defn readme
  "Takes two strings and displays a window with the first string as
  its title and the second string as text, and an OK button that
  returns from the function"
  ([title text]
     (let [ok? (promise)
           sl (SpringLayout.)
           td (text-display text 495 435
                            (hyperlink-listener [u]
                              (open-url u)))
           bt (panel (flow-layout :center)
                     (button "OK"
                             (action-listener [ae]
                               (deliver ok? true))))
           p (panel sl td bt)
           _ (do
               (doseq [ [^String dir offset]
                        [[SpringLayout/NORTH  18]
                         [SpringLayout/EAST  -18]
                         [SpringLayout/WEST   18]]]
                 (.putConstraint sl
                                 dir ^Component td
                                 (Spring/constant offset)
                                 dir ^Component p))
               (doseq [ [^String dir offset]
                        [[SpringLayout/SOUTH -8]
                         [SpringLayout/EAST  -18]
                         [SpringLayout/WEST   18]]]
                 (.putConstraint sl
                                 dir ^Component bt
                                 (Spring/constant offset)
                                 dir ^Component p))
               (.putConstraint sl
                               SpringLayout/SOUTH ^Component td
                               (Spring/constant -6)
                               SpringLayout/NORTH ^Component bt))
           window (frame title 520 500
                         p
                         (window-close-listener [_ w]
                           (.dispose w)
                           (deliver ok? false)))]
       (when @ok?
         (.dispose window)))))

(defn form
  "Takes a string to use for the window name, a string to use as a
  label for a final \"submit\" button, x and y sizes, and arguments
  describing a form for the user to fill out, presents the user with a
  window, and returns the values filled out in a hash"
  ([window-name submit-name x y & args]
     (let [state (atom {})
           submitted? (promise)
           ;; components is a seq of Component/GridBagConstraint pairs
           components
           (reduce
            (fn [ [row components]
                  [output-key spec]]
              (let [new-components
                    (case (:type spec)
                          :entry
                          [ [ (label (:label spec))
                              (grid-bag-pos 0 row 1 :east)]
                          (let [ftf
                                (formatted-text
                                 (validated-live-formatter-factory
                                  (:class spec)
                                  (:validator spec)
                                  (:modifier spec))
                                 (:width spec)
                                 (document-listener [de ftf]
                                   (if-let* [v (or (try-format
                                                    (:class spec)
                                                    (all-text-of-document
                                                     (.getDocument de)))
                                                   (.getValue ftf))
                                             _ (or (not (:validator spec))
                                                   ( (:validator spec) v))]
                                     (swap! state assoc
                                            output-key v)))
                                 (:default spec))]
                              [ (ftf-with-buttons ftf spec)
                                (grid-bag-pos 1 row 1 :west)])]
                          :live-label-entry
                          (let [ol (label " ")
                                ftf
                                (formatted-text
                                 (validated-live-formatter-factory
                                    (:class spec)
                                    (:validator spec)
                                    (:modifier spec))
                                 (:width spec)
                                 (document-listener [de ftf]
                                   (if-let* [v (or (try-format
                                                    (:class spec)
                                                    (all-text-of-document
                                                     (.getDocument de)))
                                                   (.getValue ftf))
                                             _ (or (not (:validator spec))
                                                   ( (:validator spec) v))
                                             _ (or (swap! state assoc
                                                          output-key v)
                                                   true)
                                             t ( (:live-text spec)
                                                 v)]
                                     (.setText ol t)))
                                 (:default spec))]
                            [ [ (label (:label spec))
                                (grid-bag-pos 0 row 1 :east)]
                              [ (ftf-with-buttons ftf spec)
                                (grid-bag-pos 1 row 1 :west)]
                              [ol (grid-bag-pos 1 (inc row)
                                                1 :west)]])
                          :label
                          [ [ (label (:label spec))
                              (grid-bag-pos 0 row 2 :west)]]
                          :horizontal-line
                          [ [ (horizontal-line (:width spec))
                              (grid-bag-pos 0 row 2 :center)]]
                          :space
                          [ [ (spacer (:width spec) (:height spec))
                              (grid-bag-pos 0 row 2 :center)]]
                          :checkbox
                          [ [ (label (:label spec))
                              (grid-bag-pos 0 row 1 :east)]
                            [ (checkbox (fn [pressed]
                                          (swap! state assoc output-key
                                                 pressed))
                                        (:default spec))
                              (grid-bag-pos 1 row 1 :west)]]
                          :combo-box
                          [ [ (label (:label spec))
                              (grid-bag-pos 0 row 1 :east)]
                            [ (combo-box (:choices spec)
                                         (fn [selected]
                                           (swap! state assoc output-key
                                                  selected))
                                         (:default spec))
                              (grid-bag-pos 1 row 1 :west)]]
                          :readme
                          [ [ (button (:label spec)
                                      (action-listener [ae]
                                        (send-off (agent nil)
                                          (fn [_]
                                            (readme (:title spec)
                                                    (:text spec))))))
                              (grid-bag-pos 0 row 2 :center)]])
                    rows-used
                    (case (:type spec)
                          :live-label-entry 2
                          1)]
                [ (+ row rows-used)
                  (reduce conj components new-components)]))
            [0 []]
            (partition 2 args))
           components (second components)
           gbl (GridBagLayout.)
           _ (doseq [ [component constraint] components]
               (.addLayoutComponent gbl ^Component component constraint))
           submit-button (button submit-name
                                 (action-listener [ae]
                                   (deliver submitted? true)))
           _ (.addLayoutComponent gbl submit-button
                                  (grid-bag-pos 0 (inc (count components))
                                                2 :east))
           window (frame window-name x y
                         (apply panel gbl (concat (map first components)
                                                  [submit-button]))
                         (window-close-listener [_ w]
                           (.dispose w)
                           (deliver submitted? false)))]
       (when @submitted?
         (.dispose window)
         @state))))

(defn form-exercise-1
  ([]
     (form "form test 1"
           "do the thing"
           700 400
           :magic {:label "Quantity of magic to use"
                   :type :entry
                   :class Double
                   :width 250}
           :more-magic {:label "Quantity of extra magic to use"
                        :type :entry
                        :class Double
                        :width 250})))

(defn form-exercise-2
  ([]
     (form "form test 1"
           "do the thing"
           700 400
           :magic {:label "Quantity of magic to use"
                   :type :entry
                   :class Double
                   :width 250}
           :more-magic {:label "Quantity of extra magic to use"
                        :type :live-label-entry
                        :class Double
                        :width 250
                        :live-text (fn [v]
                                     (str v " is " (/ v 2) "x2"))})))

(defn form-exercise-3
  ([]
     (form "form test 1"
           "do the thing"
           700 400
           :game-seed {:label "Game seed (text or number)"
                       :type :live-label-entry
                       :class Long
                       :buttons [{:label "randomize"
                                  :swap-val (fn [_]
                                              (-> (bit-shift-left
                                                   1 48)
                                                  rand long))}]
                       :live-text (fn [v]
                                    (str "Numeric value: " v))
                       :width 250}
           :cave-seed {:label "Cavern seed (text or number)"
                       :type :live-label-entry
                       :class Long
                       :buttons [{:label "randomize"
                                  :swap-val (fn [_]
                                              (-> (bit-shift-left
                                                   1 48)
                                                  rand long))}]
                       :live-text (fn [v]
                                    (str "Numeric value: " v))
                       :width 250}
           nil {:type :horizontal-line
                :width 400}
           nil {:label (str "Recommended level range: 15-20 for"
                            " hardcore mode, 18-25 for non-hardcore mode")
                :type :label}
           :level {:label "Level (0-100)"
                   :type :live-label-entry
                   :class Double
                   :width 250
                   :validator #(<= 0 % 100)
                   :live-text (fn [v]
                                (cond (or (> v 100)
                                          (< v 0))
                                        nil
                                      (<= v 10)
                                        (str "Note: Level " v
                                             " is extremely easy")
                                      (>= v 75)
                                        (str "Note: Level " v
                                             " is unplayably hard")
                                      (>= v 50)
                                        (str "Note: Level " v
                                             " is extremely hard")
                                      :else
                                        " "))}
           :hardcore {:label "Hardcore mode"
                      :type :checkbox}
           :creative {:label "Creative mode"
                      :type :checkbox}
           nil {:type :space
                :width 1
                :height 30})))

(defn readme-exercise-1
  ([]
     (readme "counting to 1000"
             (apply str
                    "<html>"
                    (concat (interpose " " (range 1000))
                            ["</html>"])))))
