(ns mcmap.gui-util
  (:use mcmap.util)
  (:import [java.awt.event ActionListener ActionEvent
                           WindowListener WindowEvent]
           [javax.swing JButton JPanel JFrame JFormattedTextField
                        text.DefaultFormatter text.JTextComponent
                        text.Document
                        JFormattedTextField$AbstractFormatter JLabel
                        event.DocumentListener event.DocumentEvent]
           [java.awt FlowLayout Component Dimension GridBagLayout
                     GridBagConstraints Window]
           [java.text Format]))

(set! *warn-on-reflection* true)

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
         #_(windowClosed  [x] (f x w))
         (windowClosing [x] (f x w))))))

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
                   :west (GridBagConstraints/WEST)
                   :east (GridBagConstraints/EAST)))
       gbc)))

(defn panel
  "Takes a java.awt.LayoutManager and any number of JComponents and
  returns a JPanel"
  ([lm & components]
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
     (let [fr (JFrame. ^String n)]
       (.setSize fr x y)
       (.setContentPane fr c)
       (.setVisible fr true)
       (.addWindowListener fr (wl fr))
       fr)))

(defn formatted-text
  "Takes a java.text.Format or a JFormattedTextField.AbstractFormatter,
  an x-width, and a javax.swing.event.DocumentListener, and returns a
  JFormattedTextField"
  ([format x dl]
     (let [^JFormattedTextField ftf
             (cond (instance? Format format)
                     (JFormattedTextField. ^Format format)
                   (instance? JFormattedTextField$AbstractFormatter format)
                     (JFormattedTextField.
                      ^JFormattedTextField$AbstractFormatter format)
                   :else (die "bad format class: " format))
           preferred-size (.getPreferredSize ftf)
           preferred-y-size (.height preferred-size)
           new-size (Dimension. x preferred-y-size)]
       (.setPreferredSize ftf new-size)
       (.addDocumentListener (.getDocument ftf)
                             (dl ftf))
       ftf)))

(defn live-formatter
  "Takes a Class, which must have a one-String-argument constructor
  and an inverse toString method, and returns a formatter that updates
  values with each keystroke that leads to a valid format"
  ([c]
     (doto (DefaultFormatter.)
       (.setValueClass c)
       (.setCommitsOnValidEdit true))))

(defn label
  "Takes a string and returns a JLabel, which is always left-justified"
  (^JLabel [s]
     (JLabel. ^String s JLabel/LEFT)))

(defn all-text-of-document
  "Takes a javax.swing.text.Document and returns its full text"
  ([^Document doc]
     (let [len (.getLength doc)]
       (.getText doc 0 len))))

(defn try-format
  "Tries to parse the given string into an object of the given class;
  returns nil if the constructor failed, or else an object of class c"
  ;; FIXME - this is slow and uses eval; there must be a better way
  ([^Class c s]
     (try (eval (list 'new (symbol (.getName c)) s))
          (catch Exception e))))

(defn form
  "Takes a string to use for the window name, a string to use as a
  label for a final \"submit\" button, x and y sizes, and arguments
  describing a form for the user to fill out, presents the user with a
  window, and returns the values filled out in a hash"
  ([window-name submit-name x y & args]
     (let [state (atom {})
           finished-flag (promise)
           ;; components is a seq of Component/GridBagConstraint pairs
           components (reduce
                       (fn [ [row components]
                             [output-key spec]]
                         (let [new-components
                               (case (:type spec)
                                     :entry
                                     [ [ (label (:label spec))
                                         (grid-bag-pos 0 row 1 :east)]
                                       [ (formatted-text
                                          (live-formatter (:class spec))
                                          (:width spec)
                                          (document-listener [de ftf]
                                            (swap! state
                                                   assoc output-key
                                                   (.getValue ftf))))
                                         (grid-bag-pos 1 row 1 :west)]]
                                     :live-label-entry
                                     (let [ol (label "")]
                                       [ [ (label (:label spec))
                                           (grid-bag-pos 0 row 1 :east)]
                                         [ (formatted-text
                                            (live-formatter (:class spec))
                                            (:width spec)
                                            (document-listener [de ftf]
                                              (swap! state
                                                     assoc output-key
                                                     (.getValue ftf))
                                              (.setText
                                               ol
                                               ( (:live-text spec)
                                                 (or (try-format
                                                      (:class spec)
                                                      (all-text-of-document
                                                       (.getDocument de)))
                                                     (.getValue ftf))))))
                                           (grid-bag-pos 1 row 1 :west)]
                                         [ol (grid-bag-pos 1 (inc row)
                                                           1 :west)]]))
                               rows-used
                               (case (:type spec)
                                     :entry 1
                                     :live-label-entry 2)]
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
                                   (deliver finished-flag true)))
           _ (.addLayoutComponent gbl submit-button
                                  (grid-bag-pos 0 (count components)
                                                2 :east))
           window (frame window-name x y
                         (apply panel gbl (concat (map first components)
                                                  [submit-button]))
                         (window-close-listener [we w]
                           (.dispose w)
                           (deliver finished-flag false)))]
       (when @finished-flag
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

