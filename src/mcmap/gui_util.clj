(ns mcmap.gui-util
  (:use mcmap.util)
  (:import [java.awt.event ActionListener ActionEvent]
           [javax.swing JButton JPanel JFrame JFormattedTextField
                        text.DefaultFormatter text.JTextComponent
                        text.Document
                        JFormattedTextField$AbstractFormatter JLabel
                        event.DocumentListener event.DocumentEvent]
           [java.awt FlowLayout Component Dimension GridBagLayout
                     GridBagConstraints]
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
  for inserUpdate, removeUpdate, and changedUpdate"
  ([f]
     (fn [ftf]
       (proxy [Object DocumentListener]
           []
         (insertUpdate  [x] (f x ftf))
         (removeUpdate  [x] (f x ftf))
         (changedUpdate [x] (f x ftf))))))

(defmacro document-listener
  "Takes a one-variable binding vector and code, and returns a
  javax.swing.event.DocumentListener"
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
  "Takes a name, x size, y size, and a JPanel (or other component,
  preferably opaque), and returns a JFrame"
  (^JFrame [n x y c]
     (doto (JFrame. ^String n)
       (.setSize x y)
       (.setContentPane c)
       (.setVisible true))))

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
  ([s]
     (JLabel. ^String s JLabel/LEFT)))

(defn all-text-of-document
  "Takes a javax.swing.text.Document and returns its full text"
  ([^Document doc]
     (let [len (.getLength doc)]
       (.getText doc 0 len))))

(defn form
  "Takes a string to use for the window name, a string to use as a
  label for a final \"submit\" button, x and y sizes, and arguments
  describing a form for the user to fill out, presents the user with a
  window, and returns the values filled out in a hash"
  ([window-name submit-name x y & args]
     (let [state (atom {})
           finished-flag (promise)
           ;; components is a seq of Component/GridBagConstraint pairs
           components (mapcat
                       (fn [ [output-key spec] row]
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
                                   (grid-bag-pos 1 row 1 :west)]]))
                       (partition 2 args)
                       (range))
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
                                                  [submit-button])))]
       (when @finished-flag
         (.dispose window)
         @state))))
