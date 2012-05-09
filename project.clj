(defproject mcmap "0.1.0"
  :description "Libraries for outputting Minecraft maps, and an example tool"
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :dev-dependencies [[swank-clojure "1.3.3"]
                     [midje "0.4.0"]]
  :main mcmap.gui
  :jvm-opts ["-Xmx4g -XX:-OmitStackTraceInFastThrow"])
