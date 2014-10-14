(import (javax.swing JFrame)
        (javax.swing JLabel))
 
(def frame
  (JFrame.))
 
(def label
  (JLabel. "Hello, Swing World!"))
 
(doto label
  (.setVerticalAlignment JLabel/TOP))
 
(doto frame
  (.setDefaultCloseOperation
    javax.swing.WindowConstants/EXIT_ON_CLOSE)
  (.add label)
  (.setSize 640 480)
  (.setTitle "Hello, World")
  (.setResizable false)
  (.setVisible true)) 
