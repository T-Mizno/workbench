(defn rename [prefix]
      (for [f (.listFiles (new java.io.File "."))]
           (.renameTo f (new java.io.File (str prefix "-" (.getName f)))))
 )
 





