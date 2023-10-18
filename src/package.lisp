(defpackage #:tanks
  (:use
   #:bordeaux-threads
   #:cl)
  (:import-from #:float-features
                #:with-float-traps-masked)
  (:export
   #:main))
