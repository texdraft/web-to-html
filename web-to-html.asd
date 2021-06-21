(defsystem "web-to-html"
  :components ((:file "common")
               (:file "phase-1" :depends-on ("common"))))
