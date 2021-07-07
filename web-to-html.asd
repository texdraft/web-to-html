(defsystem "web-to-html"
  :components ((:file "common")
               (:file "phase-1" :depends-on ("common"))
               (:file "pascal")
               (:file "phase-2" :depends-on ("common" "pascal"))))