(defsystem rpg-sheet
  :name "rpg-sheet"
  :version 0.1
  :maintainer "Christian Dietrich"
  :licence "GPL"
  :description "foo"
  :components ((:file "package")
               (:file "tools")
               (:file "savage-world" :depends-on ("core"))
               (:file "core" :depends-on ("package" "tools")))
  :depends-on (:cl-pdf :cl-pdf-parser))