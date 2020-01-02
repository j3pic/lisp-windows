(asdf:defsystem :lisp-windows
  :author ("Jeremy Phelps")
  :maintainer "Nobody"
  :licence "Proprietary"
  :description "A library to do stuff on Windows."
  :long-description "A binding of a subset of the Win32 API that I have found useful. Also includes a set of utilities."
  :version "0"
  :depends-on (:cffi )
  :components
  ((:file "windows")
   (:file "utils" :depends-on ("windows"))
   (:file "winprocess" :depends-on ("windows" "utils"))
   (:file "clipboard" :depends-on ("windows" "utils"))))
   
