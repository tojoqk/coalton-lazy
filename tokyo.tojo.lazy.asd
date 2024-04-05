(defsystem #:tokyo.tojo.lazy
  :description "Lazy evalution for Coalton"
  :author "Masaya Tojo"
  :license  "MIT"
  :version "0.0.0"
  :depends-on (#:coalton)
  :serial t
  :pathname "src/"
  :components ((:file "promise")
               (:file "lazy")
               (:file "stream")))
