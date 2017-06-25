#|
  This file is a part of dd-schema project.
  Copyright (c) 2016 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage dd-schema.test-asd
  (:use :cl :asdf))
(in-package :dd-schema.test-asd)


(defsystem dd-schema.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of dd-schema"
  :license "LLGPL"
  :depends-on (:dd-schema
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval (read-from-string "(5am:run! :dd-schema)"))
))
