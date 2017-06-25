#|
  This file is a part of dd-schema project.
  Copyright (c) 2016 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :dd-schema.test
  (:use :cl
        :dd-schema
        :fiveam
        :trivia :alexandria :iterate))
(in-package :dd-schema.test)



(def-suite :dd-schema)
(in-suite :dd-schema)

;; run test with (run! test-name) 




