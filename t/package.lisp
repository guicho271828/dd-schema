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

(test one
  (is (eq (schema-ref '(vector 5 (integer 255)) 4)
          (schema-ref '(vector 5 (integer 255)) 3)))
  (signals error
    (schema-expand '(object (integer 255) (integer 255))))
  (signals error
    (schema-expand '(object (integer 255) (integer 254))))
  (finishes
    (schema-expand '(object (integer 255) (integer 127)))))


