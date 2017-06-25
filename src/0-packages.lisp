
(in-package :cl-user)
(defpackage dd-schema
  (:use :cl :trivia :alexandria :iterate :lisp-namespace :function-cache)
  (:export :schema
           :leaf
           :compound
           :unate
           :binate
           :schema-index))

(in-package :dd-schema)

(defstruct schema
  (name nil :type symbol))
(defstruct (leaf (:include schema)
                 (:constructor leaf (size &optional name)))
  size)
(defstruct (compound (:include schema))
  (children #() :type (array)))

;; (define-namespace schema schema nil "Namespace for a ground schema")
(define-namespace schema (function (&rest t) schema) nil
                  "Namespace for a schema expander function")

(defmacro define-schema (name args &body definition)
  "Defines a globally available (static) schema"
  (let ((expander (symbolicate '%% name)))
    (ematch definition
      ((list* (and doc (type string)) definition)
       `(progn
          (setf (documentation ',name 'schema) ,doc)
          (defcached ,expander (,@args)
            (let ((tmp (progn ,@definition)))
              (setf (schema-name tmp) ',name)
              tmp))
          (setf (symbol-schema ',name)
                (function ,expander))))
      (_
       `(progn
          (defcached ,expander (,@args)
            (let ((tmp (progn ,@definition)))
              (setf (schema-name tmp) ',name)
              tmp))
          (setf (symbol-schema ',name)
                (function ,expander)))))))

(defun schema-expand (schema-spec)
  (ematch schema-spec
    ((list* name args)
     (apply (symbol-schema name)
            (mapcar #'schema-expand args)))
    ((symbol)
     (if (schema-boundp schema-spec)
         (funcall (symbol-schema schema-spec))
         schema-spec))
    (_
     schema-spec)))

(defun compound (sub-schema)
  (make-compound
   :children (map 'vector #'schema-expand sub-schema)))

(define-schema bit ()
  "a single bit"
  (leaf 1))
(define-schema unate ()
  (leaf 1))
(define-schema binate ()
  (leaf 2))
(define-schema integer (high)
  "parametrized integer family"
  (leaf (ceiling (log high 2))))
(define-schema vector (size sub-schema)
  (compound (make-list size :initial-element sub-schema)))
(define-schema object (&rest children)
  (let ((children (map 'vector #'ensure-list children)))
    (iter (for c1 in-vector children with-index i)
          (iter (for c2 in-vector children with-index j from i)
                (assert (not (equalp c1 c2))))))
  (compound children))

(defun schema-total-size (schema-spec)
  (labels ((rec (schema)
             (ematch schema
               ((leaf size)
                size)
               ((compound children)
                (reduce #'+ children :key #'rec)))))
    (rec (schema-expand schema-spec))))

(defun schema-index (schema-spec &rest indices)
  "Returns the beginning index of the range included in the specified schema"
  (labels ((rec (schema indices)
             (ematch* (schema indices)
               ((_ nil)
                0)
               (((leaf) _)
                (error "too many indices! ~a" indices))
               (((compound children) (list* (and index (type integer))
                                            more-indices))
                (+ (reduce #'+ children :key #'schema-total-size :end index)
                   (rec (aref children index) more-indices)))
               (((compound children) (list* sub-spec
                                            more-indices))
                (let ((sub-schema (schema-expand sub-spec)))
                  (iter (for c in-vector children)
                        (with found = nil)
                        (setf found (eq c sub-schema))
                        (until found)
                        (summing (schema-total-size c) into sum)
                        (finally
                         (if found
                             (return (+ sum
                                        (rec c more-indices)))
                             (error "schema ~a is not a subschema of ~a !"
                                    sub-spec
                                    schema)))))))))
    (rec (schema-expand schema-spec) indices)))

(defun schema-ref (schema-spec &rest specs)
  (labels ((rec (schema specs)
             (ematch* (schema specs)
               ((_ nil)
                schema)
               (((leaf) _)
                (error "too many specs! ~a" specs))
               (((compound children) (list* (and index (type integer))
                                            more-specs))
                (rec (aref children index) more-specs))
               (((compound children) (list* sub-spec
                                            more-specs))
                (let ((sub-schema (schema-expand sub-spec)))
                  (rec (or (find sub-schema children)
                           (error "schema ~a is not a subschema of ~a !"
                                  sub-spec
                                  schema))
                       more-specs))))))
    (rec (schema-expand schema-spec) specs)))
