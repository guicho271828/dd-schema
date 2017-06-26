
(in-package :cl-user)
(defpackage dd-schema
  (:use :cl :trivia :alexandria :iterate :lisp-namespace :function-cache)
  (:export :schema
           :leaf
           :compound
           :unate
           :binate
           :schema-index
           :schema-ref
           :schema-total-size
           :define-schema
           :schema-expand
           :object))

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

(defmacro define-schema (name args &body body)
  "Defines a globally available (static) schema. Can take several parameters.
The name for the schema is automatically assgined to the achema returned by the definition.
The schema is always memoized, i.e., there is a single schema under EQ for the same set of parameters.

BODY can also contain a docstring and declarations."
  (let ((expander (symbolicate '%% name)))
    (ematch body
      ((list* (and doc (type string)) body)
       `(progn
          (setf (documentation ',name 'schema) ,doc)
          (defcached ,expander (,@args)
            (let ((tmp (progn ,@body)))
              (setf (schema-name tmp) ',name)
              tmp))
          (setf (symbol-schema ',name)
                (function ,expander))))
      (_
       `(progn
          (defcached ,expander (,@args)
            (let ((tmp (progn ,@body)))
              (setf (schema-name tmp) ',name)
              tmp))
          (setf (symbol-schema ',name)
                (function ,expander)))))))

(defun schema-expand (schema-spec)
  "Expand a schema specifier and retrieves the corresponding schema object."
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
  "Helper function for instantiating a compound schema."
  (declare (list sub-schema))
  (make-compound
   :children (map 'vector #'schema-expand sub-schema)))

(define-schema bit ()
  "a single bit"
  (leaf 1))
(define-schema unate ()
  "A single bit. Alias for BIT, but is more appropriate under unate algebra."
  (leaf 1))
(define-schema binate ()
  "Two bits for representing a binate boolean variable (true, false, unknown)"
  (leaf 2))
(define-schema integer (high)
  "Parametrized integer family"
  (leaf (ceiling (log high 2))))
(define-schema vector (size sub-schema)
  "Vector of the same schema of length SIZE."
  (compound (make-list size :initial-element sub-schema)))
(define-schema structure (&rest children)
  "Structure-like schema. Can specify multiple children, but it has an additional
contraint such that each child schema should be unique so that they can
be identifiable by a schema."
  (let ((children (map 'vector #'ensure-list children)))
    (iter (for c1 in-vector children with-index i)
          (iter (for c2 in-vector children with-index j from (1+ i))
                (assert (not (equalp c1 c2))))))
  (compound children))

(defun schema-total-size (schema-spec)
  "Returns the total size of a schema. TODO: cache it.
Example:

 (schema-total-size '(structure (leaf 2) (leaf 3) (leaf 1))) -> 6
"
  (labels ((rec (schema)
             (ematch schema
               ((leaf size)
                size)
               ((compound children)
                (reduce #'+ children :key #'rec)))))
    (rec (schema-expand schema-spec))))

(defun schema-index (schema-spec &rest specs-or-indices)
  "Returns the beginning index of the range included in the specified schema. TODO: cache it.
Example:

 (schema-index '(structure (leaf 2) (leaf 3) (leaf 1)) 2) -> 5
"
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
    (rec (schema-expand schema-spec) spec-or-indice)))

(defun schema-ref (schema-spec &rest specs-or-indices)
  "Returns the schema in the schema tree specifed by schema-spec, 
Example:

 (schema-ref '(structure (leaf 2) (leaf 3) (leaf 1)) 2) -> (LEAF :NAME NIL :SIZE 1)
 "
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
    (rec (schema-expand schema-spec) specs-or-indices)))
