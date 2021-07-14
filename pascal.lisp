;;;; Data structures for Pascal.
(in-package :WEB-to-HTML)

(deftype Pascal-value ()
  '(or number string enumerator-meaning))

;; Instances of meaning are attached to :identifier tokens, for Phase 3.
(defclass meaning ()
  ((level
    :type fixnum
    :initarg :level
    :reader meaning-level
    :documentation
      "The level of lexical scope at which this meaning was introduced.")
   (standardp
    :type boolean
    :initarg :standardp
    :initform nil
    :reader meaning-standardp
    :documentation
      "True if this identifier is predefined in Pascal.")
   (identifier
    :type identifier
    :initarg :identifier
    :reader meaning-identifier)
   (section-number
    :type fixnum
    :initarg :section-number
    :reader meaning-section-number)
   (id
    :type string
    :reader meaning-id ; computed at initialization time
    :documentation
      "Holds a value suitable for the HTML id attribute; this will be used to
generate links to this identifier's definition/declaration."))
  (:documentation
    "Holds information about some identifier in the current scope."))

;; Actually it is not used at all, since Phase 2 does no error recovery.
(defclass unknown-meaning (meaning)
  ()
  (:documentation
    "Used for identifiers whose meaning is not known."))

;; Note: Numeric macros are treated as a kind of constant.
(defclass macro-meaning (meaning)
  ((parametricp
    :type boolean
    :initarg :parametricp
    :reader macro-parametricp)
   (expansion
    :type list ; token list
    :initarg :expansion
    :accessor macro-expansion)))

(defclass variable-meaning (meaning)
  ((type
    :type Pascal-type
    :initarg :type
    ;; This slot might change if a program parameter is declared as a variable.
    :accessor variable-type)))

(defclass parameter-meaning (variable-meaning)
  ((program-parameter-p
    :type boolean
    :initarg :program-parameter-p
    :initform nil
    :reader parameter-program-parameter-p
    :documentation
      "True if this variable was declared by virtue of appearing in a program's
program heading.")
   (parent
    :type (or routine-meaning program-meaning)
    :initarg :parent
    :reader parameter-parent
    :documentation
      "The procedure/function/program to which this is a parameter.")
   (by-reference-p
    :type boolean
    :initarg :by-reference-p
    :initform nil
    :reader parameter-by-reference-p
    :documentation
      "True if this parameter was declared with the var keyword; this is always
false if program-parameter-p is true.")))

(defclass type-meaning (meaning)
  ((type
    :type Pascal-type
    :initarg :type
    :reader type-type)))

(defclass constant-meaning (meaning)
  ((numeric-macro-p
    :type boolean
    :initarg :numeric-macro-p
    :initform nil
    :reader constant-numeric-macro-p)
   (value
    :type Pascal-value
    :initarg :value
    :reader constant-value)))

(defclass routine-meaning (meaning)
  ((weirdp
    :type boolean
    :initarg :weirdp
    :initform nil
    :reader routine-weirdp
    :documentation
      "True if this is a standard procedure with odd argument conventions.")
   (result-type
    :type (or Pascal-type null) ; nil for procedures
    :initarg :result-type
    :initform nil
    :reader routine-result-type)
   (parameters
    :type list ; of |parameter-meaning| objects
    :initarg :parameters
    :initform nil
    :reader routine-parameters)
   (forwardp
    :type boolean
    :initarg :forwardp
    :initform nil
    :reader routine-forwardp
    :documentation
      "True if the routine is known from a forward declaration.")
   (forward-id
    :type string
    :reader routine-forward-id
    :documentation
      "A secondary id attribute, used to link to the routine's point of forward
declaration."))
  (:documentation
    "The meaning of a procedure or function."))

(defclass program-meaning (meaning)
  ((parameters
    :type list ; of |parameter-meaning| objects
    :initarg :parameters
    :reader program-parameters))
  (:documentation
    "The meaning of a program."))

(defclass field-meaning (meaning)
  ((type
    :type Pascal-type
    :initarg :type
    :reader field-type)
   (tagp
    :type boolean
    :initarg :tagp
    :initform nil
    :reader field-tagp
    :documentation
      "True if this a tag field in a variant record."))
  (:documentation
    "The meaning of a record field."))

(defclass enumerator-meaning (meaning)
  ((parent
    :type enumeration-type
    :initarg :parent
    :reader enumerator-parent
    :documentation
      "The enumeration of which this is a member.")
   (ordinal
    :type integer
    :initarg :ordinal
    :reader enumerator-ordinal
    :documentation
      "The ordinal value of the enumerator."))
  (:documentation
    "The meaning of an enumerator."))

(defclass Pascal-type ()
  ((standardp
    :type boolean
    :initarg :standardp
    :initform nil
    :reader Pascal-type-standardp))
  (:documentation
    "The representation of a Pascal type."))

(defclass scalar-type ()
  ()
  (:documentation
    "Mixin for scalar types."))

(defclass pointer-type (Pascal-type)
  ((type
    :type (or Pascal-type null) ; nil for forward references
    :initarg :type
    :accessor pointer-type-type)))

(defclass subrange-type (Pascal-type scalar-type)
  ((upper-bound
    :type Pascal-value
    :initarg :upper-bound
    :reader subrange-upper-bound)
   (lower-bound
    :type Pascal-value
    :initarg :lower-bound
    :reader subrange-lower-bound)))

(defclass enumeration-type (Pascal-type scalar-type)
  ((enumerators
    :type list ; of |enumerator-meaning| objects
    :initform (list)
    :accessor enumeration-enumerators)))

(defclass array-type (Pascal-type)
  ((packedp
    :type boolean
    :initarg :packedp
    :reader array-packedp)
   (subscripts
    :type list ; of |scalar-type| objects
    :initarg :subscripts
    :reader array-subscripts)
   (element-type
    :type Pascal-type
    :initarg :element-type
    :reader Pascal-array-element-type)))

(defclass file-type (Pascal-type)
  ((packedp
    :type boolean
    :initarg :packedp
    :reader file-packedp)
   (element-type
    :type Pascal-type
    :initarg :element-type
    :reader file-element-type)))

(defclass record-type (Pascal-type)
  ((packedp
    :type boolean
    :initarg :packedp
    :reader record-packedp)
   (fields
    :type hash-table ; miniature environment
    :initarg :fields
    :reader record-fields)))

(defun get-field-meaning (identifier record)
  "Get the meaning of a field in a record."
  (gethash identifier (record-fields record)))

(defclass set-type (Pascal-type)
  ((packedp
    :type boolean
    :initarg :packedp
    :reader set-type-packedp)
   (element-type
    :type scalar-type
    :initarg :element-type
    :reader set-element-type)))

(defclass label ()
  ((value
    :type (or integer constant-meaning)
    :initarg :value
    :reader label-value)
   (definition-id
    :type string
    :reader label-definition-id)
   (declaration-id
    :type string
    :reader label-declaration-id)))

(defstruct environment
  (depth 0 :type fixnum) ; depth of nesting
  (name nil :type (or identifier null))
    ; under whose auspices this environment exists
  (outer nil :type (or environment null)) ; outer scope
  (labels (list) :type list) ; list of |label| objects
  (meanings (make-hash-table :test #'eq) :type hash-table))

(defgeneric generate-id (meaning environment)
  (:documentation
    "Generate a string suitable for an HTML id attribute. Primary methods of
this generic function should return only a string; an :around method does most
of the work."))

(let ((generated (make-hash-table :test #'equal)))
  (defun ensure-unique-id (string)
    "Ensure that the string will be a unique id in the document."
    (cond ((gethash string generated)
           (incf (gethash string generated))
           (format nil "~A~D" string (gethash string generated)))
          (t
           (setf (gethash string generated) 1)
           string))))

(defun generate-trace (environment)
  (if (environment-name environment)
      (let ((name (identifier-name (environment-name environment))))
        (if (not (environment-outer environment))
            name
            (concatenate 'string
                         "-"
                         name
                         (generate-trace (environment-outer environment)))))
      ""))

(defmethod initialize-instance :after ((object label)
                                       &key value environment
                                       &allow-other-keys)
  (let ((trace (generate-trace environment)))
    (with-slots (definition-id declaration-id) object
      (setf definition-id (format nil "label-~D-definition-~A" value trace)
            declaration-id (format nil "label-~D-declaration-~A" value trace)))))

(defmethod generate-id :around ((meaning meaning) environment)
  (ensure-unique-id (concatenate 'string
                                 (identifier-name (meaning-identifier meaning))
                                 "-"
                                 (call-next-method)
                                 (generate-trace environment))))

(defmethod generate-id :around ((meaning unknown-meaning) environment)
  (declare (ignore environment))
  "") ; no id will be generated for identifiers with unknown meanings

(macrolet ((define-simple-meaning-method (class-name string)
             `(defmethod generate-id ((meaning ,class-name) environment)
                (declare (ignore environment))
                ,string)))
  (define-simple-meaning-method macro-meaning "macro")
  (define-simple-meaning-method variable-meaning "variable")
  (define-simple-meaning-method type-meaning "type")
  (define-simple-meaning-method program-meaning "program")
  (define-simple-meaning-method field-meaning "field")
  (define-simple-meaning-method enumerator-meaning "enumerator"))

(defmethod generate-id ((meaning parameter-meaning) environment)
  (if (parameter-program-parameter-p meaning)
      "program-parameter"
      "parameter"))

(defmethod generate-id ((meaning constant-meaning) environment)
  (if (constant-numeric-macro-p meaning)
      "numeric-macro"
      "constant"))

(defmethod generate-id ((meaning routine-meaning) environment)
  (if (routine-result-type meaning)
      "function"
      "procedure"))

(defmethod initialize-instance :after ((object meaning)
                                       &key environment
                                       &allow-other-keys)
  (setf (slot-value object 'id) (generate-id object environment)))

(defmethod initialize-instance :after ((object routine-meaning)
                                       &key environment
                                       &allow-other-keys)
  (when (routine-forwardp object)
    (setf (slot-value object 'forward-id) (generate-id object environment))))
