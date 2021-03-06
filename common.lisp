;;;; Definitions for all three phases of processing.
(defpackage :WEB-to-HTML
  (:use :Common-Lisp)
  (:export :Phase-1))

(in-package :WEB-to-HTML)

(defstruct module
  (name nil :type (or string null)) ; nil for unnamed module
  (lower-link nil :type (or module null)) ; lexicographically lesser name
  (upper-link nil :type (or module null)) ; lexicographically greater name
  (name-tokens nil :type list) ; of tokens making up the name, for formatting
  (definitions (list) :type list) ; list of section numbers
  (uses (list) :type list)) ; list of section numbers

(defstruct section
  (number 0 :type fixnum) ; number of this section
  (changedp nil :type boolean) ; was this section altered in a change file?
  (starredp nil :type boolean) ; did the section begin with @*?
  (starred-name nil :type (or string null)) ; text from @* to period
  (TeX-part nil :type list) ; list of tokens
  (definition-part nil :type list) ; ditto
  (Pascal-part nil :type list) ; ditto
  (module nil :type (or module null)) ; nil if unnamed
  ;; The |identifier-uses| slot holds an association list, pairing |identifier|
  ;; (defined below) objects with lists of |use-record| (also defined below)
  ;; objects.
  (identifier-uses (list) :type list))

(deftype token-type-type ()
  '(member nil ; for malformed text
           ;; Reserved words are just identifiers with certain properties.
           :identifier
           ;; Pascal punctuators:
           :becomes ; :=
           :up-arrow ; ^
           :semicolon ; ;
           :colon ; :
           :comma ; ,
           :dot ; .
           :times ; *
           :solidus ; /
           :plus ; +
           :minus ; -
           :left-parenthesis ; (
           :right-parenthesis ; )
           :left-bracket ; [
           :right-bracket ; ]
           :equals ; =
           :less-than ; <
           :greater-than ; >
           :less-than-or-equal-to ; <=
           :greater-than-or-equal-to ; >=
           :not-equal ; <>
           :subrange ; ..
           :Pascal-string ; '...'
           :decimal-integer
           :floating-point-literal

           ;; WEB-specific tokens:
           :dollar-sign ; needed for compiler-directives
           :WEB-string ; "..."
           :do-index ; @!
           :don\'t-index ; @?
           :octal-integer ; @'...
           :hexadecimal-integer ; @"...
           :thin-space ; @,
           :module-name ; @<...@>
           :verbatim-Pascal ; @=...@>
           :verbatim-TeX ; @t...@>
           :Pascal-line-break ; @\
           :pseudo-semicolon ; @;
           :extra-space ; @#
           :index-Roman ; @^...@
           :index-typewriter ; @....@>
           :index-wildcard ; @:...@>
           :join ; @&
           :equivalence-sign ; ==
           :check-sum ; @$
           :begin-meta-comment ; @{
           :end-meta-comment ; @}
           :new-section ; @* or @ followed by space
           :macro-parameter ; #
           ;; The @/, @+, @|, @0, @1, and @2 control codes are ignored.

           ;; Tokens new to this program:
           :TeX-portion ; piece of TeX text
           :long-distance ; @n...@>
           :begin-comment ; start of Pascal comment
           :end-comment ; end of Pascal comment
           :begin-inline-code ; start of code in TeX text
           :end-inline-code ; end of code in TeX text
           :begin-definition ; beginning of one definition (@d)
           :begin-format)) ; beginning of one format specification (@f)

(defstruct origin
  "Tells where something came from."
  (section-number 0 :type fixnum)
  (from-module-name-p nil :type boolean)
  (module nil :type (or module null))
  (line 0 :type fixnum)
  (from-change-file-p nil :type boolean))

;; We destructure floating-point literals for two reasons:
;;         (1) It avoids any issues with precision.
;;         (2) It makes formatting the number easier in Phase 3. 
(defstruct floating-point-literal
  (whole-part nil :type integer)
  (fractional-part nil :type integer)
  (exponent-sign nil :type (member :plus :minus))
  (exponent-part nil :type integer))

(defstruct token
  (type nil :type token-type-type)
  (origin nil :type origin)
  (content nil :type (or null ; punctuators, |:begin-Pascal| for unnamed
                              ; modules, |:begin-comment|, |:end-comment|,
                              ; |:begin-inline-code, |:end-inline-code|,
                              ; |:dollar-sign|
                         integer ; integer literals, string number for
                                 ; |:WEB-string|, number for |:long-distance|
                         string ; control text, |:Pascal-string|, |:TeX-portion|
                         floating-point-literal ; floating-point numbers
                         identifier ; identifiers
                         module)) ; module references
  (extra nil :type (or extra null)))

(deftype formatting-property () ; defined for the sake of documentation
  '(member :indent ; increase indentation on next line break
           :dedent ; decrease indentation on next line break
           :outdent ; make this line stick out to the left
           :begin-table ; start table
           :end-table ; conclude table
           :line-break-before ; break line after previous token
           :line-break-after)) ; break line before next token

(defstruct extra
  "Holds additional information about a token."
  ;; The first three slots are used only for |:identifier| tokens.
  (definingp nil :type boolean) ; is this a definition?
  (declaringp nil :type boolean) ; is this a declaration?
  (meanings (list) :type list) ; list of |meaning| (defined in pascal.lisp)
                               ; objects
  (label-status nil :type (or boolean
                              label)) ; defined in pascal.lisp
  (formatting-properties (list) :type list)) ; of |formatting-property| keywords

;;; String pool maintenance.
(let ((character-set (format nil " !\"#$%&'~
                                  ()*+,-./~
                                  01234567~
                                  89:;<=>?~
                                  @ABCDEFG~
                                  HIJKLMNO~
                                  PQRSTUVW~
                                  XYZ[\\]^_~
                                  `abcdefg~
                                  hijklmno~
                                  pqrstuvw~
                                  xyz{|}~~")))
  (defun xord (character)
    "Compute the a character's ASCII code."
    (+ (position character character-set)
       #o40)))

(let* ((maximum-strings 3000) ; this is the value of max_names in TANGLE
       (check-sum-prime #o3777777667)
       (strings (make-array maximum-strings))
       (string-count 256)
       (string-table (make-hash-table :test #'equal))
       (check-sum 271828))
  (defun reset-string-pool ()
    (setf strings (make-array maximum-strings)
          string-count 256
          string-table (make-hash-table :test #'equal)
          check-sum 271828))

  (defun lookup-string (string)
    "Return the number for the given string. Doubled quotation marks within the
string will have been reduced."
    (when (= (length string) 1)
      (return-from lookup-string (xord (schar string 0))))
    (let ((number (gethash string string-table)))
      (unless number ; we are adding a new string
        (incf string-count)
        (when (> string-count maximum-strings)
          (error "String pool overflow."))
        (setf number string-count
              (aref strings number) string
              (gethash string string-table) number)
        (let ((length (length string)))
          (when (> length 99)
            (error "Preprocessed string is too long"))
          (flet ((increase-check-sum (amount)
                   (incf check-sum (+ check-sum amount))
                   (loop while (> check-sum check-sum-prime) do
                     (decf check-sum check-sum-prime))))
            (increase-check-sum length)
            (loop for c across string do
              (increase-check-sum (xord c))))))
      number))

  (defun get-string (number)
    "Return a string, given its number."
    (assert (<= number string-count))
    (aref strings number))

  (defun get-check-sum ()
    "Return the string pool's check sum."
    check-sum))

;;; Modules and sections.
(let* ((maximum-sections 2000)
       ;; Note: Index 0 of sections is not used.
       (sections (make-array (+ maximum-sections 1)))
       (section-count 0)
       (root nil) ; root of module tree
       (unnamed-module (make-module)))
  (defun reset-modules/sections ()
    (setf sections (make-array (+ maximum-sections 1))
          section-count 0
          root nil
          unnamed-module (make-module)))

  (flet ((compare-strings (s_1 s_2)
           (let ((s_1-length (length s_1))
                 (s_2-length (length s_2))
                 (mismatch (string/= s_1 s_2)))
             (cond ((not mismatch)
                    :equal)
                   ((>= mismatch s_1-length)
                    :prefix)
                   ((>= mismatch s_2-length)
                    :extension)
                   ((char< (schar s_1 mismatch) (schar s_2 mismatch))
                    :less)
                   ((char> (schar s_1 mismatch) (schar s_2 mismatch))
                    :greater)))))
    (defun lookup-module (name)
      "Search for a module, given its name; if there is no match, a new module
will be added."
      (let ((node root)
            (parent nil)
            (comparison :greater))
        (loop while node do
          (setf comparison (compare-strings name (module-name node))
                parent node)
          (cond ((eq comparison :less)
                 (setf node (module-lower-link parent)))
                ((eq comparison :greater)
                 (setf node (module-upper-link parent)))
                ((eq comparison :equal)
                 (return-from lookup-module node))
                (t
                 (cerror "Return nil." "Incompatible section names.")
                 (return-from lookup-module nil))))
        (if (not parent) ; first name being added
            (setf root (make-module :name name)
                  node root)
            (let ((module (make-module :name name)))
              (if (eq comparison :less)
                  (setf (module-lower-link parent) module)
                  (setf (module-upper-link parent) module))
              (setf node module)))
        node))

    (defun lookup-prefix (prefix)
      "Search for a module, given a prefix of its name."
      (let ((node root)
            (resume nil)
            (extension nil)
            (match-count 0))
        (loop while node do
          (let ((comparison (compare-strings prefix (module-name node))))
            (cond ((eq comparison :less)
                   (setf node (module-lower-link node)))
                  ((eq comparison :greater)
                   (setf node (module-upper-link node)))
                  (t
                   (setf extension node
                         resume (module-upper-link node)
                         node (module-lower-link node))
                   (incf match-count))))
          (unless node
            (shiftf node resume nil)))
        (when (/= match-count 1)
          (error "Ambiguous prefix."))
        extension)))

  (defun add-unnamed (number)
    "Add a definition of the unnamed module."
    (push number (module-definitions unnamed-module)))

  (defun get-unnamed ()
    "Get the unnamed module."
    unnamed-module)

  (defun map-modules (function)
    "Call a given function once for every named module."
    (labels ((do-module (node)
               (when (module-upper-link node)
                 (do-module (module-upper-link node)))
               (funcall function node)
               (when (module-lower-link node)
                 (do-module (module-lower-link node)))))
      (when root ; |root| remains nil if no module names have been entered
        (do-module root))))

  (defun add-section (section)
    "Add a new section."
    (incf section-count)
    (when (> section-count maximum-sections)
      (error "Too many sections."))
    (setf (aref sections section-count) section))

  (defun get-nth-section (n)
    "Get the nth section."
    (assert (<= n section-count))
    (aref sections n))

  (defun get-current-section ()
    (aref sections section-count))

  (defun get-section-count ()
    "Return the current section count."
    section-count))

;;; Identifiers.
(defstruct identifier
  "Global information about a name."
  (name nil :type string)
  ;; The |only-meaning| slot is initially nil. If an identifier is given only a single
  ;; meaning in the whole of Phase 2, then |only-meaning| will hold it; if the identifier
  ;; has multiple meanings, then |only-meaning| will be |t|.
  (only-meaning nil :type (or boolean meaning))
  ;; The following slot exists only for TeX and Metafont's mtype.
  (synonym nil :type (or string null))
  (reservedp nil :type boolean) ; is this identifier to be treated as a reserved word?
  (underlined-references (list) :type list) ; of section numbers
  (references (list) :type list)) ; of section numbers

(defstruct use-record
  "Describes how an identifier is used in a section."
  (meaning nil :type meaning) ; defined in pascal.lisp
  (writtenp nil :type boolean)) ; was it modified?

(let ((symbols (make-hash-table :test #'equal)))
  (defun reset-symbol-table ()
    (setf symbols (make-hash-table :test #'equal)))

  (defun map-identifiers (function)
    "Call a function once for every identifier."
    (maphash (lambda (key value)
               (declare (ignore key))
               (funcall function value))
             symbols))

  (defun lookup-identifier (name)
    "Retrieve the global information about the identifier with the given name."
    (let ((identifier (gethash name symbols)))
      (unless identifier
        (setf identifier (make-identifier :name name)
              (gethash name symbols) identifier))
      identifier)))

;;; Indexing.
(defstruct cross-reference-datum
  (underlined-references (list) :type list) ; of section numbers
  (other-references (list) :type list)) ; of section numbers

;; These tables are keyed on strings (the control text in an indexing
;; command); the values are cross-reference-datum objects.
(let ((Roman-index-entries (make-hash-table :test #'equal))
      (typewriter-index-entries (make-hash-table :test #'equal))
      (wildcard-index-entries (make-hash-table :test #'equal)))
  (defun reset-index ()
    (setf Roman-index-entries (make-hash-table :test #'equal)
          typewriter-index-entries (make-hash-table :test #'equal)
          wildcard-index-entries (make-hash-table :test #'equal)))

  (defun index (type text section underlinedp)
    "Add a reference to an index entry."
    (let* ((table (ecase type
                    ((:index-Roman)
                     Roman-index-entries)
                    ((:index-typewriter)
                     typewriter-index-entries)
                    ((:index-wildcard)
                     wildcard-index-entries)))
           (entry (gethash text table)))
      (when (not entry)
        (setf entry (make-cross-reference-datum)
              (gethash text table) entry))
      (if underlinedp
          (pushnew section (cross-reference-datum-underlined-references entry))
          (pushnew section (cross-reference-datum-other-references entry))))))

;; Call this after running the phases on one WEB file, in order to process
;; another file with a clean slate.
(defun reset ()
  "Reset the various semi-global data structures."
  (reset-string-pool)
  (reset-modules/sections)
  (reset-symbol-table)
  (reset-index))
