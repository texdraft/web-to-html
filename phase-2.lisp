;;;; Phase 2 processing (parsing/semantication).
;; (I got the term ``semantication'' from Bernard S. Greenburg's paper about the
;; Multics Maclisp compiler, accessible at https://www.multicians.org/lcp.html.)
(in-package :WEB-to-HTML) ; depends on common.lisp and pascal.lisp

(defstruct parser-state
  (pending-pointers (list) :type list) ; pointer forward references
  (level 0 :type fixnum) ; level of expansion
  (environment (make-environment) :type environment)
  (arguments (list) :type list) ; stack of macro arguments
  ;; The name slot is nil when a macro argument is being expanded.
  (name nil :type (or identifier ; name of macro being expanded
                      module  ; name of module being expanded
                      null)) ; argument
  (section nil :type (or section null)) ; section being visited
  (text nil :type list) ; current source of tokens
  (mode :module :type (member :normal-macro
                              :parametric-macro
                              :module
                              :argument))
  (backed-up nil :type list) ; tokens to be read again
  (next nil :type list) ; next definitions of current module
  (name-stack (list) :type list) ; previous values of the name slot
  (section-stack (list) :type list) ; previous values of the section slot
  (text-stack (list) :type list) ; previous values of the text slot
  (mode-stack (list) :type list) ; previous values of the mode slot
  (next-stack (list) :type list)) ; previous values of next

(defun Phase-2-error (parser control &rest arguments)
  "Report a problem during Phase 2. This function does not return."
  (error "! ~? (In section ~D~@[, tokens originating around line ~D~].)~%"
         control
         arguments
         (section-number (parser-state-section parser))
         (and (not (null (parser-state-text parser)))
              (origin-line (token-origin (first (parser-state-text parser)))))))

(defun install-meaning (parser identifier class-name &rest arguments)
  "Update the current environment by adding a new meaning for an identifier. No
attempt is made to catch redefinition errors."
  (let ((environment (parser-state-environment parser)))
    (setf (gethash identifier (environment-meanings environment))
          (apply #'make-instance class-name
                                 :identifier identifier
                                 :environment environment
                                 :level (environment-depth environment)
                                 arguments))))

(defun insert-meaning (parser meaning)
  "Update the current environment by adding an existing meaning."
  (let ((identifier (meaning-identifier meaning))
        (environment (parser-state-environment parser)))
    (setf (gethash identifier (environment-meanings environment)) meaning)))

(defun get-meaning (parser identifier &optional allow-nonexistent)
  "Look up the meaning of an identifier in the current environment. If the
search is unsuccessful and the third argument is false, an error is signaled."
  (labels ((find-meaning (environment)
             (cond ((not environment)
                    (if allow-nonexistent
                        nil
                        (Phase-2-error parser
                                       "Undeclared identifier ``~A''."
                                       (identifier-name identifier))))
                   ((gethash identifier (environment-meanings environment)))
                   (t
                    (find-meaning (environment-outer environment))))))
    (find-meaning (parser-state-environment parser))))

(defun get-label (parser value)
  "Find a label object, given its value."
  (labels ((find-label (environment)
             (if environment
                 (let ((labels (environment-labels environment)))
                   (or (find value labels :key #'label-value)
                       (find-label (environment-outer environment))))
                 (Phase-2-error parser "Undeclared label ~D." value))))
    (find-label (parser-state-environment parser))))

(defun install-label (parser value)
  "Update the current environment by adding a new label, returning this object."
  ;; We don't check for duplicates.
  (let* ((environment (parser-state-environment parser))
         (label (make-instance 'label :value value
                               :environment environment)))
    (push label (environment-labels environment))
    label))

(defun begin-scope (parser name)
  "Begin a new level of lexical scope."
  (let* ((old-environment (parser-state-environment parser))
         (new-environment (make-environment :name name
                                            :outer old-environment)))
    (setf (parser-state-environment parser) new-environment
          (environment-depth new-environment) (+ (environment-depth old-environment)
                                                 1))))

(defun end-scope (parser)
  "Exit the current level of lexical scope."
  (let ((environment (parser-state-environment parser)))
    (setf (parser-state-environment parser) (environment-outer environment))))

(defun push-input (parser)
  "Save the current expansion state for later."
  (incf (parser-state-level parser))
  (push (parser-state-name parser) (parser-state-name-stack parser))
  (push (parser-state-section parser) (parser-state-section-stack parser))
  (push (parser-state-text parser) (parser-state-text-stack parser))
  (push (parser-state-mode parser) (parser-state-mode-stack parser))
  (push (parser-state-next parser) (parser-state-next-stack parser)))

(defun pop-input (parser)
  "Restore the previous expansion state."
  (when (zerop (parser-state-level parser))
    (Phase-2-error parser "Unexpected end of input."))
  (decf (parser-state-level parser))
  (setf (parser-state-name parser) (pop (parser-state-name-stack parser))
        (parser-state-section parser) (pop (parser-state-section-stack parser))
        (parser-state-text parser) (pop (parser-state-text-stack parser))
        (parser-state-mode parser) (pop (parser-state-mode-stack parser))
        (parser-state-next parser) (pop (parser-state-next-stack parser))))

(defun end-level (parser)
  "Do the right thing when the current token list is depleted."
  (case (parser-state-mode parser)
    ((:module)
     (let ((next (pop (parser-state-next parser))))
       (if (not (null next))
           (let ((section (get-nth-section next)))
             (setf (parser-state-text parser) (section-Pascal-part section)
                   (parser-state-section parser) section))
           (pop-input parser))))
    ((:parametric-macro :normal-macro)
     (when (eq (parser-state-mode parser) :parametric-macro)
       (pop (parser-state-arguments parser)))
     (pop-input parser))
    ((:argument)
     (pop-input parser))))

(defun scan-argument (parser identifier)
  "Read the argument for a parametric macro."
  (let ((token (pop (parser-state-text parser))))
    (cond ((or (not token)
               (not (eq (token-type token) :left-parenthesis)))
           (Phase-2-error parser "No argument given for ~A."
                                 (identifier-name identifier)))
          (t
           (let ((argument (list)) ; we are building this list
                 (top-argument (first (parser-state-arguments parser))) ; current #
                 (balance 1)) ; number of open parentheses
             (loop do (let ((token (pop (parser-state-text parser))))
                        (case (token-type token)
                          ((:left-parenthesis)
                           (incf balance)
                           (push token argument))
                          ((:right-parenthesis)
                           (decf balance)
                           (when (> balance 0)
                             (push token argument)))
                          ((:macro-parameter)
                           (setf argument (revappend top-argument argument)))
                          (otherwise
                           (push token argument))))
                   until (zerop balance))
             (push (nreverse argument) (parser-state-arguments parser)))))))

(defun begin-module (parser module)
  "Begin expanding a module."
  (let* ((definitions (module-definitions module))
         (first (first definitions))
         (section (get-nth-section first))
         (next (rest definitions)))
    (setf (parser-state-next parser) next
          (parser-state-section parser) section
          (parser-state-text parser) (section-Pascal-part section)
          (parser-state-name parser) module)))

(defun skippable-token-p (token)
  "True if the token is of no interest to the parser."
  (member (token-type token) '(:index-Roman
                               :index-typewriter
                               :index-wildcard
                               :extra-space
                               :verbatim-Pascal
                               :Pascal-line-break
                               :verbatim-TeX
                               :pseudo-semicolon
                               :thin-space
                               :join
                               :don\'t-index
                               :do-index
                               :line-break
                               :begin-label
                               :end-label)))

(defun skip-skippable (parser) ; concession to indentation in get-next
  (loop while (and (not (null (parser-state-text parser)))
                   (skippable-token-p (first (parser-state-text parser))))
        do (pop (parser-state-text parser))))

;; The attach-meaning function is defined later.
(defun get-next (parser)
  "Get the next token, after macro/module expansion."
  (when (parser-state-backed-up parser)
    (return-from get-next (pop (parser-state-backed-up parser))))
  (let ((token (pop (parser-state-text parser))))
    (cond ((not token)
           (end-level parser)
           (get-next parser))
          ((skippable-token-p token)
           (get-next parser))
          (t
           (case (token-type token)
             ((:macro-parameter)
              (push-input parser)
              (setf (parser-state-mode parser) :argument
                    (parser-state-text parser) (first (parser-state-arguments parser))
                    (parser-state-name parser) nil)
              (get-next parser))
             ((:identifier)
              (let* ((identifier-token token)
                     (meaning (get-meaning parser
                                           (token-content identifier-token)
                                           t)))
                (cond ((and meaning (typep meaning 'macro-meaning))
                       (attach-meaning parser token)
                       (let ((parametricp (macro-parametricp meaning)))
                         (when parametricp
                           ;; See section 90 of TANGLE. This is necessary to
                           ;; handle ``tail calls'', where the last token in a
                           ;; macro's expansion is a call to a parametric macro.
                           (loop while (null (parser-state-text parser)) do
                             (end-level parser)
                             (skip-skippable parser))
                           (scan-argument parser (token-content identifier-token)))
                         (push-input parser)
                         (setf (parser-state-mode parser) (if parametricp
                                                              :parametric-macro
                                                              :normal-macro)))
                       (setf (parser-state-text parser) (macro-expansion meaning)
                             (parser-state-name parser) (meaning-identifier meaning))
                       (get-next parser))
                      (t
                       token))))
             ((:module-name)
              (push-input parser)
              (begin-module parser (token-content token))
              (get-next parser))
             ((:begin-comment)
              (loop until (eq (token-type token) :end-comment) do
                (setf token (pop (parser-state-text parser))))
              (get-next parser))
             ((:begin-meta-comment)
              (get-next parser))
             ((:end-meta-comment)
              (get-next parser))
             (otherwise
              token))))))

(defun put-back-token (parser token)
  "Place a token back in the input stream, to be read again."
  (push token (parser-state-backed-up parser)))

;; I'd recommend not reading this code.
(defun scan-definition-part (parser section-number text)
  (let ((token))
    (labels ((next ()
               (setf token (pop text))
               (cond ((and token (skippable-token-p token))
                      (next))
                     ((and token
                           (eq (token-type token) :begin-comment))
                      ;; Phase 1 guarantees that every :begin-comment is
                      ;; balanced, so we will not run out of tokens in the
                      ;; following loop.
                      (loop do (setf token (pop text))
                            until (eq (token-type token) :end-comment))
                      (next))
                     (t
                      token)))
             (bad ()
               (error "Bad definition part in section ~D.~%" section-number))
             (get-replacement-text ()
               (prog1 (loop do (next)
                            while (and token
                                       (not (or (eq (token-type token)
                                                    :begin-definition)
                                                (eq (token-type token)
                                                    :begin-format))))
                            collect token)
                      (push token text))) ; put the :begin-whatever back
             (scan-numeric (identifier)
               (let ((value 0)
                     (next-sign +1))
                 (flet ((add-in (value)
                          (incf value (* next-sign value))
                          (setf next-sign +1)))
                   (loop do (next)
                            (when token
                              (case (token-type token)
                                ((:decimal-integer
                                  :octal-integer
                                  :hexadecimal-integer)
                                 (add-in (token-content token)))
                                ((:identifier)
                                 (let ((meaning (get-meaning parser
                                                             (token-content token))))
                                   (unless (and (typep meaning 'constant-meaning)
                                                (constant-numeric-macro-p meaning))
                                     (bad))
                                   (add-in (constant-value meaning))))
                                ((:minus)
                                 (setf next-sign (- next-sign)))
                                ((:plus)
                                 :do-nothing)
                                ((:begin-definition :begin-format)
                                 :do-nothing)
                                ((:WEB-string)
                                 (add-in (token-content token)))
                                (otherwise
                                 (bad))))
                         until (or (not token)
                                   (eq (token-type token) :begin-definition)
                                   (eq (token-type token) :begin-format)))
                   (push token text))
                 (install-meaning parser identifier
                                  'constant-meaning
                                  :numeric-macro-p t
                                  :value value))))
      (loop do (next)
            until (null text)
            do (cond ((eq (token-type token) :begin-definition)
                      (let ((name (next)))
                        (unless (and name
                                     (eq (token-type name) :identifier))
                          (bad))
                        (case (token-type (next))
                          ((:left-parenthesis)
                           (next)
                           (unless (and token
                                        (eq (token-type token)
                                            :macro-parameter))
                             (bad))
                           (next)
                           (unless (and token
                                        (eq (token-type token)
                                            :right-parenthesis))
                             (bad))
                           (next)
                           (unless (and token
                                        (eq (token-type token)
                                            :equivalence-sign))
                             (bad))
                           (install-meaning parser
                                            (token-content name)
                                            'macro-meaning
                                            :parametricp t
                                            :expansion (get-replacement-text)))
                          ((:equals)
                           (scan-numeric (token-content name)))
                          ((:equivalence-sign)
                           (install-meaning parser
                                            (token-content name)
                                            'macro-meaning
                                            :parametricp nil
                                            :expansion (get-replacement-text)))
                          (otherwise
                           (bad)))))
                     ((eq (token-type token) :begin-format)
                      (let ((name (next)))
                        (unless (and name
                                     (eq (token-type name) :identifier))
                          (bad))
                        (next)
                        (unless (and token
                                     (eq (token-type token) :equivalence-sign))
                          (bad))
                        (let ((equivalent (next)))
                          (unless (and equivalent
                                       (eq (token-type equivalent) :identifier))
                            (bad))
                          (do-format (token-content name)
                                     (token-content equivalent)))))
                     (t
                      (bad)))))))

(defun do-format (name equivalent)
  "Perform the act of changing an identifier's formatting."
  (setf (identifier-reservedp name) (identifier-reservedp equivalent)))

(defun install-macros (parser)
  (loop for n from 1 to (get-section-count) do
    (let ((section (get-nth-section n)))
      (unless (null (section-definition-part section))
        (scan-definition-part parser
                              n
                              (section-definition-part section))))))

;;; Parsing.
;; The code here is based on the grammar given in Jensen and Wirth's Pascal User
;; Manual and Report, with a few adjustments to account for Pascal-H.

(defun ensure-extra (token)
  "Get the extra field of a token, creating it if it does not exist."
  (unless (token-extra token)
    (setf (token-extra token) (make-extra)))
  (token-extra token))

(defun attach-meaning (parser token &optional info)
  "Annotate an identifier token with its meaning. The info parameter, if
present, should be either :declaring or :defining."
  (let ((extra (ensure-extra token)))
    (pushnew (get-meaning parser (token-content token))
             (extra-meanings extra)
             :test #'eq)
    (cond ((eq info :declaring)
           (setf (extra-declaringp extra) t))
          ((eq info :defining)
           (setf (extra-definingp extra) t)))))

(defun attach-field-meaning (parser token record)
  "Annotate the name of a record field with its meaning."
  (let ((meaning (get-field-meaning (token-content token) record)))
    (unless meaning
      (Phase-2-error parser "Bad field selector."))
    (push meaning (extra-meanings (ensure-extra token)))))

(defun attach-line-break (token type place)
  "Annotate a token with line-breaking information."
  (let ((extra (ensure-extra token)))
    (if (eq place :before)
        (setf (extra-break-before extra) type)
        (setf (extra-break-after extra) type))))

(defun is-reserved-word-p (token word)
  "Return true if the token should be read as the given Pascal reserved word."
  (and (eq (token-type token) :identifier)
       (identifier-reservedp (token-content token))
       (or (string= (identifier-name (token-content token)) word)
           ;; An identifier defined to be synonymous with a reserved word should
           ;; be treated as that word. (This facility is present only for the
           ;; sake of mtype in TeX and Metafont.)
           (and (identifier-synonym (token-content token))
                (string= (identifier-synonym (token-content token)) word)))))

(defun is-free-identifier-p (token)
  "Return true if the token is not a reserved word."
  (and (eq (token-type token) :identifier)
       (not (identifier-reservedp (token-content token)))))

(defun expected-error (parser what)
  "Report a missing element of the input."
  (Phase-2-error parser "Sorry, I really wanted ~A here." what))

;; We often want to look for either a certain punctuator or a certain reserved
;; word. Matching on the former is easy, since we can check the type slot;
;; however, the token-type of reserved words is :identifier, so they must be
;; matched by logic as in is-reserved-word-p. Thus the concept of a ``token
;; designator'' is introduced. A token designator can be a keyword symbol, in
;; which case the token-type is checked, or a string, in which case
;; is-reserved-word-p is called.
(defun token-matches-p (token designator)
  "Return true if a token matches the given designator."
  (or (and (keywordp designator)
           (eq (token-type token) designator))
      (and (stringp designator)
           (is-reserved-word-p token designator))))

;; <program> ::= <program heading> <block> .
(defun parse-program (parser)
  "Parse a Pascal program."
  (parse-program-heading parser)
  (parse-block parser)
  (let ((token (get-next parser)))
    (unless (token-matches-p token :dot)
      (expected-error parser "a dot"))))

;; <program heading> ::= program <identifier> <program parameters> ;
(defun parse-program-heading (parser)
  "Parse a program's heading."
  (let ((token (get-next parser)))
    (if (token-matches-p token "program")
        (let ((program-name (get-next parser)))
          (cond ((token-matches-p program-name :identifier)
                 (begin-scope parser (token-content program-name))
                 (let ((token (get-next parser))
                       (parameters))
                   (when (token-matches-p token :left-parenthesis)
                     (setf parameters (parse-program-parameters parser)
                           token (get-next parser)))
                   (if (token-matches-p token :semicolon)
                       (attach-line-break token :indent :after)
                       (expected-error parser "a semicolon"))
                   (install-meaning parser (token-content program-name)
                                    'program-meaning
                                    :parameters parameters))
                 (attach-meaning parser program-name :defining))
                (t
                 (expected-error parser "an identifier"))))
        (expected-error parser "``program''"))))

;; <program parameters> ::= <empty>
;;                        | ( <file identifier> { , <file identifier } )
;;
;; <file identifier> ::= identifier
(defun parse-program-parameters (parser)
  "Install any program file parameters given in the program heading."
  (let ((type (type-type (get-meaning parser ; default type of the files
                                      (lookup-identifier "text"))))
        (token (get-next parser)))
    (loop do (cond ((token-matches-p token :identifier)
                    (install-meaning parser (token-content token)
                                     'parameter-meaning
                                     :program-parameter-p t
                                     :type type)
                    (attach-meaning parser token :declaring))
                   (t
                    (expected-error parser "an identifier")))
             (setf token (get-next parser))
          while (token-matches-p token :comma)
          do (setf token (get-next parser)))
    (unless (token-matches-p token :right-parenthesis)
      (expected-error parser "a right parenthesis"))))

;; <block> ::= <label declaration part>
;;             <constant definition part>
;;             <type definition part>
;;             <variable declaration part>
;;             <routine declaration part>
;;             <compound statement>
(defun parse-block (parser)
  "Parse a block, installing any declarations. On entry, the last token read
will have been a semicolon, and begin-scope will have been called."
  (let ((token (get-next parser)))
    (when (token-matches-p token "label")
      (parse-label-declarations parser)
      (setf token (get-next parser))
      (attach-line-break token :normal :before))
    (when (token-matches-p token "const")
      (parse-constant-definitions parser)
      (setf token (get-next parser))
      (attach-line-break token :normal :before))
    (when (token-matches-p token "type")
      (parse-type-definitions parser)
      (setf token (get-next parser))
      (attach-line-break token :normal :before))
    (when (token-matches-p token "var")
      (parse-variable-declarations parser)
      (setf token (get-next parser))
      (attach-line-break token :normal :before))
    (when (or (token-matches-p token "procedure")
              (token-matches-p token "function"))
      (parse-routine-declarations parser token)
      (setf token (get-next parser))
      (attach-line-break token :normal :before))
    (cond ((token-matches-p token "begin")
           (attach-line-break token :indent :after)
           (parse-compound-statement parser))
          (t
           (expected-error parser "a compound statement or a declaration")))))

;; <label declaration part> ::= label <constant> { , <constant> } ;
(defun parse-label-declarations (parser)
  "Parse the label declaration part of a block."
  (let ((token))
    (loop do (let ((first-token (get-next parser)))
               (put-back-token parser first-token)
               (let ((value (parse-label parser)))
                 (if (and value (integerp value))
                     (let ((label (install-label parser value))
                           (extra (ensure-extra first-token)))
                       (setf (extra-label-status extra) label))
                     (expected-error parser "a constant integral expression"))))
             (setf token (get-next parser))
          while (token-matches-p token :comma))
    (if (token-matches-p token :semicolon)
        (attach-line-break token :dedent :after)
        (expected-error parser "a semicolon"))))

;; <constant definition part> ::=
;;   const <constant definition> { ; <constant definition> } ;
;;
;; <constant definition> ::= <identifier> = <constant>
(defun parse-constant-definitions (parser)
  "Parse the constant definition part of a block."
  (let ((token (get-next parser)))
    (unless (is-free-identifier-p token)
      (expected-error parser "an identifier"))
    (loop while (is-free-identifier-p token) do
      (let ((name-token token))
        (setf token (get-next parser))
        (cond ((token-matches-p token :equals)
               (parse-expression parser)
               (install-meaning parser (token-content name-token)
                                'constant-meaning)
               (attach-meaning parser name-token :defining)
               (setf token (get-next parser))
               (cond ((token-matches-p token :semicolon)
                      (attach-line-break token :normal :after)
                      (setf token (get-next parser)))
                     (t
                      (expected-error parser "a semicolon"))))
              (t
               (expected-error parser "an equals sign")))))
    (put-back-token parser token)))

;; <type definition part> ::= type <type definition> { ; <type definition> } ;
;;
;; <type definition> ::= <identifier> = <type>
(defun parse-type-definitions (parser)
  "Parse the type definition part of a block."
  (let ((token (get-next parser))
        (indentedp nil))
    (unless (is-free-identifier-p token)
      (expected-error parser "an identifier"))
    (loop while (is-free-identifier-p token) do
      (let ((name-token token))
        (setf token (get-next parser))
        (cond ((token-matches-p token :equals)
               (let ((type (parse-type parser t (token-content name-token))))
                 (install-meaning parser (token-content name-token)
                                  'type-meaning
                                  :type (or type (make-instance 'Pascal-type)))
                 (attach-meaning parser name-token :defining)
                 (let ((entry (assoc (token-content name-token)
                                     (parser-state-pending-pointers parser))))
                   (when (cdr entry)
                     (loop for pending in (cdr entry) do
                       (setf (pointer-type-type pending) type))
                     (setf (cdr entry) nil))))
               (setf token (get-next parser))
               (cond ((token-matches-p token :semicolon)
                      (cond ((not indentedp)
                             (attach-line-break token :indent :after)
                             (setf indentedp t))
                            (t
                             (attach-line-break token :normal :after)))
                      (setf token (get-next parser))
                      (unless (or (token-matches-p token "var")
                                  (token-matches-p token "procedure")
                                  (token-matches-p token "function")
                                  (token-matches-p token "begin")
                                  (token-matches-p token :identifier))
                        (Phase-2-error parser "Unexpected symbol.")))
                     (t
                      (expected-error parser "a semicolon"))))
              (t
               (expected-error parser "an equals sign")))))
    (attach-line-break token :dedent :before)
    (put-back-token parser token)))

;; <type> ::= <simple type> | <structured type> | <pointer type>
;;
;; <pointer type> ::= ^ <type identifier>
;;
;; <type identifier> ::= <identifier>
;;
;; <structured type> ::= <unpacked structured type>
;;                     | packed <unpacked structured type>
;;
;; <unpacked structured type> ::= <array type>
;;                              | <record type>
;;                              | <set type>
;;                              | <file type>
(defun parse-type (parser type-definition-p &optional identifier)
  "Parse and return the representaton of a Pascal type. If the type-definition-p
parameter is true, then we are currently parsing type definitions, so forward
references are permitted in pointer types."
  (let ((token (get-next parser))
        (packedp nil))
    (when (token-matches-p token "packed")
      (setf packedp t
            token (get-next parser)))
    (cond ((token-matches-p token "array")
           (parse-array-type parser packedp type-definition-p))
          ((token-matches-p token "record")
           (attach-line-break token :indent :after)
           (parse-record-type parser identifier packedp type-definition-p))
          ((token-matches-p token "set")
           (setf token (get-next parser))
           (if (token-matches-p token "of")
               (make-instance 'set-type
                              :element-type (parse-simple-type parser)
                              :packedp packedp)
               (expected-error parser "``of''")))
          ((token-matches-p token "file")
           (setf token (get-next parser))
           (if (token-matches-p token "of")
               (make-instance 'file-type
                              :element-type (parse-type parser t)
                              :packedp packedp)
               (expected-error parser "``of''")))
          ((token-matches-p token :up-arrow)
           (when packedp
             (Phase-2-error parser "Only structured types can be packed."))
           (let ((identifier-token (get-next parser)))
             (if (is-free-identifier-p identifier-token)
                 (let ((meaning (get-meaning parser
                                             (token-content identifier-token)
                                             t)))
                   (cond ((and type-definition-p (not meaning))
                          (let ((type (make-instance 'pointer-type :type nil))
                                (entry (assoc (token-content token)
                                              (parser-state-pending-pointers parser))))
                            (if entry
                                (push type (cdr entry))
                                (push (cons (token-content identifier-token)
                                            (list type))
                                      (parser-state-pending-pointers parser)))
                            type))
                         ((not meaning)
                          (Phase-2-error parser "Unknown type name."))
                         (t
                          (attach-meaning parser identifier-token)
                          (make-instance 'pointer-type
                                         :type (type-type meaning)))))
                 (expected-error parser "an identifier"))))
          (t
           (when packedp
             (Phase-2-error parser "Only structured types can be packed."))
           (put-back-token parser token)
           (parse-simple-type parser)))))

;; <array type> ::= array [ <simple type> { , <simple type> } ] of <type>
(defun parse-array-type (parser packedp type-definition-p)
  "Parse the specification of an array type."
  (let ((subscripts (list))
        (token (get-next parser)))
    (cond ((token-matches-p token :left-bracket)
           (loop do (push (parse-simple-type parser) subscripts)
                    (setf token (get-next parser))
                 while (token-matches-p token :comma))
           (cond ((token-matches-p token :right-bracket)
                  (setf token (get-next parser))
                  (if (token-matches-p token "of")
                      (make-instance 'array-type
                                     :packedp packedp
                                     :subscripts (nreverse subscripts)
                                     :element-type (parse-type parser
                                                               type-definition-p))
                      (expected-error parser "``of''")))
                 (t
                  (expected-error parser "a right bracket"))))
          (t
           (expected-error parser "a left bracket")))))

;; <record type> ::= record <field list> end
;;
;; <field list> ::= <fixed part>
;;                | <fixed part> ; <variant part>
;;                | <variant part>
;;
;; <fixed part> ::= <record section> { ; <record section> }
;;
;; <record section> ::= <field identifier> { , <field identifier> } : <type>
;;                    | <empty>
;;
;; <variant part> ::=
;;   case <tag field> <type identifier> of <variant> { ; <variant> }
;;
;; <tag field> ::= <field identifier> : | <empty>
;;
;; <variant> ::= <case label list> : ( <field list> ) | <empty>
;;
;; <case label list> ::= <constant> { , <constant> }
(defun parse-record-type (parser identifier packedp type-definition-p)
  "Parse the specification of a record type."
  (let ((token (get-next parser)))
    (begin-scope parser identifier)
    (labels ((parse-variant ()
               (setf token (get-next parser))
               (unless (or (token-matches-p token :semicolon)
                           (token-matches-p token "end"))
                 (put-back-token parser token)
                 (loop do (parse-expression parser)
                          (setf token (get-next parser))
                       while (token-matches-p token :comma))
                 (cond ((token-matches-p token :colon)
                        (attach-line-break token :indent :after)
                        (setf token (get-next parser))
                        (cond ((token-matches-p token :left-parenthesis)
                               (setf token (get-next parser))
                               (parse-field-list) ; affects token
                               (if (token-matches-p token :right-parenthesis)
                                   (attach-line-break token :dedent :after)
                                   (expected-error parser
                                                   "a right parenthesis"))
                               (setf token (get-next parser)))
                              (t
                               (expected-error parser "a left parenthesis"))))
                       (t
                        (expected-error parser "a colon")))))
             (parse-variant-part ()
               (setf token (get-next parser)) ; get past ``case''
               (if (is-free-identifier-p token)
                   (let ((tag token))
                     (setf token (get-next parser))
                     (when (token-matches-p token :colon)
                       (let ((type (parse-type parser type-definition-p)))
                         (install-meaning parser (token-content tag)
                                          'field-meaning
                                          :tagp t
                                          :type type))
                       (setf token (get-next parser))))
                   (expected-error parser "a type identifier"))
               (unless (token-matches-p token "of")
                 (expected-error parser "``of''"))
               (attach-line-break token :normal :after)
                 ; as in parse-case-statement
               (loop do (parse-variant) ; affects token
                     while (token-matches-p token :semicolon)
                     do (attach-line-break token :normal :after)))
             (parse-record-section ()
               (let ((group (list)))
                 (loop do (unless (is-free-identifier-p token)
                            (expected-error parser "an identifier"))
                          (push token group)
                          (setf token (get-next parser))
                       while (token-matches-p token :comma)
                       do (setf token (get-next parser)))
                 (unless (token-matches-p token :colon)
                   (expected-error parser "a colon"))
                 (let ((type (parse-type parser type-definition-p)))
                   (loop for token in group
                         as identifier := (token-content token)
                         do (install-meaning parser identifier
                                             'field-meaning
                                             :type type)))))
             (parse-field-list ()
               (loop while (is-free-identifier-p token) do
                 (parse-record-section)
                 (setf token (get-next parser))
                 (when (token-matches-p token :semicolon)
                   (attach-line-break token :normal :after)
                   (setf token (get-next parser))))
               (when (token-matches-p token "case")
                 (parse-variant-part))))
      (parse-field-list))
    (unless (token-matches-p token "end")
      (expected-error parser "``end''"))
    (attach-line-break token :dedent :before)
    (let ((fields (environment-meanings (parser-state-environment parser))))
      (end-scope parser)
      (make-instance 'record-type
                     :fields fields
                     :packedp packedp))))

;; <simple type> ::= <scalar type> | <subrange type> | <type identifier>
;;
;; <scalar type> ::= ( <identifier> { , <identifier> } )
;;
;; <subrange type> ::= <constant> .. <constant>
(defun parse-simple-type (parser)
  "Parse a simple type."
  (let ((token (get-next parser)))
    (cond ((token-matches-p token :left-parenthesis)
           (let ((enumeration (make-instance 'enumeration-type))
                 (count 0))
             (loop do (setf token (get-next parser))
                      (cond ((is-free-identifier-p token)
                             (install-meaning parser (token-content token)
                                              'enumerator-meaning
                                              :ordinal count
                                              :parent enumeration)
                             (attach-meaning parser token :defining)
                             (push (get-meaning parser (token-content token))
                                   (enumeration-enumerators enumeration)))
                            (t
                             (expected-error parser "an identifier")))
                      (setf token (get-next parser))
                   while (token-matches-p token :comma))
             (unless (token-matches-p token :right-parenthesis)
               (expected-error parser "a right parenthesis"))
             enumeration))
          ((and (is-free-identifier-p token)
                (typep (get-meaning parser (token-content token))
                       'type-meaning))
           (attach-meaning parser token)
           (type-type (get-meaning parser (token-content token))))
          (t
           (put-back-token parser token)
           (let ((lower-bound (parse-expression parser)))
             (setf token (get-next parser))
             (if (token-matches-p token :subrange)
                 (make-instance 'subrange-type
                                :lower-bound lower-bound
                                :upper-bound (parse-expression parser))
                 (expected-error parser "``..''")))))))

;; <variable declaration part> ::=
;;   var <variable declaration> { ; <variable declaration> } ;
;;
;; <variable declaration> ::= <identifier> { , <identifier> } : <type>
(defun parse-variable-declarations (parser)
  "Parse the variable declarations for a block."
  (let ((token (get-next parser))
        (indentedp nil))
    (loop while (is-free-identifier-p token) do
      (let ((names (list)))
        (loop do (push token names)
                 (setf token (get-next parser))
              while (token-matches-p token :comma)
              do (setf token (get-next parser)))
        (cond ((token-matches-p token :colon)
               (let ((type (parse-type parser nil)))
                 (loop for name in names
                       as identifier := (token-content name)
                       do (install-meaning parser identifier
                                           'variable-meaning
                                           :type type)
                          (attach-meaning parser name :declaring)))
               (setf token (get-next parser)))
              (t
               (expected-error parser "a colon"))))
      (cond ((token-matches-p token :semicolon)
             (cond ((not indentedp)
                    (attach-line-break token :indent :after)
                    (setf indentedp t))
                   (t
                    (attach-line-break token :normal :after)))
             (setf token (get-next parser)))
            (t
             (expected-error parser "a semicolon"))))
    (attach-line-break token :dedent :before)
    (put-back-token parser token)))

;; <routine declaration part> ::= { <routine declaration> ; }
;;
;; <routine declaration> ::= <procedure declaration> | <function declaration>
;;
;; <procedure declaration> ::= <procedure heading> <block>
;;
;; <procedure heading> ::= procedure <identifier> <parameter part> ;
;;
;; <function declaration> ::= <function heading> <block>
;;
;; <function heading> ::=
;;   function <identifier> <parameter part> : <type identifier>
;;
;; <parameter part> ::= <empty>
;;                    | <parameter section> { ; <parameter section> }
;;
;; <parameter section> ::= <parameter group> | var <parameter group>
;;
;; <parameter group> ::= <identifier> { , <identifier> } : <type identifier>
(defun parse-routine-declarations (parser token)
  "Parse the procedure and function declarations for a block."
  (labels ((finish-routine-declaration (name name-token meaning procedurep)
             (let ((result-type nil))
               (unless procedurep
                 (unless (token-matches-p token :colon)
                   (expected-error parser "a colon"))
                 (setf token (get-next parser))
                 (unless (is-free-identifier-p token)
                   (expected-error parser "a type identifier"))
                 (attach-meaning parser token)
                 (setf result-type (get-meaning parser (token-content token))
                       token (get-next parser)))
               (unless (token-matches-p token :semicolon)
                 (expected-error parser "a semicolon"))
               (attach-line-break token :indent :after)
               (setf token (get-next parser))
               (let ((forwardp nil))
                 (when (and (is-free-identifier-p token)
                            (string= (identifier-name (token-content token))
                                     "forward"))
                   (setf forwardp t))
                 (cond (forwardp
                        (setf (routine-forwardp meaning) t))
                       (t
                        (when (routine-forwardp meaning)
                          (begin-scope parser name)
                          (loop for parameter in (routine-parameters meaning)
                                do (insert-meaning parser parameter)))
                        (put-back-token parser token)
                        (parse-block parser)
                        (when (routine-forwardp meaning)
                          (end-scope parser))))
                 (attach-meaning parser name-token (if forwardp
                                                       :declaring
                                                       :defining))))
             (setf token (get-next parser)))
           (parse-parameter-part ()
             (setf token (get-next parser))
             (let ((parameters (list)))
               (loop do (let ((by-reference-p nil))
                          (when (token-matches-p token "var")
                            (setf by-reference-p t
                                  token (get-next parser)))
                          (let ((names (list)))
                            (loop do (unless (is-free-identifier-p token)
                                       (expected-error parser
                                                       "an identifier"))
                                     (push token names)
                                     (setf token (get-next parser))
                                  while (token-matches-p token :comma)
                                  do (setf token (get-next parser)))
                            (cond ((token-matches-p token :colon)
                                   (setf token (get-next parser))
                                   (unless (is-free-identifier-p token)
                                     (expected-error parser
                                                     "a type identifier"))
                                   (let ((type (get-meaning parser
                                                            (token-content token))))
                                     (loop for name-token in names do
                                       (let ((name (token-content name-token)))
                                         (install-meaning parser name
                                                          'parameter-meaning
                                                          :by-reference-p by-reference-p
                                                          :type (type-type type))
                                         (attach-meaning parser
                                                         name-token
                                                         :declaring)
                                         (push (get-meaning parser name)
                                               parameters)))))
                                  (t
                                   (expected-error parser "a colon")))))
                        (setf token (get-next parser))
                     while (token-matches-p token :semicolon)
                     do (setf token (get-next parser)))
               (unless (token-matches-p token :right-parenthesis)
                 (expected-error parser "a right parenthesis"))
               parameters))
           (parse-routine-declaration ()
             (let ((procedurep (token-matches-p token "procedure")))
               (setf token (get-next parser))
               (cond ((is-free-identifier-p token)
                      (let* ((name-token token) ; saved for attach-meaning
                             (name (token-content name-token))
                             (meaning (get-meaning parser name t)))
                        (unless (and meaning
                                     (typep meaning 'routine-meaning)
                                     (= (meaning-level meaning)
                                        (environment-depth (parser-state-environment parser))))
                          (install-meaning parser name
                                           'routine-meaning)
                          (setf meaning (get-meaning parser name)))
                        (begin-scope parser name)
                        (setf token (get-next parser))
                        (when (token-matches-p token :left-parenthesis)
                          (setf (routine-parameters meaning)
                                (parse-parameter-part))
                          (setf token (get-next parser)))
                        (finish-routine-declaration name
                                                    name-token
                                                    meaning
                                                    procedurep))
                      (end-scope parser))
                     (t
                      (expected-error parser "an identifier"))))))
    (loop while (or (token-matches-p token "procedure")
                    (token-matches-p token "function")) do
      (parse-routine-declaration)
      (cond ((token-matches-p token :semicolon)
             (attach-line-break token :normal :after)
             (setf token (get-next parser)))
            (t
             (expected-error parser "a semicolon"))))
    (put-back-token parser token)))

;; <compound statement> ::= begin <statement> { ; <statement> } end
(defun parse-compound-statement (parser)
  "Parse a compound statement. The ``begin'' has already been read."
  (let ((token))
    (loop do (parse-statement parser)
             (setf token (get-next parser))
          while (token-matches-p token :semicolon)
          do (attach-line-break token :normal :after))
    (if (token-matches-p token "end")
        (attach-line-break token :dedent :before)
        (expected-error parser "``end''"))))

;; <statement> ::= <unlabelled statement> | <constant> : <unlabelled statement>
;;
;; <unlabelled statement> ::= <simple statement> | <structured statement>
;;
;; <simple statement> ::= <assignment statement>
;;                      | <procedure statement>
;;                      | <go to statement>
;;                      | <empty statement>
;;
;; <structured statement> ::= <compound statement>
;;                          | <if statement>
;;                          | <case statement>
;;                          | <while statement>
;;                          | <repeat statement>
;;                          | <for statement>
;;                          | <goto statement>
;;                          | <with statement>
(defun parse-statement (parser)
  "Parse a statement."
  (let ((token (get-next parser)))
    (maybe-parse-statement-label parser token)
    (setf token (get-next parser))
    (cond ((is-free-identifier-p token)
           (parse-assignment/call parser token))
          ((token-matches-p token "begin")
           (attach-line-break token :indent :after)
           (parse-compound-statement parser))
          ((token-matches-p token "if")
           (parse-if-statement parser))
          ((token-matches-p token "case")
           (parse-case-statement parser))
          ((token-matches-p token "while")
           (parse-while-statement parser))
          ((token-matches-p token "repeat")
           (attach-line-break token :indent :after)
           (parse-repeat-statement parser))
          ((token-matches-p token "for")
           (parse-for-statement parser))
          ((token-matches-p token "goto")
           (parse-go-to-statement parser))
          ((token-matches-p token "with")
           (parse-with-statement parser))
          (t
           (put-back-token parser token))))) ; no statement

(defun maybe-parse-statement-label (parser token)
  "Look for a label before a statement."
  (cond ((or (and (is-free-identifier-p token)
                  (typep (get-meaning parser (token-content token))
                         'constant-meaning))
             (token-matches-p token :decimal-integer)
             (token-matches-p token :plus)
             (token-matches-p token :minus)
             (token-matches-p token :hexadecimal-integer)
             (token-matches-p token :octal-integer))
         ;; The idea is that if a statement begins with one of these tokens,
         ;; then we must be looking at a label (a constant expression).
         (put-back-token parser token)
         (let ((value (parse-label parser))
               (extra (ensure-extra token)))
           (if value
               (setf (extra-label-status extra) (get-label parser value))
               (expected-error parser "a label")))
         (setf token (get-next parser))
         (unless (token-matches-p token :colon)
           (expected-error parser "a colon"))
         (attach-line-break token :normal :after))
        (t
         (put-back-token parser token))))

;; <assignment statement> ::= <variable> := <expression>
;;                          | <function identifier> := <expression>
;;
;; <procedure statement> ::= <procedure identifier> <argument part>
(defun parse-assignment/call (parser identifier-token)
  "Parse an assignment or a procedure call."
  (let* ((identifier (token-content identifier-token))
         (meaning (get-meaning parser identifier)))
    (cond ((typep meaning 'routine-meaning)
           (attach-meaning parser identifier-token)
           (if (routine-weirdp meaning)
               (do-weird-call parser meaning)
               (let ((token (get-next parser)))
                 (cond ((token-matches-p token :left-parenthesis)
                        (parse-argument-part parser))
                       ((token-matches-p token :becomes)
                        (parse-expression parser))
                       (t
                        (put-back-token parser token))))))
          (t
           (put-back-token parser identifier-token)
           (parse-variable parser)
           (let ((token (get-next parser)))
             (if (token-matches-p token :becomes)
                 (parse-expression parser)
                 (expected-error parser "`:='")))))))

;; <if statement> ::= if <expression> then <statement>
;;                  | if <expression> then <statement> else <statement
(defun parse-if-statement (parser)
  "Parse an if statement."
  (parse-expression parser)
  (let ((token (get-next parser)))
    (unless (token-matches-p token "then")
      (expected-error parser "``then''"))
    (attach-line-break token :indent :after)
    (parse-statement parser)
    (setf token (get-next parser))
    (attach-line-break token :dedent :before)
    (when (token-matches-p token "else")
      (attach-line-break token :indent :after)
      (parse-statement parser)
      (setf token (get-next parser))
      (attach-line-break token :dedent :before))
    (put-back-token parser token)))

;; <case statement> ::=
;;   case <expression> of <case list element> { ; <case list element> } end
;;
;; <case list element> ::= <case label list> : <statement> | <empty>
(defun parse-case-statement (parser)
  "Parse a case statement."
  (parse-expression parser)
  (let ((token (get-next parser)))
    (unless (token-matches-p token "of")
      (expected-error parser "``of''"))
    (attach-line-break token :normal :after) ; not :indent
    (setf token (get-next parser))
    (loop do (unless (or (token-matches-p token "end")
                         (token-matches-p token :semicolon))
               (loop do (cond ((and (is-free-identifier-p token)
                                    (string= (identifier-name (token-content token))
                                             "others"))
                               :do-nothing)
                              (t
                               (put-back-token parser token)
                               (parse-expression parser)))
                        (setf token (get-next parser))
                     while (token-matches-p token :comma)
                     do (setf token (get-next parser)))
               (cond ((token-matches-p token :colon)
                      (attach-line-break token :indent :after)
                      (parse-statement parser))
                     (t
                      (expected-error parser "a colon")))
               (setf token (get-next parser)))
          while (token-matches-p token :semicolon)
          do (attach-line-break token :normal :after)
             (setf token (get-next parser)))
    (unless (token-matches-p token "end")
      (expected-error parser "``end''"))
    (attach-line-break token :normal :after))) ; not :dedent

;; <while statement> ::= while <expression> do <statement>
(defun parse-while-statement (parser)
  "Parse a while statement."
  (parse-expression parser)
  (let ((token (get-next parser)))
    (cond ((token-matches-p token "do")
           (attach-line-break token :indent :after)
           (parse-statement parser))
          (t
           (expected-error parser "``do''")))))

;; <repeat statement> ::=
;;   repeat <statement> { ; <statement> } until <expression>
(defun parse-repeat-statement (parser)
  "Parse a repeat statement."
  (let ((token))
    (loop do (parse-statement parser)
             (setf token (get-next parser))
          while (token-matches-p token :semicolon)
          do (attach-line-break token :normal :after))
    (cond ((token-matches-p token "until")
           (attach-line-break token :dedent :before)
           (parse-expression parser))
          (t
           (expected-error parser "``until''")))))

;; <for statement> ::= for <identifier> := <for list> do <statement>
;;
;; <for list> ::= <expression> to <expression>
;;              | <expression> downto <expression>
(defun parse-for-statement (parser)
  "Parse a for statement."
  (let ((token (get-next parser)))
    (cond ((is-free-identifier-p token)
           (attach-meaning parser token)
           (setf token (get-next parser))
           (cond ((token-matches-p token :becomes)
                  (parse-expression parser)
                  (setf token (get-next parser))
                  (cond ((or (token-matches-p token "to")
                             (token-matches-p token "downto"))
                         (parse-expression parser)
                         (setf token (get-next parser))
                         (cond ((token-matches-p token "do")
                                (attach-line-break token :indent :after)
                                (parse-statement parser)
                                (setf token (get-next parser))
                                (attach-line-break token :dedent :before)
                                (put-back-token parser token))
                               (t
                                (expected-error parser "``do''"))))
                        (t
                         (expected-error parser "``to'' or ``downto''"))))
                 (t
                  (expected-error parser "``:=''"))))
          (t
           (expected-error parser "an identifier")))))

;; <go to statement> ::= goto <label>
(defun parse-go-to-statement (parser)
  "Parse a go to statement."
  (let* ((token (get-next parser))
         (extra (ensure-extra token)))
    (put-back-token parser token)
    (let ((label (get-label parser (parse-label parser))))
      (setf (extra-label-status extra) label))))

;; Note that this duplicates logic from scan-definition-part.
(defun parse-label (parser)
  "Parse a ``label'' (really a constant expression)."
  (let ((token)
        (value 0)
        (next-sign +1))
    (flet ((add-in (value)
             (incf value (* next-sign value))
             (setf next-sign +1))
           (mark-label ()
             (setf (extra-label-status (ensure-extra token)) t)))
      (loop do
        (setf token (get-next parser))
        (case (token-type token)
          ((:decimal-integer
            :octal-integer
            :hexadecimal-integer)
           (add-in (token-content token))
           (mark-label))
          ((:identifier)
           (unless (is-free-identifier-p token)
             (put-back-token parser token)
             (return))
           (let ((meaning (get-meaning parser
                                       (token-content token))))
             (unless (and (typep meaning 'constant-meaning)
                          (constant-numeric-macro-p meaning))
               (Phase-2-error parser "Bad label."))
             (attach-meaning parser token)
             (add-in (constant-value meaning))
             (mark-label)))
          ((:minus)
           (setf next-sign (- next-sign))
           (mark-label))
          ((:plus)
           (mark-label))
          (otherwise
           (put-back-token parser token)
           (return)))))
    value))

;; <with statement> ::= with <record variable list> do <statement>
;;
;; <record variable list> ::= <record variable> { , <record variable> }
(defun parse-with-statement (parser)
  "Parse a with statement."
  (let ((depth 0) ; number of scopes created (1 per record variable)
        (token))
    (loop do (let ((type (parse-variable parser)))
               (cond ((typep type 'record-type)
                      (begin-scope parser nil)
                      (incf depth)
                      (let ((environment (parser-state-environment parser))
                            (fields (record-fields type)))
                        (setf (environment-meanings environment) fields)))
                     (t
                      (Phase-2-error parser
                                     "Non-record in ``with'' statement."))))
             (setf token (get-next parser))
          while (token-matches-p token :comma))
    (cond ((token-matches-p token "do")
           (attach-line-break token :indent :after)
           (parse-statement parser)
           (setf token (get-next parser))
           (attach-line-break token :dedent :before)
           (put-back-token parser token))
          (t
           (expected-error parser "``do''")))
    (loop repeat depth do
      (end-scope parser))))

;; <variable> ::= <identifier>
;;              | <component variable>
;;              | <referenced variable>
;;
;; <component variable> ::= <indexed variable>
;;                        | <field designator>
;;                        | <file buffer>
;;
;; <indexed variable> ::= <identifier> [ <expression> { , <expression> } ]
;;
;; <field designator> ::= <variable> . <identifier>
;;
;; <file buffer> ::= <variable> ^
;;
;; <referenced variable> ::= <variable> ^
(defun parse-variable (parser)
  "Parse a variable. On entry, the next token will be an identifier. The return
value is the type of the variable, which is used only by parse-with-statement."
  (let* ((token (get-next parser))
         (meaning (get-meaning parser (token-content token))))
    (attach-meaning parser token)
    (unless (typep meaning '(or variable-meaning field-meaning))
      (expected-error parser "a variable"))
    (let ((type (if (typep meaning 'variable-meaning)
                    (variable-type meaning)
                    (field-type meaning)))
          (token (get-next parser)))
      (loop do ; while the token is :dot, :up-arrow, or :left-bracket
        (cond ((token-matches-p token :dot)
               (setf token (get-next parser))
               (if (is-free-identifier-p token)
                   (cond ((typep type 'record-type)
                          (attach-field-meaning parser token type)
                          (let ((meaning (get-field-meaning (token-content token)
                                                            type)))
                            (setf type (field-type meaning))))
                         (t
                          (Phase-2-error parser "Field on non-record.")))
                   (expected-error parser "a field identifier")))
              ((token-matches-p token :up-arrow)
               (cond ((typep type 'pointer-type)
                      (setf type (pointer-type-type type)))
                     ((typep type 'file-type)
                      (setf type (file-element-type type)))
                     (t
                      (Phase-2-error parser "Invalid up arrow."))))
              ((token-matches-p token :left-bracket)
               (cond ((typep type 'array-type)
                      (setf type (Pascal-array-element-type type))
                      (loop do (parse-expression parser)
                               (setf token (get-next parser))
                            while (token-matches-p token :comma))
                      (unless (token-matches-p token :right-bracket)
                        (expected-error parser "a right bracket")))
                     (t
                      (Phase-2-error parser "Subscript on non-array."))))
              (t
               (put-back-token parser token)
               (return)))
        (setf token (get-next parser)))
      type)))

;; <argument part> ::= ( <expression> { , <expression> } )
(defun parse-argument-part (parser)
  "Parse the argument list in a call expression or statement. On entry, a left
parenthesis will have been read."
  (let ((token))
    (loop do (parse-expression parser)
             (setf token (get-next parser))
          while (token-matches-p token :comma))
    (unless (token-matches-p token :right-parenthesis)
      (expected-error parser "a right parenthesis"))))

;; Why, oh why, did Wirth decide that this needed to be a part of the language?
(defun do-weird-call (parser meaning)
  "Handle a call to a weird standard procedure/function."
  (let ((name (identifier-name (meaning-identifier meaning)))
        (token (get-next parser)))
    (cond ((member name '("eof" "eoln" "read_ln" "readln") :test #'string=)
           (if (token-matches-p token :left-parenthesis)
               (parse-argument-part parser)
               (put-back-token parser token)))
          ((member name '("write" "write_ln" "writeln") :test #'string=)
           (cond ((token-matches-p token :left-parenthesis)
                  (loop do (parse-expression parser)
                           (setf token (get-next parser))
                           (loop while (token-matches-p token :colon) do
                             (parse-expression parser)
                             (setf token (get-next parser)))
                        while (token-matches-p token :comma))
                  (unless (token-matches-p token :right-parenthesis)
                    (expected-error parser "a right parenthesis")))
                 ((string= name "write")
                  (Phase-2-error parser "No arguments given for write."))
                 (t
                  (put-back-token parser token)))))))

;; <expression> ::= <simple expression>
;;                | <simple expression> <relational operator> <simple expression>
;;
;; <relational operator> ::= = | <> | < | <= | >= | > | in
(defun parse-expression (parser)
  "Parse an expression. On entry, the next token will be the first token of the
expression; on exit, the next token will be the first token after the
expression."  
  (parse-simple-expression parser)
  (let ((token (get-next parser)))
    (if (or (member (token-type token) '(:equals
                                         :not-equal
                                         :less-than
                                         :less-than-or-equal-to
                                         :greater-than-or-equal-to
                                         :greater-than))
            (token-matches-p token "in"))
        (parse-simple-expression parser)
        (put-back-token parser token))))

;; <simple expression> ::= <term>
;;                       | <simple expression> <adding operator> <term>
;;
;; <adding operator> ::= + | - | or
(defun parse-simple-expression (parser)
  "Parse a simple expression."
  (let ((token))
    (loop do (parse-term parser)
             (setf token (get-next parser))
          while (or (token-matches-p token :plus)
                    (token-matches-p token :minus)
                    (token-matches-p token "or")))
    (put-back-token parser token)))

;; <term> ::= <factor>
;;          | <sign> <term>
;;          | <term> <multiplying operator> <term>
;;
;; <multiplying operator> ::= * | / | div | mod | and
;;
;; <sign> ::= + | -
(defun parse-term (parser)
  "Parse a term of an expression."
  (let ((token (get-next parser)))
    (unless (or (token-matches-p token :minus)
                (token-matches-p token :plus))
      (put-back-token parser token))
    (loop do (parse-factor parser)
             (setf token (get-next parser))
          while (or (token-matches-p token :times)
                    (token-matches-p token :solidus)
                    (token-matches-p token "div")
                    (token-matches-p token "mod")
                    (token-matches-p token "and")))
    (put-back-token parser token)))

;; <factor> ::= <variable>
;;            | <unsigned constant>
;;            | ( <expression> )
;;            | <function designator>
;;            | <set>
;;            | not <factor>
;;
;; <unsigned constant> ::= <unsigned number>
;;                       | <string>
;;                       | <constant identifier>
;;                       | nil
;;
;; <function designator> ::= <function identifier>
;;                         | <function identifier> <argument part>
;;
;; <function identifier> ::= <identifier>
;;
;; <set> ::= [ <element list> ]
;;
;; <element list> ::= <element> { , <element> } | <empty>
;;
;; <element> ::= <expression> | <expression> .. <expression>
(defun parse-factor (parser)
  "Parse a factor of an expression."
  (let ((token (get-next parser)))
    (case (token-type token)
      ((:left-parenthesis)
       (parse-expression parser)
       (setf token (get-next parser))
       (unless (token-matches-p token :right-parenthesis)
         (expected-error parser "a right parenthesis")))
      ((:left-bracket)
       (setf token (get-next parser))
       (unless (token-matches-p token :right-bracket) ; allow empty set
         (put-back-token parser token)
         (loop do (parse-expression parser)
                  (setf token (get-next parser))
                  (when (token-matches-p token :subrange)
                    (parse-expression parser)
                    (setf token (get-next parser)))
               while (token-matches-p token :comma)))
       (unless (token-matches-p token :right-bracket)
         (expected-error parser "a right bracket")))
      ((:Pascal-string
        :WEB-string
        :decimal-integer
        :octal-integer
        :hexadecimal-integer
        :check-sum
        :floating-point-literal)
       :do-nothing)
      ((:identifier)
       (cond ((is-free-identifier-p token)
              (let ((meaning (get-meaning parser (token-content token))))
                (typecase meaning
                  (routine-meaning
                   (attach-meaning parser token)
                   (if (routine-weirdp meaning)
                       (do-weird-call parser meaning)
                       (unless (null (routine-parameters meaning))
                         (let ((token (get-next parser)))
                           (if (token-matches-p token :left-parenthesis)
                               (parse-argument-part parser)
                               (expected-error parser
                                               "a left parenthesis"))))))
                  ((or variable-meaning field-meaning)
                   (put-back-token parser token)
                   (parse-variable parser))
                  ((or constant-meaning enumerator-meaning)
                   (attach-meaning parser token))
                  (unknown-meaning
                   :do-nothing)
                  (t
                   (Phase-2-error parser "Bad identifier in expression.")))))
             ((token-matches-p token "nil")
              :do-nothing)
             ((token-matches-p token "not")
              (parse-factor parser))
             (t
              (Phase-2-error parser "Unexpected symbol."))))
      (otherwise
       (Phase-2-error parser "Unexpected symbol.")))))

(defun install-standard-identifiers (parser)
  "Put Pascal-H's predeclared identifiers into the parser's environment."
  (labels ((identify (name &rest arguments)
             (apply #'install-meaning parser
                                      (lookup-identifier name)
                                      arguments))
           (identify-type (name standardp type)
             (identify name
                       'type-meaning
                       :standardp standardp
                       :type type))
           (identify-routine (name &optional parameters result-type)
             (identify name
                       'routine-meaning
                       :parameters parameters
                       :result-type result-type))
           (identify-weird-routine (name)
             (identify name
                       'routine-meaning
                       :weirdp t)))
    (identify-type "char" t (make-instance 'subrange-type))
    (identify-type "integer" t (make-instance 'subrange-type))
    (identify-type "real" t (make-instance 'subrange-type))
    (identify-type "boolean" t (make-instance 'enumeration-type))
    (identify-type "text"
                   t
                   (make-instance 'file-type
                                  :packedp nil
                                  :element-type (get-meaning parser
                                                             (lookup-identifier "char"))))
    (identify "true"
              'enumerator-meaning
              :standardp t)
    (identify "false"
              'enumerator-meaning
              :standardp t)
    (identify-weird-routine "dispose")
    (identify-weird-routine "eof")
    (identify-weird-routine "eoln")
    (identify-weird-routine "read_ln")
    (identify-weird-routine "readln")
    (identify-weird-routine "write")
    (identify-weird-routine "write_ln")
    (identify-weird-routine "writeln")
    ;; Currently the parser cares only about whether a routine has parameters,
    ;; without regard for their specification. (This is why ``new'', ``pack'',
    ;; and ``unpack'' are not considered ``weird''.)
    (identify-routine "abs" (list t))
    (identify-routine "arctan" (list t))
    (identify-routine "break" (list t))
    (identify-routine "break_in" (list t))
    (identify-routine "chr" (list t))
    (identify-routine "close" (list t))
    (identify-routine "erstat" (list t))
    (identify-routine "get" (list t))
    (identify-routine "ln" (list t))
    (identify-routine "odd" (list t))
    (identify-routine "ord" (list t))
    (identify-routine "new" (list t))
    (identify-routine "page" (list t))
    (identify-routine "pack" (list t))
    (identify-routine "pred" (list t))
    (identify-routine "put" (list t))
    (identify-routine "read" (list t))
    (identify-routine "reset" (list t))
    (identify-routine "rewrite" (list t))
    (identify-routine "round" (list t))
    (identify-routine "round" (list t))
    (identify-routine "sin" (list t))
    (identify-routine "sqr" (list t))
    (identify-routine "sqrt" (list t))
    (identify-routine "succ" (list t))
    (identify-routine "trunc" (list t))
    (identify-routine "unpack" (list t))))

(defun install-reserved-words ()
  "Insert Pascal's reserved words into the symbol table."
  ;; Uncomment the following expression for TeX.
  #|(setf (identifier-synonym (lookup-identifier "mtype")) "type")|#
  (mapcar (lambda (name)
            (setf (identifier-reservedp (lookup-identifier name)) t))
          '("and"
            "array"
            "begin"
            "case"
            "const"
            "div"
            "do"
            "downto"
            "else"
            "end"
            "file"
            "for"
            "function"
            "goto"
            "if"
            "in"
            "label"
            "mod"
            "nil"
            "not"
            "of"
            "or"
            "packed"
            "procedure"
            "program"
            "record"
            "repeat"
            "set"
            "then"
            "to"
            ;; Change this to "mtype" for TeX.
            "type"
            "until"
            "var"
            "while"
            "with")))

(defun Phase-2 ()
  "Parse and annotate the user's program."
  (install-reserved-words)
  (let ((parser (make-parser-state)))
    (install-macros parser)
    (install-standard-identifiers parser)
    (begin-module parser (get-unnamed))
    (parse-program parser)))
