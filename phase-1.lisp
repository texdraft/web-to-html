;;;; Phase 1 processing (input/lexical analysis).
(in-package :WEB-to-HTML)

;;; Basic input.

;; The input-has-ended and new-section conditions are of the utmost importance
;; to the overall flow of Phase 1
(define-condition input-has-ended ()
  ()
  (:documentation
    "Signaled when there is no more input."))

(define-condition new-section ()
  ((starredp
    :type boolean
    :initform nil
    :initarg :starredp
    :reader new-section-starredp))
  (:documentation
    "Signaled when the beginning of a new section is found"))

;; The change file malarkey is mostly taken straight from TANGLE/WEAVE, with
;; alterations for the elimination of the globals buffer, loc, and limit. I'd
;; recommend skipping it and going to the line containing
;; ``;;; Lexical analysis.''; all you need to know is that get-line returns the
;; next line of input, and you can mostly ignore the slots of input-state.

(defstruct input-state
  (line 0 :type fixnum) ; current line number in the current file
  (WEB-file nil :type stream) ; primary input
  (change-file nil :type (or stream null)) ; updates
  (other-line 0 :type fixnum) ; line number in the other file
  (change-line-text nil :type (or string null)) ; saved line from the change file
  (changing-is-done-p nil :type boolean) ; has the change file ended?
  (changingp t :type boolean) ; is input coming from from the change file?
  (change-pending-p nil :type boolean))
    ; are we waiting to see whether the current section should be marked as changed?

(defun change-changing (input)
  (setf (input-state-changingp input) (not (input-state-changingp input)))
  (rotatef (input-state-line input) (input-state-other-line input)))

(defun initialize-input (WEB-file change-file)
  "Set up the input state, given the streams for the input files. A change file
is optional."
  (let ((input (make-input-state :WEB-file WEB-file
                                 :change-file change-file)))
    (look-for-change input)
    (change-changing input)
    input))

;; The input-error function is defined later.

(defun look-for-change (input)
  "Look ahead in the change file for the next change."
  (let ((text))
    (loop do (incf (input-state-line input))
             (setf text (read-line (input-state-change-file input) nil nil))
             (when (and text
                        (>= (length text) 2)
                        (char= (schar text 0) #\@)
                        (member (schar text 1) '(#\y #\z) :test #'char-equal))
               (input-error input "Where is the matching @x?"))
          while text
          until (and (>= (length text) 2)
                     (char= (schar text 0) #\@)
                     (char-equal (schar text 1) #\x)))
    (unless text
      (setf (input-state-changing-is-done-p input) t)
      (return-from look-for-change))
    (loop do (incf (input-state-line input))
             (setf text (read-line (input-state-change-file input) nil nil))
             (unless text
               (input-error input "Change file ended after @x."))
          while text
          until (>= (length text) 1))
    (setf (input-state-change-line-text input) text)))

(defun check-change (input WEB-text)
  "Switch to the change file if appropriate. The second argument is the current
line of the WEB file."
  (assert (not (input-state-changingp input)))
  (let ((text (input-state-change-line-text input)))
    (when (or (not text)
              (string/= text WEB-text))
      (return-from check-change))
    (setf (input-state-change-pending-p input) nil)
    (let ((trimmed (string-left-trim '(#\Space #\Tab #\Newline) text)))
      (when (and (>= (length trimmed) 2)
                 (char= (schar trimmed 0) #\@)
                 (or (member (schar trimmed 1) '(#\Space #\Tab #\Newline)
                             :test #'char=)
                     (char= (schar trimmed 1) #\*)))
        (setf (input-state-change-pending-p input) t)))
    (unless (input-state-change-pending-p input)
      (setf (section-changedp (get-current-section)) t))
    (let ((discrepancies 0))
      (loop do (change-changing input) ; now it's true
               (incf (input-state-line input))
               (setf text (read-line (input-state-change-file input) nil nil))
               (unless text
                 (input-error input "Change file ended before @y.")
                 (setf (input-state-changing-is-done-p input) t)
                 (change-changing input) ; false again
                 (return-from check-change))
               (when (and (>= (length text) 2)
                          (char= (schar text 0) #\@)
                          (member (schar text 1) '(#\x #\z) :test #'char-equal))
                 (input-error input "Where is the matching @y?"))
            until (and (>= (length text) 2)
                       (char= (schar text 0) #\@)
                       (char-equal (schar text 1) #\y))
            do (change-changing input) ; now it's false
               (incf (input-state-line input))
               (setf WEB-text (read-line (input-state-WEB-file input) nil nil))
               (unless WEB-text
                 (input-error input "WEB file ended during a change.")
                 (signal 'input-has-ended))
               (unless (string= text WEB-text)
                 (incf discrepancies)))
      (when (> discrepancies 0)
        (input-error input "Hmm... ~D of the preceding lines failed to match."
                           discrepancies)))))

(defun get-line (input)
  "Get the next line of input."
  (incf (input-state-line input))
  (let ((text))
    (when (input-state-changingp input)
      (setf text (read-line (input-state-change-file input) nil nil))
      (unless text
        (input-error input "Change file ended without @z.")
        (setf text "@z"))
      (when (>= (length text) 2)
        (when (input-state-change-pending-p input)
          (let ((trimmed (string-left-trim '(#\Space #\Tab #\Newline) text)))
            (when (and (>= (length trimmed) 2)
                       (char= (schar trimmed 0) #\@)
                       (or (member (schar trimmed 1) '(#\Space #\Tab #\Newline)
                                   :test #'char=)
                           (char= (schar trimmed 1) #\*)))
              (setf (input-state-change-pending-p input) nil)))
          (when (input-state-change-pending-p input)
            (setf (section-changedp (get-current-section)) t
                  (input-state-change-pending-p input) nil)))
        (when (char= (schar text 0) #\@)
          (cond ((member (schar text 1) '(#\x #\y) :test #'char-equal)
                 (input-error input "Where is the matching @z?"))
                ((char-equal (schar text 1) #\z)
                 (look-for-change input)
                 (change-changing input))))))
    (unless (input-state-changingp input)
      (setf text (read-line (input-state-WEB-file input) nil nil))
      (unless text
        (signal 'input-has-ended))
      (unless (input-state-changing-is-done-p input)
        (check-change input text)
        (when (input-state-changingp input)
          (setf text (get-line input)))))
    text))

;;; Lexical analysis.
(defstruct lexer-state
  ;; The input-state slot is nil when a module name is being tokenized.
  (input-state nil :type (or input-state null)) ; low-level details
  ;; The only place where buffer = nil is relevant is in next-character-file.
  (buffer nil :type (or string null)) ; text of line or module name; initially nil
  (position 0 :type fixnum) ; current place in line or module name

  ;; Contributions are stored in reverse order as the lists are being built.
  (TeX-part (list) :type list)
  (definition-part (list) :type list)
  (Pascal-part (list) :type list)
  (name-tokens (list) :type list)
  (balance 0 :type fixnum) ; number of unmatched left braces in TeX text
  (in-comment-p nil :type boolean) ; are we currently reading a comment?
  (in-inline-code-p nil :type boolean) ; are we reading inline code?
  (in-TeX-part-p nil :type boolean) ; are we reading the TeX part of a section?
  (in-definition-part-p nil :type boolean)
    ; are we reading the definition part of a section?
  (in-Pascal-part-p nil :type boolean) ; are reading the Pascal part of a section?
  (in-module-name-p nil :type boolean) ; are we reading a module name?
  (module nil :type (or module null)) ; module whose name we are currently reading

  ;; We want to be able to use the lexing routines for tokenizing both normal
  ;; text and module names. So while reading from the input file, next-character
  ;; and previous-character are set to the functions next-character-file and
  ;; next-line-file below, but when a section name is being tokenized, they are
  ;; set to functions that move ahead in the name (which will be a string). The
  ;; in-module-name-p slot is true only in this latter situation.
  (next-character nil :type (function (lexer-state) character))
    ; called to get the next character
  (previous-character nil :type (function (lexer-state) character)))
    ; called to move back to the previous character

;; Only one of in-TeX-part-p, in-definition-part-p, in-module-name-p, and
;; in-Pascal-part-p will be true at any given time; and they cannot all be
;; false. When in-inline-code-p and in-comment-p are both true, in-TeX-part-p
;; and in-module-name-p will be false.

(defun input-error (state control &rest arguments)
  "Report a problem occurring in Phase 1. The state argument can be either an
input-state object or a lexer-state object; it is the former only during the
low-level process of change file merging."
  (format *error-output* "! ~?~%" control arguments)
  (flet ((report-input-state (input)
           (format *error-output* "  In line ~D of ~:[WEB~;change~] file"
                                  (input-state-line input)
                                  (input-state-changingp input))))
    (cond ((lexer-state-p state)
           (cond ((lexer-state-in-module-name-p state)
                  (format *error-output* "  In section name <~A>."
                                         (module-name (lexer-state-module state))))
                 (t
                  (report-input-state (lexer-state-input-state state))
                  ;; We don't need to add a line break after printing the
                  ;; buffer, because next-line-file always adds #\Newline to the
                  ;; end of every line.
                  (format *error-output* ":~%~A" (lexer-state-buffer state))
                  (loop repeat (lexer-state-position state) do
                    (write-char #\Space *error-output*))
                  (write-char #\^ *error-output*))))
          (t
           (report-input-state state)
           (write-char #\. *error-output*))))
  (terpri *error-output*))

(defun clear-lexer-state (lexer)
  "Empty a lexer-state object, returning the tokens it collected."
  (multiple-value-prog1 (if (lexer-state-in-module-name-p lexer)
                            (nreverse (lexer-state-name-tokens lexer))
                            (values (nreverse (lexer-state-TeX-part lexer))
                                    (nreverse (lexer-state-definition-part lexer))
                                    (nreverse (lexer-state-Pascal-part lexer))))
                        (setf (lexer-state-TeX-part lexer) (list)
                              (lexer-state-definition-part lexer) (list)
                              (lexer-state-Pascal-part lexer) (list)
                              (lexer-state-in-comment-p lexer) nil
                              ;; The in-module-name-p slot need not be reset.
                              ;; (In fact, it shouldn't be, because Phase-1
                              ;; might call input-error after this function.)
                              (lexer-state-in-inline-code-p lexer) nil
                              (lexer-state-in-TeX-part-p lexer) nil
                              (lexer-state-in-definition-part-p lexer) nil
                              (lexer-state-in-Pascal-part-p lexer) nil)))

(defun initialize-lexer-state (input next-character previous-character)
  (make-lexer-state :input-state input
                    :next-character next-character
                    :previous-character previous-character))

;; We are expecting TeX text
;;         - when in a section's TeX part but not in inline code within it,
;;         - when in a comment but not in inline code within the comment, and
;;         - when in a module name but not in inline code within the name;
;; otherwise we are expecting Pascal text. Comments and inline code cannot nest,
;; so that ``|Pascal{\TeX}Pascal|'' is an error. (This implies that when both
;; in-inline-code-p and in-comment-p are true, we must be reading inline code
;; within a comment in Pascal code.)
(defun expecting-TeX-p (lexer)
  "Should we be looking for a :TeX-portion or for Pascal code?"
  (or (and (lexer-state-in-TeX-part-p lexer)
           (not (lexer-state-in-inline-code-p lexer)))
      (and (not (lexer-state-in-inline-code-p lexer))
           (lexer-state-in-comment-p lexer))
      (and (lexer-state-in-module-name-p lexer)
           (not (lexer-state-in-inline-code-p lexer)))))

(defun next-line-file (lexer)
  "Moves to the next line of input and updates the lexer state."
  (setf (lexer-state-buffer lexer) (concatenate 'string
                                                (get-line (lexer-state-input-state lexer))
                                                (string #\Newline))
        (lexer-state-position lexer) 0))

(defun next-character-file (lexer)
  "Moves to the next character of the input file and updates the lexer state."
  (if (or (not (lexer-state-buffer lexer))
          (char= (schar (lexer-state-buffer lexer)
                        (lexer-state-position lexer))
                 #\Newline))
      (next-line-file lexer)
      (incf (lexer-state-position lexer)))
  (schar (lexer-state-buffer lexer) (lexer-state-position lexer)))

(defun previous-character-file (lexer)
  (assert (> (lexer-state-position lexer) 0))
    ; don't move back at the beginning of a line
  (decf (lexer-state-position lexer))
  (schar (lexer-state-buffer lexer) (lexer-state-position lexer)))

(defun next-character (lexer)
  (funcall (lexer-state-next-character lexer) lexer))

(defun previous-character (lexer)
  (funcall (lexer-state-previous-character lexer) lexer))

(defun capture-origin (lexer)
  "Construct an origin object from the current lexer state."
  (if (lexer-state-in-module-name-p lexer)
      (make-origin :from-module-name-p t
                   :module (lexer-state-module lexer))
      (make-origin :section-number (get-section-count)
                   :line (input-state-line (lexer-state-input-state lexer))
                   :from-change-file-p (input-state-changingp (lexer-state-input-state lexer)))))

(defun contribute (lexer &key type origin content)
  (let ((token (make-token :type type
                           :origin origin
                           :content content)))
    (cond ((lexer-state-in-TeX-part-p lexer)
           (push token (lexer-state-TeX-part lexer)))
          ((lexer-state-in-definition-part-p lexer)
           (push token (lexer-state-definition-part lexer)))
          ((lexer-state-in-Pascal-part-p lexer)
           (push token (lexer-state-Pascal-part lexer)))
          (t
           (push token (lexer-state-name-tokens lexer))))))

;; Unlike in TANGLE, tokens aren't read one-at-a-time. Instead, we gather them
;; until a big change in state happens---namely, the beginning of a section or
;; the end of input.
;; You might like to think of read-Pascal and read-TeX as coroutines. The
;; ``return'' statements below are effectively yields; the read-text function
;; manages switching between the two routines. An earlier design implemented the
;; coroutining aspect by having read-Pascal and read-TeX call each other,
;; causing the call stack to grow as we switch from Pascal to TeX text and vice
;; versa. The greatest depth reached while reading tex.web is around 670, in
;; section 585.
(defun read-Pascal (lexer)
  (loop do ; until end of file, beginning of section, or start of TeX text
    (let ((c (next-character lexer)))
      (loop while (member c '(#\Space #\Tab #\Newline) :test #'char=) do
        (setf c (next-character lexer)))
      (case c
        ((#\@)
         (control-code lexer))
        ((#\})
         (input-error lexer "Spurious end of comment."))
        ((#\{)
         (cond ((lexer-state-in-inline-code-p lexer)
                (input-error lexer "You can't have a comment in inline code."))
               (t
                (setf (lexer-state-in-comment-p lexer) t)
                (contribute lexer
                            :type :begin-comment
                            :origin (capture-origin lexer))
                (return)))) ; yield to read-TeX
        ((#\|)
         (cond ((lexer-state-in-inline-code-p lexer)
                (setf (lexer-state-in-inline-code-p lexer) nil)
                (contribute lexer
                            :type :end-inline-code
                            :origin (capture-origin lexer))
                (return)) ; yield to read-TeX
               (t
                (input-error lexer "What's a `|' doing here?"))))
        ((#\')
         (get-Pascal-string lexer))
        ((#\")
         (get-WEB-string lexer))
        (otherwise
         (cond ((digit-char-p c)
                (get-number c lexer))
               ((or (alpha-char-p c) (char= c #\_))
                (get-identifier c lexer))
               (t
                (get-punctuator c lexer))))))))

(defun read-TeX (lexer)
  (let ((portion (make-string-output-stream))
        (origin (capture-origin lexer))
        (c (next-character lexer)))
    (flet ((finish-portion ()
             (let ((text (get-output-stream-string portion)))
               ;; We can get an empty :TeX-portion if the input file ends during
               ;; the execution of control-code or just after finish-portion is
               ;; called.
               (when (> (length text) 0) ; don't add empty :TeX-portions
                 (contribute lexer
                             :type :TeX-portion
                             :origin origin
                             :content text)))))
      (handler-bind ((input-has-ended
                       (lambda (condition)
                         (declare (ignore condition))
                         (when (expecting-TeX-p lexer)
                           (finish-portion)))))
        (loop while (expecting-TeX-p lexer) do
          (case c
            ((#\|)
             (setf (lexer-state-in-inline-code-p lexer) t)
             (finish-portion)
             (contribute lexer
                         :type :begin-inline-code
                         :origin (capture-origin lexer))
             (return)) ; yield to read-Pascal
            ((#\\)
             (write-char #\\ portion)
             (write-char (next-character lexer) portion))
            ((#\{)
             (incf (lexer-state-balance lexer))
             (write-char c portion))
            ((#\})
             (cond ((> (lexer-state-balance lexer) 0)
                    (decf (lexer-state-balance lexer))
                    (write-char c portion))
                   ((lexer-state-in-comment-p lexer)
                    (setf (lexer-state-in-comment-p lexer) nil)
                    (finish-portion)
                    (contribute lexer
                                :type :end-comment
                                :origin (capture-origin lexer))
                    (return)) ; yield to read-Pascal
                   (t
                    (input-error lexer "Unbalanced right brace in TeX text."))))
            ((#\@)
             (finish-portion)
             (control-code lexer) ; a change in state might entail a ``yield'',
             (unless (expecting-TeX-p lexer) ; ...
               (return))) ; which is effected here
            (otherwise
             (write-char c portion)))
          (setf c (next-character lexer)))))))

(defun read-text (lexer)
  (loop do ; until end of file or start of section (that is, until
           ; input-has-ended or new-section is signaled)
    ;; The functions should alternate on each iteration. In other words, the
    ;; value of expecting-TeX-p must change every go through the loop. Thus this
    ;; test is not strictly necessary; the loop could simply be
    ;; (loop do (read-TeX) (read-Pascal)), because the initial state in a call
    ;; to read-text is always one in which TeX text is expected. However, I find
    ;; the current formulation to be clearer.
    (if (expecting-TeX-p lexer)
        (read-TeX lexer)
        (read-Pascal lexer))))

(defun control-code (lexer)
  (let ((c (next-character lexer)))
    (case c
      ((#\@)
       (cond ((expecting-TeX-p lexer)
              (contribute lexer
                          :type :TeX-portion
                          :origin (capture-origin lexer)
                          :content "@"))
             (t
              (input-error lexer "Random @@ in Pascal code.")
              (contribute lexer
                          :type nil
                          :origin (capture-origin lexer)))))
      ((#\')
       (get-octal lexer))
      ((#\")
       (get-hexadecimal lexer))
      ((#\$)
       (contribute lexer
                   :type :check-sum
                   :origin (capture-origin lexer)))
      ((#\*)
       (signal 'new-section :starredp t))
      ((#\Space #\Tab #\Newline)
       (signal 'new-section))
      ((#\!)
       (contribute lexer
                   :type :do-index
                   :origin (capture-origin lexer)))
      ((#\?)
       (contribute lexer
                   :type :don\'t-index
                   :origin (capture-origin lexer)))
      ((#\,)
       (contribute lexer
                   :type :thin-space
                   :origin (capture-origin lexer)))
      ((#\/ #\+ #\| #\0 #\1 #\2)
       :do-nothing)
      ((#\\)
       (contribute lexer
                   :type :Pascal-line-break
                   :origin (capture-origin lexer)))
      ((#\D #\d)
       (cond ((or (lexer-state-in-Pascal-part-p lexer)
                  (lexer-state-in-inline-code-p lexer))
              (input-error lexer "Unexpected @d."))
             (t
              (setf (lexer-state-in-TeX-part-p lexer) nil
                    (lexer-state-in-definition-part-p lexer) t)
              (contribute lexer
                          :type :begin-definition
                          :origin (capture-origin lexer)))))
      ((#\F #\f)
       (cond ((or (lexer-state-in-Pascal-part-p lexer)
                  (lexer-state-in-inline-code-p lexer))
              (input-error lexer "Unexpected @f."))
             (t
              (setf (lexer-state-in-TeX-part-p lexer) nil
                    (lexer-state-in-definition-part-p lexer) t)
              (contribute lexer
                          :type :begin-format
                          :origin (capture-origin lexer)))))
      ((#\P #\p)
       (when (or (lexer-state-in-Pascal-part-p lexer)
                 (lexer-state-in-inline-code-p lexer))
         (input-error lexer "Unexpected @p.")
         (when (and (lexer-state-in-inline-code-p lexer)
                    (lexer-state-in-TeX-part-p lexer))
            (setf (lexer-state-in-inline-code-p lexer) nil)))
       (setf (lexer-state-in-Pascal-part-p lexer) t
             (lexer-state-in-definition-part-p lexer) nil
             (lexer-state-in-TeX-part-p lexer) nil)
       (contribute lexer
                   :type :begin-Pascal
                   :origin (capture-origin lexer)))
      ((#\{)
       (contribute lexer
                   :type :begin-meta-comment
                   :origin (capture-origin lexer)))
      ((#\})
       (contribute lexer
                   :type :end-meta-comment
                   :origin (capture-origin lexer)))
      ((#\#)
       (contribute lexer
                   :type :extra-space
                   :origin (capture-origin lexer)))
      ((#\;)
       (contribute lexer
                   :type :pseudo-semicolon
                   :origin (capture-origin lexer)))
      ((#\&)
       (contribute lexer
                   :type :join
                   :origin (capture-origin lexer)))
      ((#\>)
       (input-error lexer "Extra @>."))
      (otherwise
       (unless (member c '(#\< #\= #\T #\t #\^ #\. #\: #\N #\n))
         (input-error lexer "Unknown control code.")
         (return-from control-code))
       (labels ((read-control-text (module-name-p)
                  (with-output-to-string (text)
                    (let ((seen-space-p nil)) ; was the previous character whitespace?
                      (loop do ; until @> or other control code (if not in module name)
                        (let ((c (next-character lexer)))
                          (cond ((char= c #\@)
                                 (let ((c (next-character lexer)))
                                   (case c
                                     ((#\@)
                                      (unless module-name-p
                                        (write-char #\@ text)
                                        (setf seen-space-p nil)))
                                     ((#\>)
                                      (return))
                                     (otherwise
                                      (cond ((not module-name-p)
                                             (input-error lexer
                                                          "Control codes are forbidden ~
                                                           in control text.")
                                             (return))
                                            (t ; errors will be detected later
                                             (write-char #\@ text)
                                             (write-char c text)))))))
                                ((and (not module-name-p)
                                      (char= c #\Newline))
                                 (input-error lexer "Control text didn't end."))
                                ((member c '(#\Space #\Tab #\Newline)
                                         :test #'char=)
                                 ;; Consecutive whitespace characters are
                                 ;; normalized to a single #\Space.
                                 (unless seen-space-p
                                   (setf seen-space-p t)
                                   (write-char #\Space text)))
                                (t
                                 (setf seen-space-p nil)
                                 (write-char c text))))))))
                (control-text (module-name-p) ; concession to indentation
                  (handler-case (read-control-text module-name-p)
                    (input-has-ended (condition)
                      (input-error lexer "Input ended in ~:[control text~;module name~]."
                                         module-name-p)
                      ;; No token will be stored, and no module name will enter
                      ;; the tree, since this signal unwinds the stack to
                      ;; scan-section. The lexer state's boolean slots will not
                      ;; change, but there won't be any further issues because
                      ;; we are at the end of input.
                      (signal condition)))))
         (cond ((char= c #\<)
                (let* ((origin (capture-origin lexer))
                       (name (control-text t))
                       (length (length name)))
                  (when (lexer-state-in-inline-code-p lexer)
                    (input-error lexer "Module name in inline code.")
                    (setf (lexer-state-in-inline-code-p lexer) nil))
                  (when (or (lexer-state-in-TeX-part-p lexer)
                            (lexer-state-in-definition-part-p lexer))
                    (setf (lexer-state-in-TeX-part-p lexer) nil
                          (lexer-state-in-definition-part-p lexer) nil
                          (lexer-state-in-Pascal-part-p lexer) t)
                    (contribute lexer
                                :type :begin-Pascal
                                :origin (capture-origin lexer)
                                :content t)) ; indicate named module
                  (if (and (> length 3)
                           (char= #\.
                                  (schar name (- length 1))
                                  (schar name (- length 2))
                                  (schar name (- length 3))))
                      (contribute lexer
                                  :type :module-name
                                  :origin origin
                                  :content (lookup-prefix (subseq name 0 (- length 3))))
                      (contribute lexer
                                  :type :module-name
                                  :origin origin
                                  :content (lookup-module name)))))
               ((char-equal c #\n)
                (let* ((text (control-text nil))
                       (value (parse-integer text :junk-allowed t))
                       (origin (capture-origin lexer)))
                  (if (or (not value)
                          ;; A check that the value is not greater than the
                          ;; number of sections can't be made here, since @n can
                          ;; refer to a section not yet scanned.
                          (<= value 0))
                      (input-error lexer "Invalid number in @n...@>")
                      (contribute lexer
                                  :type :long-distance
                                  :origin origin
                                  :content value))))
               (t
                (contribute lexer
                            :type (case c
                                    ((#\=)
                                     :verbatim-Pascal)
                                    ((#\T #\t)
                                     :verbatim-TeX)
                                    ((#\^)
                                     :index-Roman)
                                    ((#\.)
                                     :index-typewriter)
                                    ((#\:)
                                     :index-wildcard))
                            :origin (capture-origin lexer)
                            :content (control-text nil)))))))))

(flet ((get-string (delimiter lexer)
         (with-output-to-string (text)
           (loop do ; until we see the delimiter alone
             (let ((c (next-character lexer)))
               (case c
                 ((#\@)
                  (let ((c (next-character lexer)))
                    (unless (char= c #\@)
                      (input-error lexer
                                   "You should double @ signs in strings.")))
                  (write-char #\@ text)) ; store only one @
                 ((#\Newline)
                  (input-error lexer "String didn't end")
                  (return))
                 (otherwise
                  (if (char= c delimiter)
                      (let ((c (next-character lexer)))
                        (cond ((char= c delimiter)
                               (write-char delimiter text))
                              (t
                               (previous-character lexer)
                               (return))))
                      (write-char c text)))))))))
  (defun get-Pascal-string (lexer)
    (contribute lexer
                :type :Pascal-string
                :origin (capture-origin lexer)
                :content (get-string #\' lexer)))

  (defun get-WEB-string (lexer)
    (contribute lexer
                :type :WEB-string
                :origin (capture-origin lexer)
                :content (lookup-string (get-string #\" lexer)))))

(defun get-number (initial lexer)
  (let ((integer-value 0)
        (c initial))
    (loop for d := (digit-char-p c) while d do
      (setf integer-value (+ (* integer-value 10) d)
            c (next-character lexer)))
    (if (or (char= c #\.)
            (char-equal c #\e))
        (get-floating-point integer-value c lexer)
        (contribute lexer
                    :type :decimal-integer
                    :origin (capture-origin lexer)
                    :content integer-value))
    (previous-character lexer)))

(defun get-floating-point (whole-part c lexer)
  (let ((fractional-part 0)
        (exponent-sign :plus)
        (exponent-part 0))
    (when (char= c #\.)
      (loop for c := (next-character lexer)
            as d := (digit-char-p c)
            while d
            do (setf fractional-part (+ (* fractional-part 10) d)))
      (when (char= c #\.) ; here we have something like ``0..100''
        (contribute lexer
                    :type :decimal-integer
                    :origin (capture-origin lexer)
                    :content whole-part)
        (contribute lexer
                    :type :subrange
                    :origin (capture-origin lexer))
        (next-character lexer)
        (return-from get-floating-point)))
    (when (char-equal c #\e)
      (let ((c (next-character lexer)))
        (when (member c '(#\+ #\-))
          (when (char= c #\-)
            (setf exponent-sign :minus))
          (next-character lexer))
        (loop for d := (digit-char-p c) while d do
          (setf c (next-character lexer)
                exponent-part (+ (* exponent-part 10) d)))))
    (contribute lexer
                :type :floating-point-literal
                :origin (capture-origin lexer)
                :content (make-floating-point-literal :whole-part whole-part
                                                      :fractional-part fractional-part
                                                      :exponent-sign exponent-sign
                                                      :exponent-part exponent-part))))

(defun get-octal (lexer)
  (let ((accumulator 0))
    (loop for c := (next-character lexer)
          as d := (digit-char-p c 8)
          while d
          do (setf accumulator (+ (* accumulator 8) d)))
    (previous-character lexer)
    (contribute lexer
                :type :octal-integer
                :origin (capture-origin lexer)
                :content accumulator)))

(defun get-hexadecimal (lexer)
  (let ((accumulator 0))
    (loop for c := (next-character lexer)
          as d := (digit-char-p c 16)
          while d
          do (setf accumulator (+ (* accumulator 16) d)))
    (previous-character lexer)
    (contribute lexer
                :type :hexadecimal-integer
                :origin (capture-origin lexer)
                :content accumulator)))

(defun get-identifier (initial lexer)
  (let ((name (with-output-to-string (text)
                (loop for c := initial then (next-character lexer)
                      while (or (alphanumericp c)
                                (char= c #\_))
                      do (write-char c text)))))
    (previous-character lexer)
    (contribute lexer
                :type :identifier
                :origin (capture-origin lexer)
                :content (lookup-identifier name))))

(defun get-punctuator (initial lexer)
  (case initial
    ((#\#)
     (contribute lexer
                 :type :macro-parameter
                 :origin (capture-origin lexer)))
    ((#\^)
     (contribute lexer
                 :type :up-arrow
                 :origin (capture-origin lexer)))
    ((#\,)
     (contribute lexer
                 :type :comma
                 :origin (capture-origin lexer)))
    ((#\;)
     (contribute lexer
                 :type :semicolon
                 :origin (capture-origin lexer)))
    ((#\/)
     (contribute lexer
                 :type :solidus
                 :origin (capture-origin lexer)))
    ((#\+)
     (contribute lexer
                 :type :plus
                 :origin (capture-origin lexer)))
    ((#\-)
     (contribute lexer
                 :type :minus
                 :origin (capture-origin lexer)))
    ((#\$)
     (contribute lexer
                 :type :dollar-sign
                 :origin (capture-origin lexer)))
    ((#\*)
     (let ((c (next-character lexer)))
       (cond ((char= c #\))
              (input-error lexer "Spurious end of comment."))
             (t
              (previous-character lexer)
              (contribute lexer
                          :type :times
                          :origin (capture-origin lexer))))))
    ((#\[)
     (contribute lexer
                 :type :left-bracket
                 :origin (capture-origin lexer)))
    ((#\])
     (contribute lexer
                 :type :right-bracket
                 :origin (capture-origin lexer)))
    ((#\()
     (let ((c (next-character lexer)))
       (cond ((char= c #\*)
              (cond ((lexer-state-in-inline-code-p lexer)
                     (input-error lexer "You can't have a comment in inline code."))
                    (t
                     (contribute lexer
                                 :type :begin-comment
                                 :origin (capture-origin lexer))
                     (setf (lexer-state-in-comment-p lexer) t))))
             ((char= c #\.)
              (contribute lexer
                          :type :left-bracket
                          :origin (capture-origin lexer)))
             (t
              (previous-character lexer)
              (contribute lexer
                          :type :left-parenthesis
                          :origin (capture-origin lexer))))))
    ((#\))
     (contribute lexer
                 :type :right-parenthesis
                 :origin (capture-origin lexer)))
    ((#\.)
     (let ((c (next-character lexer)))
       (cond ((char= c #\.)
              (contribute lexer
                          :type :subrange
                          :origin (capture-origin lexer)))
             ((char= c #\))
              (contribute lexer
                          :type :right-bracket
                          :origin (capture-origin lexer)))
             (t
              (previous-character lexer)
              (contribute lexer
                          :type :dot
                          :origin (capture-origin lexer))))))
    ((#\<)
     (let ((c (next-character lexer)))
       (cond ((char= c #\>)
              (contribute lexer
                          :type :not-equal
                          :origin (capture-origin lexer)))
             ((char= c #\=)
              (contribute lexer
                          :type :less-than-or-equal-to
                          :origin (capture-origin lexer)))
             (t
              (previous-character lexer)
              (contribute lexer
                          :type :less-than
                          :origin (capture-origin lexer))))))
    ((#\>)
     (let ((c (next-character lexer)))
       (cond ((char= c #\=)
              (contribute lexer
                          :type :greater-than-or-equal-to
                          :origin (capture-origin lexer)))
             (t
              (previous-character lexer)
              (contribute lexer
                          :type :greater-than
                          :origin (capture-origin lexer))))))
    ((#\=)
     (let ((c (next-character lexer)))
       (cond ((char= c #\=)
              (contribute lexer
                          :type :equivalence-sign
                          :origin (capture-origin lexer)))
             (t
              (previous-character lexer)
              (contribute lexer
                          :type :equals
                          :origin (capture-origin lexer))))))
    ((#\:)
     (let ((c (next-character lexer)))
       (cond ((char= c #\=)
              (contribute lexer
                          :type :becomes
                          :origin (capture-origin lexer)))
             (t
              (previous-character lexer)
              (contribute lexer
                          :type :colon
                          :origin (capture-origin lexer))))))
    (otherwise ; we fell through to here from read-Pascal
     (input-error lexer "Invalid character."))))

;; Perhaps a better name for scan-section is scan-sections, since it does not
;; return after scanning only one section. Instead it does its thing until the
;; end of input, by means of a tail call.
(defun scan-section (lexer starredp)
  (let ((section (make-section :starredp starredp)))
    (add-section section)
    (setf (section-number section) (get-section-count))
    (when starredp
      (setf (section-starred-name section) (with-output-to-string (text)
                                             (loop for c := (next-character lexer)
                                                   while (char/= c #\.)
                                                   do (write-char c text)))))
    (flet ((finish-section ()
             (multiple-value-bind (TeX-part definition-part Pascal-part)
                                  (clear-lexer-state lexer)
               (setf (section-TeX-part section) TeX-part
                     (section-definition-part section) definition-part
                     (section-Pascal-part section) Pascal-part)
               (unless (null Pascal-part)
                 (cond ((eq (token-content (first Pascal-part)) t)
                        (assert (and (token-p (second Pascal-part))
                                     (eq (token-type (second Pascal-part))
                                         :module-name)))
                        ;; Note: We don't check the tokens following the module
                        ;; name for validity here (see section 176 of TANGLE).
                        (let ((module (token-content (second Pascal-part))))
                          (push (section-number section)
                                (module-definitions module))))
                       (t
                        (add-unnamed (section-number section))))))))
      (setf (lexer-state-in-TeX-part-p lexer) t)
      (handler-case (read-text lexer)
        (input-has-ended (condition)
          (declare (ignore condition))
          (when (lexer-state-in-inline-code-p lexer)
            (input-error lexer "Input ended in inline code.")
            (contribute lexer
                        :type :end-inline-code))
          (when (lexer-state-in-comment-p lexer)
            (input-error lexer "Input ended in comment.")
            (contribute lexer
                        :type :end-comment))
          (finish-section))
        (new-section (condition)
          (finish-section)
          (scan-section lexer (new-section-starredp condition)))))))

(defun scan-WEB (WEB-file change-file)
  (let ((lexer (initialize-lexer-state (initialize-input WEB-file change-file)
                                       #'next-character-file
                                       #'previous-character-file))
        (limbo (make-string-output-stream)))
    (let ((c (next-character lexer)))
      (loop do ; until start of section or end of file
        (cond ((char= c #\@)
               (setf c (next-character lexer))
               (cond ((char= c #\@)
                      (write-char #\@ limbo))
                     ((or (member c '(#\Space #\Tab #\Newline) :test #'char=)
                          (char= c #\*))
                      (return))))
              (t
               (write-char c limbo)))
        (setf c (next-character lexer)))
      (setf limbo (get-output-stream-string limbo))
      (scan-section lexer (char= c #\*)))))

(defun Phase-1 (WEB-file &optional (change-file (make-string-input-stream "")))
  (scan-WEB WEB-file change-file)
  (when (null (module-definitions (get-unnamed-module)))
    (cerror "Keep going" "No output was specified."))
  ;; Now tokenize the module names and nreverse the lists of definitions.
  (let ((unnamed (get-unnamed)))
    (setf (module-definitions unnamed) (nreverse (module-definitions unnamed))))
  (map-modules (lambda (module)
                 (setf (module-definitions module) (nreverse (module-definitions module)))
                 (let* ((name (module-name module))
                        (length (length name))
                        (next-character (lambda (lexer)
                                          (if (= (lexer-state-position lexer)
                                                 length)
                                              (signal 'input-has-ended)
                                              (prog1 (schar name
                                                            (lexer-state-position lexer))
                                                     (incf (lexer-state-position lexer))))))
                        (previous-character (lambda (lexer)
                                              (assert (> (lexer-state-position lexer)
                                                         0))
                                              (decf (lexer-state-position lexer))
                                              (schar name (lexer-state-position lexer))))
                        (lexer (initialize-lexer-state nil
                                                       next-character
                                                       previous-character)))
                   ;; The position slot is 0 by default.
                   (setf (lexer-state-buffer lexer) name
                         (lexer-state-in-module-name-p lexer) t
                         (lexer-state-module lexer) module)
                   (handler-case (read-text lexer)
                     (input-has-ended (condition)
                       (declare (ignore condition))
                       (when (lexer-state-in-inline-code-p lexer)
                         (input-error lexer "Input ended in inline code.")
                         (contribute lexer
                                     :type :end-inline-code))
                       (setf (module-name-tokens module) (clear-lexer-state lexer)))
                     (new-section (condition) ; i.e., we found @* or @space
                       (declare (ignore condition))
                       ;; All tokens after the unexpected control code will be
                       ;; dropped.
                       (setf (module-name-tokens module) (clear-lexer-state lexer))
                       (input-error lexer "New section in module name.")))))))