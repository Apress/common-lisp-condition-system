;;;; debugger.lisp

(in-package #:portable-condition-system)

;;; TODO: I haven't yet touched this file.

(defun break (&optional (format-control "Break") &rest format-arguments)
  (with-simple-restart (continue "Return from BREAK.")
    (invoke-debugger
     (make-condition 'simple-condition
                     :format-control format-control
                     :format-arguments format-arguments)))
  nil)

(defvar *debug-level* 0)
(defvar *debug-abort* nil)
(defvar *debug-continue* nil)
(defvar *debug-condition* nil)
(defvar *debug-restarts* nil)
(defvar *number-of-debug-restarts* 0)
(defvar *debug-eval* 'eval)
(defvar *debug-print* (lambda (values) (format t "~&~{~S~^,~%~}" values)))

(defmacro debug-command                (x) `(get ,x 'debug-command))
(defmacro debug-command-argument-count (x) `(get ,x 'debug-command-argument-count))

(defmacro define-debug-command (name bvl &rest body)
  `(progn (setf (debug-command ',name) (lambda ,bvl ,@body))
          (setf (debug-command-argument-count ',name) ,(length bvl))
          ',name))

(defun read-debug-command ()
  (format t "~&Debug ~D> "*debug-level*)
  (cond ((char= (peek-char t) #\:)
         (with-input-from-string (stream (read-line))
           (let ((eof (list nil)))
             (do ((form (let ((*package* (find-package "KEYWORD")))
                          (read-char) ;Eat the ":" so that ":1" reliably reads a number.
                          (read stream nil eof))
                        (read stream nil eof))
                  (l '() (cons form l)))
                 ((eq form eof) (nreverse l))))))
        (t
         (list :eval (read)))))

(define-debug-command :eval (form)
  (funcall *debug-print* (multiple-value-list (funcall *debug-eval* form))))

(define-debug-command :abort ()
  (if *debug-abort*
      (invoke-restart-interactively *debug-abort*)
      (format t "~&There is no way to abort.~%")))

(define-debug-command :continue ()
  (if *debug-continue*
      (invoke-restart-interactively *debug-continue*)
      (format t "~&There is no way to continue.~%")))

(define-debug-command :error ()
  (format t "~&~A~%"*debug-condition*))

(define-debug-command :help ()
  (format t "~&You are in a portable debugger.~
             ~%Type a debugger command or a form to evaluate.~
             ~%Commands are:~%")
  (show-restarts *debug-restarts* *number-of-debug-restarts* 16)
  (format t "~& :EVAL form     Evaluate a form.~
             ~% :HELP          Show this text.~%")
  (if *debug-abort*    (format t "~& :ABORT         Exit by ABORT.~%"))
  (if *debug-continue* (format t "~& :CONTINUE      Exit by CONTINUE.~%"))
  (format t "~& :ERROR         Reprint error message.~%"))



(defun show-restarts (&optional (restarts *debug-restarts*)
                        (max *number-of-debug-restarts*)
                        target-column)
  (unless max (setq max (length restarts)))
  (when restarts
    (do ((w (if target-column
                (- target-column 3)
                (ceiling (log max 10))))
         (p restarts (cdr p))
         (i 0 (1+ i)))
        ((or (not p) (= i max)))
      (format t "~& :~A "              (let ((s (format nil "~D"(+ i 1))))
                (with-output-to-string (str)
                  (format str "~A"s)
                  (dotimes (i (- w (length s)))
                    (write-char #\Space str)))))
      (if (eq (car p) *debug-abort*) (format t "(Abort) "))
      (if (eq (car p) *debug-continue*) (format t "(Continue) "))
      (format t "~A"(car p))
      (format t "~%"))))

(defvar *debugger-hook* nil)

(defun invoke-debugger (&optional (datum "Debug") &rest arguments)
  (let ((condition (coerce-to-condition datum arguments 'simple-condition 'debug)))
    (when *debugger-hook*
      (let ((hook *debugger-hook*)
            (*debugger-hook* nil))
        (funcall hook condition hook)))
    (standard-debugger condition)))

(defun standard-debugger (condition)
  (let* ((*debug-level* (1+ *debug-level*))
         (*debug-restarts* (compute-restarts))
         (*number-of-debug-restarts* (length *debug-restarts*))
         (*debug-abort*    (find-restart 'abort))
         (*debug-continue* (or (let ((c (find-restart 'continue)))
                                 (if (or (not *debug-continue*)
                                         (not (eq *debug-continue* c)))
                                     c nil))
                               (let ((c (if *debug-restarts*
                                            (first *debug-restarts*) nil)))
                                 (if (not (eq c *debug-abort*)) c nil))))
         (*debug-condition* condition))
    (format t "~&~A~%"condition)
    (show-restarts)
    (do ((command (read-debug-command)
                  (read-debug-command)))
        (nil)
      (execute-debugger-command (car command) (cdr command) *debug-level*))))

(defun execute-debugger-command (cmd args level)
  (with-simple-restart (abort "Return to debug level ~D."level)
    (cond ((not cmd))
          ((integerp cmd)
           (cond ((and (plusp cmd)
                       (< cmd (+ *number-of-debug-restarts* 1)))
                  (let ((restart (nth (- cmd 1) *debug-restarts*)))
                    (if args
                        (apply #'invoke-restart restart (mapcar *debug-eval* args))
                        (invoke-restart-interactively restart))))
                 (t
                  (format t "~&No such restart."))))
          (t
           (let ((fn (debug-command cmd)))
             (if fn
                 (cond ((not (= (length args) (debug-command-argument-count cmd)))
                        (format t "~&Too ~:[few~;many~] arguments to ~A."
                                (> (length args) (debug-command-argument-count cmd))
                                cmd))
                       (t
                        (apply fn args)))
                 (format t "~&~S is not a debugger command.~%"cmd)))))))
