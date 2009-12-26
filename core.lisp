;;; NUTS-CORE
;;; (c) Vsevolod Dyomkin, Oleksandr Manzyuk. see LICENSE file for permissions

(in-package :nuts-core)

(locally-enable-literal-syntax :sharp-backq)

;; basic ops

(defvar *logg-out* #p"nuts.log"
        "Output file for logging")

(defmacro with-log-file ((var) &body body)
  `(with-open-stream (,var (if *logg-out*
                               (open *logg-out*
                                     :direction         :output
                                     :if-exists         :append
                                     :if-does-not-exist :create)
                               (make-broadcast-stream *standard-output*)))
     ,@body))

(defun logf (long-format-p control-string &rest args)
  "Print log message formatted according to <_:arg control-string /> ~
and <_:arg args>.  The destination of the message depends on the value ~
of <_:var *logg-out* />: if is NIL, the message is printed to the ~
standard output; otherwise, <_:var *logg-out* /> must be a pathname, ~
in which case the message is appended to the corresponding file (the ~
file is created if it does not exist).  If <_arg: long-format-p /> is ~
T, the message is prepended with the execution time. Returns NIL."
  (with-log-file (out)
    (format out "~:[~*~;~a | ~]~?"
            long-format-p (nth-value 1 (now)) control-string args)))

(defun logp (object)
  "Print <_:arg object /> to the log file designated by the pathname ~
<_:var *logg-out* /> or to the standard output if <_:var *logg-out* /> ~
is NIL. Returns <_:arg object />."
  (with-log-file (out)
    (print object out)))

(defmacro monitor (form)
  "Evaluate <_:arg form /> logging the result.  If evaluation raises
an error, log and reraise it."
  (with-gensyms (e values)    
    `(handler-case ,form
       (error (,e)
         (logf nil "ERROR ~a~%" ,e)
         (error ,e))
       (:no-error (&rest ,values)
         (logf t "~a => ~{~a~^ ~}~%" ',form ,values)
         (values-list ,values)))))

(defmacro check (pred &rest args)
  "Check, if the <_:arg pred /> is satisfied. Pred should be a literal name, ~
not a function object.
Log the result."
  (with-gensyms (rez)
    `(let ((,rez (funcall ',pred ,@args)))
       (logf nil "~a ~{~a ~}-> ~a~%" ',pred ',args ,rez)
       (values ,rez ',args))))

;; tests

(defvar *test-thunks* (make-hash-table)
  "Compiled test closures")

(defmacro deftest (name (&rest args) &body body)
  "A test is just a named closure, stored in <_:var *test-thunks* /> hash-table.
It can reference other tests by name the same, as ordinary functions.
Meta-tests (test suites) can be created just by calling some tests inside ~
the other one. Thus it is possible to shadow real functions and macros with ~
tests (only inside a test), so use with caution."
  `(progn
     (when (gethash ',name *test-thunks*)
       (warn "Redefining test ~a" ',name))
     (setf (gethash ',name *test-thunks*)
           (lambda ,args
             (cumulative-and
              ,@(mapcar #`(if-it (when (listp _)
                                   ;; test referenced like a function
                                   (gethash (car _) *test-thunks*))
                                 (progn
                                   (warn "Used test ~a in function call ~
position"
                                         (car _))
                                   `(funcall ,it ,@(cdr _)))
                                 _)
                        body))))
     (values ',name
             (to-string *test-thunks*))))

(defvar *catch-errors?* t
  "Intercept error signals from tests?")

(defun %run-test (name &rest args)
  (if-it (gethash name *test-thunks*)
         (handler-case (apply it args)
           (error (e)
             (when *catch-errors?* (error e))
             (logf nil "Error: ~A~%" (type-of e))
             (values nil e)))
         (progn
           (logf nil "Unknown test: ~A~%" name)
           (values nil nil))))

(defun %run-tests (&rest tests)
  (let ((tests (or tests (hash-table-keys *test-thunks*))))
    (logf t "Running ~A tests~:P...~%" (length tests))
    (let ((count 0) list-of-results list-of-errors)
      (dolist (test tests)
        (destructuring-bind (name . args) (ensure-list test)
          (logf t "Test #~A: ~A~#[~;~A~]" (incf count) name args)
          (multiple-value-bind (result errors) (apply #'%run-test name args)
            (logf nil "Result of test #~A: ~A" count result)
            (push result list-of-results)
            (push errors list-of-errors))))
      (values (nreverse list-of-results) (nreverse list-of-errors)))))

(defmacro run-tests (&rest tests)
  "Run TESTS, where each test is supplied either as a symbol NAME or as a
list (NAME . ARGS).  If TESTS is NIL, run all tests from *TEST-THUNKS*.
Returns 2 lists: a list of test results (each result being T or NIL),
and a list of test errors (each error being either a condition that
was signalled during the test execution or a list of failed cases)."
  `(%run-tests ,@(loop for test in tests collect `',test)))

(locally-disable-literal-syntax :sharp-backq)

;;; end