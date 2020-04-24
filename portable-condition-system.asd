;;;; portable-condition-system.asd

(asdf:defsystem #:portable-condition-system
  :description "A portable condition system for Common Lisp"
  :author "Kent M. Pitman <kent@nhplace.com>"
  :maintainer "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "CC0"
  :version "1.0.0"
  :serial t
  :pathname "src"
  :depends-on (#:alexandria
               #:split-sequence)
  :components ((:file "package")
               (:file "conditions")
               (:file "condition-hierarchy")
               (:file "coerce-to-condition")
               (:file "restarts")
               (:file "assertions")
               (:file "signaling")
               (:file "handlers")
               (:file "debugger"))
  :in-order-to ((test-op (load-op #:portable-condition-system/test)))
  :perform
  (test-op (o c)
           (symbol-call "PORTABLE-CONDITION-SYSTEM/TEST" "RUN")))

(asdf:defsystem #:portable-condition-system/test
  :description "Tests for Portable Condition System"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "MIT"
  :version "1.0.0"
  :serial t
  :pathname "t"
  :depends-on (#:portable-condition-system)
  :components ((:file "package")
               (:file "1am")
               (:file "debugger")
               (:file "ansi-test-data")
               (:file "ansi-test-support")
               (:module "ansi-test"
                :components
                ((:file "condition")
                 (:file "cell-error-name")
                 (:file "assert")
                 (:file "error")
                 (:file "cerror")
                 (:file "check-type")
                 (:file "warn")
                 (:file "invoke-debugger")
                 (:file "handler-bind")
                 (:file "handler-case")
                 (:file "ignore-errors")
                 (:file "define-condition")
                 (:file "compute-restarts")
                 (:file "restart-bind")
                 (:file "restart-case")
                 (:file "with-condition-restarts")
                 (:file "with-simple-restart")
                 (:file "abort")
                 (:file "muffle-warning")
                 (:file "continue")
                 (:file "store-value")
                 (:file "use-value")
                 (:file "make-condition")
                 (:file "ecase")
                 (:file "etypecase")
                 (:file "ccase")
                 (:file "ctypecase")))))
