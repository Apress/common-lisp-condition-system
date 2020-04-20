;;;; integration/portable-condition-system.integration.asd

(asdf:defsystem #:portable-condition-system.integration
  :description "Integrates PCS with the host condition system"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "CC0"
  :version "0.0.1"
  :serial t
  :depends-on (#:portable-condition-system
               #:trivial-custom-debugger)
  :components ((:file "integration"))
  :in-order-to ((test-op (load-op #:portable-condition-system.integration/test)))
  :perform
  (test-op (o c)
           (symbol-call "PORTABLE-CONDITION-SYSTEM.INTEGRATION/TEST" "RUN")))

(asdf:defsystem #:portable-condition-system.integration/test
  :description "Tests for Portable Condition System Integration"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:portable-condition-system.integration)
  :components ((:file "test")))
