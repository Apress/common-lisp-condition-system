;;;; integration/portable-condition-system.integration.asd

(asdf:defsystem #:portable-condition-system.integration
  :description "Integrates PCS with the host condition system"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "CC0"
  :version "0.0.1"
  :serial t
  :depends-on (#:portable-condition-system
               #:trivial-custom-debugger)
  :components ((:file "integration")))
