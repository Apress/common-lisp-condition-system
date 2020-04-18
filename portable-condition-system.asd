;;;; portable-condition-system.asd

(asdf:defsystem #:portable-condition-system
  :description "A portable condition system for Common Lisp"
  :author "Kent M. Pitman <kent@nhplace.com>"
  :maintainer "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "CC0"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "restarts")
               (:file "conditions")
               (:file "restarts-late")
               (:file "debugger")))

;;; TODO integration system
