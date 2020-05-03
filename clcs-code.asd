;;;; clcs-code.asd

(asdf:defsystem #:clcs-code
  :description "Companion code for the Common Lisp Condition System book"
  :author "Michał \"phoe\" Herda <phoe@disroot.org"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components
  ((:module "00-dynavars"
    :components ((:file "clcs-00-dynamic-variables")))
   (:module "01-hooks"
    :components ((:file "clcs-01-lets-meet-tom")
                 (:file "clcs-02-hook-1-launching-csgo")
                 (:file "clcs-03-hook-2-only-call-csgo-players")
                 (:file "clcs-04-hook-3-only-call-parents-maybe")
                 (:file "clcs-05-hook-4-holiday-wishes")
                 (:file "clcs-06-hook-5-calling-girlfriend-again")
                 (:file "clcs-07-multiple-types-of-hooks")))
   (:module "02-handlers"
    :components ((:file "clcs-08-lets-meet-tom-again")
                 (:file "clcs-09-handler-1-launching-csgo")
                 (:file "clcs-10-handler-2-only-call-csgo-players")
                 (:file "clcs-11-handler-3-only-call-parents-maybe")
                 (:file "clcs-12-handler-4-holiday-wishes")
                 (:file "clcs-13-handler-5-calling-girlfriend-again")
                 (:file "clcs-14-exception-handling-1")
                 (:file "clcs-15-exception-handling-2")
                 (:file "clcs-16-protection-against-transfers-of-control")
                 (:file "clcs-17-handler-clustering.lisp")))))
