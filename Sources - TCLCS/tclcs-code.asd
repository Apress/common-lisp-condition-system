;;;; tclcs-code.asd

(asdf:defsystem #:tclcs-code
  :description "Companion code for \"The Common Lisp Condition System\""
  :author "Michał \"phoe\" Herda <phoe@disroot.org"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:trivial-custom-debugger)
  :components
  ((:module "00-dynavars"
    :components ((:file "tclcs-00-dynamic-variables")))
   (:module "01-hooks"
    :components ((:file "tclcs-01-lets-meet-tom")
                 (:file "tclcs-02-hook-1-launching-csgo")
                 (:file "tclcs-03-hook-2-only-call-csgo-players")
                 (:file "tclcs-04-hook-3-only-call-parents-maybe")
                 (:file "tclcs-05-hook-4-holiday-wishes")
                 (:file "tclcs-06-hook-5-calling-girlfriend-again")
                 (:file "tclcs-07-multiple-types-of-hooks")))
   (:module "02-handlers"
    :components ((:file "tclcs-08-lets-meet-tom-again")
                 (:file "tclcs-09-handler-1-launching-csgo")
                 (:file "tclcs-10-handler-2-only-call-csgo-players")
                 (:file "tclcs-11-handler-3-only-call-parents-maybe")
                 (:file "tclcs-12-handler-4-holiday-wishes")
                 (:file "tclcs-13-handler-5-calling-girlfriend-again")
                 (:file "tclcs-14-exception-handling-1")
                 (:file "tclcs-15-exception-handling-2")
                 (:file "tclcs-16-protection-against-transfers-of-control")
                 (:file "tclcs-17-handler-clustering")))
   (:module "03-choices"
    :components ((:file "tclcs-18-lets-meet-kate-and-mark")
                 (:file "tclcs-19-choice-1-escape-through-front-door")
                 (:file "tclcs-20-choice-1-escape-through-both-doors")
                 (:file "tclcs-21-choice-1-same-named-choices")
                 (:file "tclcs-22-choice-2-excuse")))
   (:module "04-restarts"
    :components ((:file "tclcs-23-lets-meet-kate-and-mark-again")
                 (:file "tclcs-24-restart-1-escape-through-front-door")
                 (:file "tclcs-25-restart-1-escape-through-both-doors")
                 (:file "tclcs-26-restart-1-same-named-restarts")
                 (:file "tclcs-27-restart-2-excuse")
                 (:file "tclcs-28-restart-2-interactive-excuse")
                 (:file "tclcs-29-actually-restarting-restarts")
                 (:file "tclcs-30-custom-restart-invoking-functions")))
   (:module "05-reporting"
    :components ((:file "tclcs-31-reporting-conditions")
                 (:file "tclcs-32-reporting-restarts")))
   (:module "06-warnings"
    :components ((:file "tclcs-33-warnings")))
   (:module "07-assertions"
    :components ((:file "tclcs-34-assert")
                 (:file "tclcs-35-check-type")
                 (:file "tclcs-36-case-assertions")))
   (:module "08-debugger"
    :components ((:file "tclcs-37-debugger-reporting-condition")
                 (:file "tclcs-38-debugger-reporting-condition-type")
                 (:file "tclcs-39-debugger-reporting-restarts")
                 (:file "tclcs-40-debugger-choosing-restarts")
                 (:file "tclcs-41-installing-custom-debugger")
                 (:file "tclcs-42-recursive-debugger")
                 (:file "tclcs-43-reducing-debugger-level")
                 (:file "tclcs-44-repl-in-debugger")))
   (:module "96-call-with-handler-restart"
    :components ((:file "tclcs-94-call-with-handler-restart")
                 (:file "tclcs-95-call-with-handler-restart-cached")))
   (:module "97-handler-bind-star-case-star"
    :components ((:file "tclcs-96-handler-bind-star")
                 (:file "tclcs-97-handler-case-star")))
   (:module "98-handler-bind-case"
    :components ((:file "tclcs-98-handler-bind-case")))
   (:module "99-macros-101"
    :components ((:file "tclcs-99-macros-101")))))
