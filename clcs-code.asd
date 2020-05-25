;;;; clcs-code.asd

(asdf:defsystem #:clcs-code
  :description "Companion code for \"The Common Lisp Condition System\""
  :author "Michał \"phoe\" Herda <phoe@disroot.org"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:trivial-custom-debugger)
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
                 (:file "clcs-17-handler-clustering")))
   (:module "03-choices"
    :components ((:file "clcs-18-lets-meet-kate-and-mark")
                 (:file "clcs-19-choice-1-escape-through-front-door")
                 (:file "clcs-20-choice-1-escape-through-both-doors")
                 (:file "clcs-21-choice-1-same-named-choices")
                 (:file "clcs-22-choice-2-excuse")))
   (:module "04-restarts"
    :components ((:file "clcs-23-lets-meet-kate-and-mark-again")
                 (:file "clcs-24-restart-1-escape-through-front-door")
                 (:file "clcs-25-restart-1-escape-through-both-doors")
                 (:file "clcs-26-restart-1-same-named-restarts")
                 (:file "clcs-27-restart-2-excuse")
                 (:file "clcs-28-restart-2-interactive-excuse")
                 (:file "clcs-29-actually-restarting-restarts")
                 (:file "clcs-30-custom-restart-invoking-functions")))
   (:module "05-reporting"
    :components ((:file "clcs-31-reporting-conditions")
                 (:file "clcs-32-reporting-restarts")))
   (:module "06-warnings"
    :components ((:file "clcs-33-warnings")))
   (:module "07-assertions"
    :components ((:file "clcs-34-assert")
                 (:file "clcs-35-check-type")
                 (:file "clcs-36-case-assertions")))
   (:module "08-debugger"
    :components ((:file "clcs-37-debugger-reporting-condition")
                 (:file "clcs-38-debugger-reporting-condition-type")
                 (:file "clcs-39-debugger-reporting-restarts")
                 (:file "clcs-40-debugger-choosing-restarts")
                 (:file "clcs-41-installing-custom-debugger")
                 (:file "clcs-42-recursive-debugger")
                 (:file "clcs-43-reducing-debugger-level")
                 (:file "clcs-44-repl-in-debugger")))
   (:module "97-call-with-handler-restart"
    :components ((:file "clcs-97-call-with-handler-restart")))
   (:module "98-handler-case-star"
    :components ((:file "clcs-98-handler-case-star")))
   (:module "99-macros-101"
    :components ((:file "clcs-99-macros-101")))))
