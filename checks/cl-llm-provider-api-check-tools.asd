;;;; -- Source Check Tools --

;;; qlot installs project deps by reading ASDF system definitions.
;;; This system declares cheque, the source checker loaded by the ./script/check script.
;;; This is so that installing checks/qlfile also installs cheque's own dependencies.

;; CL-UNICODE's conditional build dependency is absent from qlot's static plan.
(asdf:defsystem #:cl-llm-provider-api-check-tools
  :description "The devel tools loaded for cl-llm-provider-api's source checks."
  :depends-on  (#:cheque #:flexi-streams))
