(in-package #:modularize-user)
(define-module #:hourly
  (:use #:cl #:radiance)
  (:shadow)
  (:export)
  (:local-nicknames))
(in-package #:hourly)

(define-trigger startup ())
