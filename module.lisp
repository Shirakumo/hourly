#|
 This file is a part of Hourly
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:hourly
  (:use #:cl #:radiance)
  (:shadow)
  (:export)
  (:local-nicknames))
(in-package #:hourly)

(define-trigger startup ())
