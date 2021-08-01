#|
 This file is a part of Hourly
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:hourly)

(defun render-page (page content &rest args)
  (r-clip:with-clip-processing ("template.ctml")
    (apply #'r-clip:process T
           :page page
           :content (plump:parse content)
           :version (asdf:component-version (asdf:find-system :hourly))
           args)))

(define-page dashboard "hourly/^$" (:access (perm hourly user))
  (render-page "Dashboard"
               (@template "dashboard.ctml")
               :projects (list-projects)
               :hour (current-hour)))

(define-page project "hourly/([^/]+)/?" (:uri-groups (project) :access (perm hourly user))
  (let ((project (check-accessible (ensure-project project))))
    (render-page (dm:field project "title")
                 (@template "project.ctml")
                 :up (url> "hourly/") :up-text "Dashboard"
                 :project project
                 :tasks (list-tasks :amount 100 :skip (* 100 (1- (int* (post/get "page") 1))))
                 :hour (current-hour))))

(define-page task "hourly/([^/]+)/([^/]+)/?" (:uri-groups (project task) :access (perm hourly user))
  (let* ((project (ensure-project project))
         (task (check-accessible (ensure-task task project))))
    (render-page (dm:field task "title")
                 (@template "task.ctml")
                 :up (url> "hourly/~a" (dm:field project "title")) :up-text (dm:field project "title")
                 :project project
                 :task task
                 :hours (list-hours task)
                 :hour (current-hour))))
