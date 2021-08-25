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
               :projects (list-projects :user (auth:current))
               :hour (current-hour)
               :personal (or (find-project "personal" NIL)
                             (make-project "personal"))
               :pins (list-pinned-tasks)))

(define-page project "hourly/project/([^/]+)(?:/(.+))?$" (:uri-groups (id title) :access (perm hourly user))
  (declare (ignore title))
  (let ((project (check-accessible (ensure-project (db:ensure-id id)))))
    (render-page (dm:field project "title")
                 (@template "project.ctml")
                 :up (url> "hourly/") :up-text "Dashboard"
                 :project project
                 :tasks (list-tasks project :amount 100 :skip (* 100 (1- (int* (post/get "page") 1))))
                 :hour (current-hour :project project))))

(define-page task "hourly/task/([^/]+)(?:/(.+))?$" (:uri-groups (id title) :access (perm hourly user))
  (declare (ignore title))
  (let* ((task (check-accessible (ensure-task (db:ensure-id id))))
         (project (ensure-project (dm:field task "project"))))
    (render-page (dm:field task "title")
                 (@template "task.ctml")
                 :up (url> (format NIL "hourly/~a/~a" (dm:id project) (dm:field project "title")))
                 :up-text (dm:field project "title")
                 :project project
                 :task task
                 :hours (list-hours :task task)
                 :hour (current-hour :task task))))

(define-page export-csv "hourly/export(?:/(.+))?$" (:uri-groups (project) :access (perm hourly user))
  (let* ((project (when project (check-accessible (ensure-project (db:ensure-id project))))))
    (render-page "Export"
                 (@template "export.ctml")
                 :up (when project (url> (format NIL "hourly/~a/~a" (dm:id project) (dm:field project "title"))))
                 :up-text (when project (dm:field project "title"))
                 :project project)))

(define-page pin "hourly/pin/(.+)" (:uri-groups (id) :access (perm hourly user))
  (let* ((task (check-accessible (ensure-task (db:ensure-id id))))
         (project (ensure-project (dm:field task "project"))))
    (render-page (dm:field task "title")
                 (@template "pin.ctml")
                 :up (url> (format NIL "hourly/~a/~a" (dm:id project) (dm:field project "title")))
                 :up-text (dm:field project "title")
                 :project project
                 :task task
                 :pin (find-pin task))))
