#|
 This file is a part of Hourly
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:hourly)

(defun output (object message url-format &rest args)
  (let ((target (url> (apply #'format NIL url-format args)
                      :query `(("message" . ,message)))))
    (if (string= "true" (post/get "browser"))
        (redirect target)
        (api-output object :message message :target target))))

(define-api hourly/stats (&optional (scale "week")) (:access (perm hourly user))
  (let* ((user (auth:current))
         (labels ())
         (data ()))
    (flet ((compute (start step y-labels)
             (loop for i from 0 below (length y-labels)
                   for d = (mod (- start i) (length y-labels))
                   for tmax = (get-universal-time) then tmin
                   for tmin = (- tmax step)
                   for hours = (db:select 'hour
                                          (db:query (:and (:= 'author (dm:id user))
                                                          (:<= 'start tmax)
                                                          (:< tmin 'end)))
                                          :fields '(start end))
                   do (push (aref y-labels d) labels)
                      (push (loop for hour in hours
                                  when (gethash hour "end")
                                  sum (- (gethash hour "end") (gethash hour "start")))
                            data))))
      (cond ((equal scale "week")
             (compute (nth-value 6 (decode-universal-time (get-universal-time) 0))
                      (* 60 60 24)
                      #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))
            ((equal scale "month")
             (compute (1- (nth-value 3 (decode-universal-time (get-universal-time) 0)))
                      (* 60 60 24)
                      (coerce (nreverse (loop for i downfrom (nth-value 3 (decode-universal-time (get-universal-time) 0)) to 1
                                              collect i))
                              'vector)))
            ((equal scale "year")
             (compute (1- (nth-value 4 (decode-universal-time (get-universal-time) 0)))
                      (* 60 60 24 30)
                      #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
            (T
             (error 'api-argument-invalid :argument "scale"))))
    (api-output (mktable "labels" labels "points" data))))

(define-api hourly/project (project) (:access (perm hourly user))
  (api-output (check-accessible (ensure-project project))))

(define-api hourly/project/list (&optional amount skip) (:access (perm hourly user))
  (api-output (list-projects (auth:current) :amount (int* amount) :skip (int* skip 0))))

(define-api hourly/project/new (title &optional description access-user[] access-level[]) (:access (perm hourly project new))
  (let ((project (make-project title :description description
                                     :users (loop for user in access-user[]
                                                  for level in access-level[]
                                                  collect (list user (int* level 1))))))
    (output project "Project created." "hourly/~a" (dm:field project "title"))))

(define-api hourly/project/edit (project &optional title description access-user[] access-level[]) (:access (perm hourly user))
  (let ((project (check-accessible (ensure-project project) :level 3)))
    (edit-project project :title title
                          :description description
                          :users (loop for user in access-user[]
                                       for level in access-level[]
                                       collect (list user (int* level 1))))
    (output project "Project updated." "hourly/~a" (dm:field project "title"))))

(define-api hourly/project/delete (project) (:access (perm hourly user))
  (let ((project (check-accessible (ensure-project project) :level 4)))
    (delete-project project)
    (output project "Project deleted." "hourly/~a" (dm:field project "title"))))

(define-api hourly/project/stats (project &optional (scale "week")) (:access (perm hourly user))
  (let* ((project (check-accessible (ensure-project project)))
         (labels ())
         (data ()))
    (flet ((compute (start step y-labels)
             (loop for i from 0 below (length y-labels)
                   for d = (mod (- start i) (length y-labels))
                   for tmax = (get-universal-time) then tmin
                   for tmin = (- tmax step)
                   for hours = (db:select (rdb:join (hour task) (((task project) (project _id)) _id))
                                          (db:query (:and (:= 'project (dm:id project))
                                                          (:<= 'start tmax)
                                                          (:< tmin 'end)))
                                          :fields '(start end))
                   do (push (aref y-labels d) labels)
                      (push (loop for hour in hours
                                  when (gethash hour "end")
                                  sum (- (gethash hour "end") (gethash hour "start")))
                            data))))
      (cond ((equal scale "week")
             (compute (nth-value 6 (decode-universal-time (get-universal-time) 0))
                      (* 60 60 24)
                      #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))
            ((equal scale "month")
             (compute (1- (nth-value 3 (decode-universal-time (get-universal-time) 0)))
                      (* 60 60 24)
                      (coerce (nreverse (loop for i downfrom (nth-value 3 (decode-universal-time (get-universal-time) 0)) to 1
                                              collect i))
                              'vector)))
            ((equal scale "year")
             (compute (1- (nth-value 4 (decode-universal-time (get-universal-time) 0)))
                      (* 60 60 24 30)
                      #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
            (T
             (error 'api-argument-invalid :argument "scale"))))
    (api-output (mktable "labels" labels "points" data))))

(define-api hourly/task (project task) (:access (perm hourly user))
  (api-output (check-accessible (find-task task project))))

(define-api hourly/task/list (project &optional amount skip) (:access (perm hourly user))
  (api-output (list-tasks (check-accessible (ensure-project project))) :amount (int* amount) :skip (int* skip 0)))

(define-api hourly/task/new (project title &optional description) (:access (perm hourly user))
  (let* ((project (check-accessible (ensure-project project) :level 2))
         (task (make-task project title :description description)))
    (output task "Task created." "hourly/~a/~a" (dm:field project "title") (dm:id task))))

(define-api hourly/task/edit (task &optional title description) (:access (perm hourly user))
  (let ((task (check-accessible (ensure-task task) :level 2)))
    (edit-task task :title title :description description)
    (output task "Task updated." "hourly/~a/~a" (dm:field task "project") (dm:id task))))

(define-api hourly/task/delete (task) (:access (perm hourly user))
  (let ((task (check-accessible (ensure-task task) :level 2)))
    (delete-task task)
    (output task "Task deleted." "hourly/~a" (dm:field task "project"))))

(define-api hourly/task/stats (task) (:access (perm hourly user))
  (let ((task (check-accessible (ensure-task task))))
    (api-output)))

(define-api hourly/task/hours (task) (:access (perm hourly user))
  (api-output (list-hours (check-accessible (ensure-task task)))))

(define-api hourly/task/start (&optional task project comment) (:access (perm hourly user))
  (cond (task
         (let* ((task (check-accessible (ensure-task task)))
                (hour (make-hour task :comment comment)))
           (output hour "Hour created." "hourly/~a/~a#~a" (dm:field task "project") (dm:id task) (dm:id hour))))
        (project
         (let* ((project (check-accessible (ensure-project project)))
                (task (make-task project "untitled"))
                (hour (make-hour task :comment comment)))
           (output hour "Hour created." "hourly/~a/~a#~a" (dm:field task "project") (dm:id task) (dm:id hour))))
        (T
         (error "Fuck"))))

(define-api hourly/task/stop (hour &optional comment) (:access (perm hourly user))
  (let* ((hour (check-accessible (ensure-hour hour)))
         (task (ensure-task (dm:field hour "task"))))
    (edit-hour hour :end (get-universal-time) :comment comment)
    (output hour "Hour logged." "hourly/~a/~a#~a" (dm:field task "project") (dm:id task) (dm:id hour))))

(define-api hourly/task/note (hour comment) (:access (perm hourly user))
  (let* ((hour (check-accessible (ensure-hour hour)))
         (task (ensure-task (dm:field hour "task"))))
    (edit-hour hour :comment comment)
    (output hour "Hour logged." "hourly/~a/~a#~a" (dm:field task "project") (dm:id task) (dm:id hour))))

(define-api hourly/task/undo (hour) (:access (perm hourly user))
  (let* ((hour (check-accessible (ensure-hour hour) :level 2))
         (task (ensure-task (dm:field hour "task"))))
    (delete-hour hour)
    (output hour "Hour deleted." "hourly/~a/~a" (dm:field task "project") (dm:id task))))
