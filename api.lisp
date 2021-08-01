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
                                          (db:query (:and (:= 'logger (user:id user))
                                                          (:<= 'start tmax)
                                                          (:< tmin 'end))))
                   do (push (aref y-labels d) labels)
                      (push (loop for hour in hours
                                  when (gethash "end" hour)
                                  sum (- (gethash "end" hour) (gethash "start" hour)))
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

(define-api hourly/export (&optional project (header "include") (col-separator "comma") (row-separator "crlf") (quotation-use "as-needed") (quotation-mark "double-quote") (quotation-escape "quote") (time-format "iso-8601")) (:access (perm hourly user))
  (flet ((timestamp-formatter (format)
           (lambda (x)
             (local-time:format-timestring NIL (local-time:universal-to-timestamp x) :format format))))
    (let* ((project (when project (check-accessible (ensure-project project))))
           (header (cond ((string= header "include") '("Project" "Task" "Author" "Comment" "Start" "End" "Duration"))
                         ((string= header "exclude") NIL)
                         (T (error 'api-argument-invalid :argument 'header))))
           (col-separator (cond ((string= col-separator "comma") #\,)
                                ((string= col-separator "tab") #\Tab)
                                (T (error 'api-argument-invalid :argument 'col-separator))))
           (row-separator (cond ((string= row-separator "crlf") (coerce '(#\Return #\Linefeed) 'string))
                                ((string= row-separator "lf") (coerce '(#\Linefeed) 'string))
                                (T (error 'api-argument-invalid :argument 'row-separator))))
           (quotation-use (cond ((string= quotation-use "as-needed") NIL)
                                ((string= quotation-use "always") T)
                                (T (error 'api-argument-invalid :argument 'quotation-use))))
           (quotation-mark (cond ((string= quotation-mark "double-quote") #\")
                                 ((string= quotation-mark "single-quote") #\')
                                 (T (error 'api-argument-invalid :argument 'quotation-mark))))
           (quotation-escape (cond ((string= quotation-escape "quote") (list quotation-mark quotation-mark))
                                   ((string= quotation-escape "backslash") (list #\\ quotation-mark))
                                   (T (error 'api-argument-invalid :argument 'qoutation-escape))))
           (time-format (cond ((string= time-format "iso-8601") (timestamp-formatter local-time:+iso-8601-format+))
                              ((string= time-format "rfc-1123") (timestamp-formatter local-time:+rfc-1123-format+))
                              ((string= time-format "timestamp") #'identity)
                              (T (error 'api-argument-invalid :argument 'time-format))))
           (rows ()))
      (flet ((process (hour)
               (push (list "PROJECT"
                           "TASK"
                           (user:username (dm:field hour "logger"))
                           (dm:field hour "comment")
                           (funcall time-format (dm:field hour "start"))
                           (funcall time-format (dm:field hour "end"))
                           (- (dm:field hour "end") (dm:field hour "start")))
                     rows)))
        (if project
            (dolist (task (list-tasks project))
              (mapc #'process (list-hours :task task)))
            (mapc #'process (list-hours :logger (auth:current)))))
      (setf (header "Content-Disposition") (format NIL "inline; filename=~s" (format NIL "hourly~@[~a~].csv" (when project (dm:field project "title")))))
      (setf (header "Content-Type") "text/csv;charset=utf-8")
      (with-output-to-string (out)
        (cl-csv:write-csv
         (if header
             (list* header rows)
             rows)
         :stream out
         :separator col-separator
         :quote quotation-mark
         :escape quotation-escape
         :newline row-separator
         :always-quote quotation-use)))))

(define-api hourly/project (project) (:access (perm hourly user))
  (api-output (check-accessible (ensure-project project))))

(define-api hourly/project/list (&optional amount skip) (:access (perm hourly user))
  (api-output (list-projects :user (auth:current) :amount (int* amount) :skip (int* skip 0))))

(define-api hourly/project/new (title &optional description access-user[] access-level[]) (:access (perm hourly user))
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
                                                          (:< tmin 'end))))
                   do (push (aref y-labels d) labels)
                      (push (loop for hour in hours
                                  when (gethash "end" hour)
                                  sum (- (gethash "end" hour) (gethash "start" hour)))
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
  (api-output (list-hours :task (check-accessible (ensure-task task)))))

(define-api hourly/task/start (&optional task project comment) (:access (perm hourly user))
  (cond (task
         (let* ((task (check-accessible (ensure-task task)))
                (hour (make-hour task :comment comment)))
           (output hour "Hour created." "hourly/~a/~a" (dm:field task "project") (dm:id task))))
        (project
         (let* ((project (check-accessible (ensure-project project)))
                (task (make-task project "untitled"))
                (hour (make-hour task :comment comment)))
           (output hour "Hour created." "hourly/~a/~a" (dm:field task "project") (dm:id task))))
        (T
         (error "Fuck"))))

(define-api hourly/task/stop (hour &optional comment) (:access (perm hourly user))
  (let* ((hour (check-accessible (ensure-hour hour)))
         (task (ensure-task (dm:field hour "task"))))
    (edit-hour hour :end (get-universal-time) :comment comment)
    (output hour "Hour logged." "hourly/~a/~a" (dm:field task "project") (dm:id task))))

(define-api hourly/task/note (hour comment) (:access (perm hourly user))
  (let* ((hour (check-accessible (ensure-hour hour)))
         (task (ensure-task (dm:field hour "task"))))
    (edit-hour hour :comment comment)
    (output hour "Hour logged." "hourly/~a/~a" (dm:field task "project") (dm:id task))))

(define-api hourly/task/undo (hour) (:access (perm hourly user))
  (let* ((hour (check-accessible (ensure-hour hour) :level 2))
         (task (ensure-task (dm:field hour "task"))))
    (delete-hour hour)
    (output hour "Hour deleted." "hourly/~a/~a" (dm:field task "project") (dm:id task))))
