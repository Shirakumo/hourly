(in-package #:hourly)

(define-trigger db:connected ()
  (db:create 'project
             '((author :id)
               (title (:varchar 32))
               (description :text))
             :indices '(author title))

  (db:create 'access
             '((project (:id project))
               (user :id)
               (level (:integer 1)))
             :indices '(project user))

  (db:create 'task
             '((project (:id project))
               (author :id)
               (title (:varchar 32))
               (description :text)
               (time (:integer 5)))
             :indices '(project title))
  
  (db:create 'hour
             '((task (:id task))
               (logger :id)
               (start (:integer 5))
               (end (:integer 5))
               (comment :text))
             :indices '(task logger))

  (db:create 'pin
             '((user :id)
               (task (:id task))
               (label (:varchar 1)))
             :indices '(user)))

(defun list-projects (&key user (skip 0) amount)
  (if user
      (let ((id (ensure-id user)))
        (nconc (dm:get 'project (db:query (:= 'author id))
                       :sort '((title :desc)) :skip skip :amount amount :hull 'project)
               (fixup-ids (dm:get (rdb:join (access project) (project _id)) (db:query (:= 'user id))
                                  :sort '((title :desc)) :skip skip :amount amount :hull 'project)
                          "project")))
      (dm:get 'project (db:query :all)
              :sort '((title :desc)) :skip skip :amount amount)))

(defun find-project (title &optional (errorp T))
  ;; KLUDGE: This is terrible.
  (or (loop for project in (list-projects :user (auth:current))
            do (when (string= title (dm:field project "title"))
                 (return project)))
      (when errorp
        (error "No such project ~a" title))))

(defun ensure-project (project-ish)
  (etypecase project-ish
    (db:id (or (dm:get-one 'project (db:query (:= '_id project-ish)))
               (error "No such project ~a" project-ish)))
    (string
     (or (find-project project-ish NIL)
         (ensure-project (db:ensure-id project-ish))))
    (dm:data-model project-ish)))

(defun make-project (title &key description (author (auth:current)) users)
  (dm:with-model project ('project NIL)
    (setf-dm-fields project title description author)
    (db:with-transaction ()
      (dm:insert project)
      (loop for (user level) in users
            do (db:insert 'access `(("project" . ,(dm:id project))
                                    ("user" . ,(user:id user))
                                    ("level" . ,(or level 1))))))
    project))

(defun edit-project (project &key title description (users NIL users-p))
  (let ((project (ensure-project project)))
    (setf-dm-fields project title description)
    (db:with-transaction ()
      (dm:save project)
      (when users-p
        (let ((existing (dm:get 'access (db:query (:= 'project (dm:id project))))))
          (loop for (user level) in users
                for previous = (find (user:id user) existing :key (lambda (dm) (dm:field dm "user")) :test #'equal)
                do (cond (previous
                          (setf existing (delete previous existing))
                          (setf (dm:field previous "level") (or level 1))
                          (dm:save previous))
                         (T
                          (db:insert 'access `(("project" . ,(dm:id project))
                                               ("user" . ,(user:id user))
                                               ("level" . ,(or level 1)))))))
          (mapc #'dm:delete existing))))
    project))

(defun delete-project (project)
  (db:with-transaction ()
    (mapc #'delete-task (list-tasks project))
    (db:remove 'access (db:query (:= 'project (dm:id project))))
    (dm:delete project)))

(defun list-tasks (project &key (skip 0) amount)
  (dm:get 'task (db:query (:= 'project (ensure-id project)))
          :sort '((time :asc)) :skip skip :amount amount))

(defun find-task (title project &optional (errorp T))
  (or (dm:get-one 'task (db:query (:and (:= 'title title)
                                        (:= 'project (ensure-id project)))))
      (when errorp
        (error "No such task ~a" title))))

(defun ensure-task (task-ish &optional project)
  (etypecase task-ish
    (db:id (or (dm:get-one 'task (db:query (:= '_id task-ish)))
               (error "No such task ~a" task-ish)))
    (string
     (or (when project (find-task task-ish project NIL))
         (ensure-task (db:ensure-id task-ish) project)))
    (dm:data-model task-ish)))

(defun make-task (project title &key description (author (auth:current)) (time (get-universal-time)))
  (let ((project (ensure-project project)))
    (dm:with-model task ('task NIL)
      (setf-dm-fields task project title description author time)
      (dm:insert task))))

(defun edit-task (task &key title description)
  (setf-dm-fields task title description)
  (dm:save task))

(defun delete-task (task)
  (db:with-transaction ()
    (mapc #'delete-hour (list-hours :task task))
    (unpin-task task)
    (dm:delete task)))

(defun list-hours (&key task logger)
  (cond (task
         (dm:get 'hour (db:query (:= 'task (ensure-id task))) :sort '((start :desc))))
        (logger
         (dm:get 'hour (db:query (:= 'logger (user:id logger))) :sort '((start :desc))))))

(defun ensure-hour (hour-ish)
  (etypecase hour-ish
    (db:id (or (dm:get-one 'hour (db:query (:= '_id hour-ish)))
               (error "No such hour ~a" hour-ish)))
    (string
     (ensure-hour (db:ensure-id hour-ish)))
    (dm:data-model hour-ish)))

(defun make-hour (task &key (logger (auth:current)) (start (get-universal-time)) end comment)
  (dm:with-model hour ('hour NIL)
    (setf-dm-fields hour task logger start end comment)
    (dm:insert hour)))

(defun edit-hour (hour &key start end comment)
  (setf-dm-fields hour start end comment)
  (dm:save hour))

(defun delete-hour (hour)
  (dm:delete hour))

(defun current-hour (&key (user (auth:current)) project task)
  (cond (project
         (dm:get-one (rdb:join (hour task) (task _id))
                     (db:query (:and (:null 'end)
                                     (:= 'logger (user:id user))
                                     (:= 'project (dm:id (ensure-project project)))))
                     :sort '((start :desc))))
        (task
         (dm:get-one 'hour (db:query (:and (:null 'end)
                                           (:= 'logger (user:id user))
                                           (:= 'task (dm:id (ensure-task task)))))
                     :sort '((start :desc))))
        (T
         (dm:get-one 'hour (db:query (:and (:null 'end) (:= 'logger (user:id user)))) :sort '((start :desc))))))

(defun find-pin (task &key (user (auth:current)))
  (dm:get-one 'pin  (db:query (:and (:= 'user (user:id user))
                                    (:= 'task (ensure-id task))))))

(defun pin-task (task &key (user (auth:current)) label)
  (db:with-transaction ()
    (let ((current (find-pin task :user user)))
      (cond ((null current)
             (db:insert 'pin `(("user" . ,(user:id user))
                               ("task" . ,(ensure-id task))
                               ("label" . ,(or label "")))))
            (label
             (setf (dm:field current "label") label)
             (dm:save current))))))

(defun unpin-task (task &key (user (auth:current)))
  (let ((pin (find-pin task :user user)))
    (when pin
      (dm:delete pin))))

(defun list-pinned-tasks (&key (user (auth:current)))
  (dm:get (rdb:join (pin task) (task _id)) (db:query (:= 'user (user:id user)))
          :sort '((title :asc))))
