(in-package #:hourly)

(defun ensure-id (object)
  (etypecase object
    (db:id object)
    (dm:data-model (dm:id object))
    (user:user (user:id object))))

(defun fixup-ids (dms field)
  (dolist (dm dms dms)
    (setf (dm:field dm "_id") (dm:field dm field))))

(defun int* (thing &optional default)
  (if (and thing (string/= thing ""))
      (parse-integer thing)
      default))

(defun time* (thing &optional default)
  (if (and thing (string/= thing ""))
      (or (cl-ppcre:register-groups-bind (y m d hh mm ss) ("(?:(\\d+)-(\\d+)-(\\d+)T)?(\\d+):(\\d+)(?::(\\d+))?" thing)
            (encode-universal-time (parse-integer (or ss "0"))
                                   (parse-integer mm)
                                   (parse-integer hh)
                                   (parse-integer (or d "1"))
                                   (parse-integer (or m "1"))
                                   (parse-integer (or y "1900"))
                                   0))
          (parse-integer thing))
      default))

(defun start-of-day (&optional (time (get-universal-time)))
  (multiple-value-bind (ss mm hh d m y) (decode-universal-time time 0)
    (declare (ignore ss mm hh))
    (encode-universal-time 0 0 0 d m y 0)))

(defun end-of-day (&optional (time (get-universal-time)))
  (+ (start-of-day time) (1- (* 24 60 60))))

(defun day-of-year (&optional (time (get-universal-time)))
  (1+ (floor (- time (encode-universal-time 0 0 0 1 1 (nth-value 5 (decode-universal-time time)) NIL))
             (* 24 60 60))))

(defun url> (uri &key query fragment)
  (uri-to-url uri :representation :external
                  :query query
                  :fragment fragment))

(defun mktable (&rest entries)
  (let ((table (make-hash-table)))
    (loop for (k v) on entries by #'cddr
          do (setf (gethash k table) v))
    table))

(defmacro setf-dm-fields (model &rest vars)
  (let ((modelg (gensym "MODEL")))
    `(let ((,modelg ,model))
       ,@(loop for var in vars
               collect (destructuring-bind (var &optional (field (string-downcase var))) (radiance::enlist var)
                         `(typecase ,var
                            (null)
                            (user:user
                             (setf (dm:field ,modelg ,field) (user:id ,var)))
                            (dm:data-model
                             (setf (dm:field ,modelg ,field) (dm:id ,var)))
                            (T
                             (setf (dm:field ,modelg ,field) ,var)))))
       ,modelg)))

(defun check-accessible (dm &key (level 1) (target (dm:collection dm)) (user (auth:current)))
  (labels ((check (author)
             (unless (equal (user:id user) author)
               (error 'radiance:request-denied :message (format NIL "You do not own the ~a you were trying to access."
                                                                (dm:collection dm)))))
           (check-project (project)
             (let* ((project (ensure-project project))
                    (record (db:select 'access
                                       (db:query (:and (:= 'project (dm:id project))
                                                       (:= 'user (user:id user))))
                                       :amount 1 :fields '(level)))
                    (active-level (if record
                                      (gethash "level" (first record))
                                      0)))
               (unless (or (equal (user:id user) (dm:field project "author"))
                           (<= level active-level))
                 (error 'radiance:request-denied :message (format NIL "You do not have permission to access ~as." target))))))
    (ecase (dm:collection dm)
      (project
       (check-project dm))
      (task
       (unless (equal (user:id user) (dm:field dm "author"))
         (check-project (dm:field dm "project"))))
      (hour
       (check (dm:field dm "logger"))))
    dm))
