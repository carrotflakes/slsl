(in-package :cl-user)
(defpackage slsl.client
  (:use :cl)
  (:import-from :jsown
                #:parse
                #:to-json)
  (:import-from :websocket-driver
                #:send
                #:start-connection
                #:on)
  (:import-from :slsl.message
                #:message)
  (:import-from :slsl.schedule
                #:schedule)
  (:import-from :clocy
                #:next)
  (:export #:client
           #:ws
           #:self
           #:enqueue-message
           #:send-messages
           #:add-schedule
           #:remove-schedule
           #:dispatch-schedule
           #:last-timestamp
           #:start
           #:complete-posting-message
           #:enqueue-event
           #:*client*
           #:find-user-by-id
           #:find-channel-by-id
           #:find-user-by-name
           #:find-channel-by-name))
(in-package :slsl.client)

(defclass client ()
  ((token :initarg :token
          :initform (error ":token is required"))
   (ws :initarg :ws
       :initform (error ":ws is required")
       :reader ws)
   (channels :initarg :channels
             :initform (error ":channels is required")
             :accessor channels)
   (users :initarg :users
          :initform (error ":users is required")
          :accessor users)
   (self :initarg :self
         :initform nil
         :reader self)
   (event-id :initform 0
             :accessor event-id)
   (pendings :initform '()
             :accessor pendings)
   (last-timestamp :initform nil
                   :accessor last-timestamp)
   (schedule-queue :initform '() ; queue of (time spec-form lambda)
                   :accessor schedule-queue)
   (message-queue :initform '()
                  :accessor message-queue)))

   '(groups :initarg :groups
          :initform (error ":groups is required")
          :accessor groups)
   '(ims :initarg :ims
          :initform (error ":ims is required")
     :accessor ims)

(defun new-event-id (client)
  (incf (event-id client)))

(defun params-alist (params)
  (when params
    (cons (cons (string-downcase (first params))
                (second params))
          (params-alist (cddr params)))))

(defun send-message (client message)
  (let* ((event-id (new-event-id client))
         (json (to-json `(:obj ("id" . ,event-id)
                               ("type" . "message")
                               ("text" . ,(slsl.message:text message))
                               ("channel" . ,(slsl.channel:id (slsl.message:channel message)))))))
    (send (ws client) json)
    (push (cons event-id message) (pendings client))))

(defun send-messages (client)
  "キューに溜まったイベントを送出"
  (map nil
       (lambda (message)
         (send-message client message))
       (nreverse (message-queue client)))
  (setf (message-queue client) '()))

(defun enqueue-message (client message)
  "イベントをキューに入れる"
  (push message (message-queue client)))


(defun complete-posting-message (client id timestamp)
  (let ((pending (assoc id (pendings client))))
    (when (slsl.message:then (cdr pending))
      (funcall (slsl.message:then (cdr pending))))
    (setf (pendings client) (delete pending (pendings client))
          (last-timestamp client) timestamp)))


(defun enqueue-schedule (client schedule)
    (setf (schedule-queue client)
          (sort (cons schedule (schedule-queue client)) #'< :key #'slsl.schedule:next-time)))

(defun add-schedule (client name generator then &optional (now (get-universal-time)))
  (let* ((schedule (make-instance 'schedule
                                  :name name
                                  :generator generator
                                  :time (next generator)
                                  :then then)))
    (enqueue-schedule client schedule)))

(defun remove-schedule (client name)
  (setf (schedule-queue client)
        (delete name (schedule-queue client) :key #'slsl.schedule:name :test #'equal)))

(defun dispatch-schedule (client &optional (now (get-universal-time)))
  (with-slots (schedule-queue) client
    (when (and schedule-queue
               (<= (slsl.schedule:next-time (first schedule-queue)) now))
      (let* ((schedule (pop schedule-queue))
             (then (slsl.schedule:then schedule))
             (next-time (next (slsl.schedule:generator schedule))))
        (when next-time
          (setf (slsl.schedule:next-time schedule) next-time)
          (enqueue-schedule client schedule))
        (when then
          (funcall then))))))


(defvar *client* nil)


(defun find-channel-by-id (channel-id)
  (find channel-id (channels *client*)
        :key #'slsl.channel:id :test #'string=))

(defun find-user-by-id (user-id)
  (find user-id (users *client*)
        :key #'slsl.user:id :test #'string=))

(defun find-user-by-name (user-name)
  (find user-name (users *client*)
        :key #'slsl.user:name :test #'string=))

(defun find-channel-by-name (channel-name)
  (find channel-name (channels *client*)
        :key #'slsl.channel:name :test #'string=))
