(in-package :cl-user)
(defpackage slsl.channel
  (:use :cl)
  (:export #:channel
           #:im
           #:id
           #:name
           #:topic
           #:purpose
           #:value
           #:creator
           #:last-set
           #:user
           #:archived-p
           #:members))
(in-package :slsl.channel)

(defclass topic ()
  ((value :initarg :value
          :initform nil
          :reader value)
   (creator :initarg :creator
            :initform nil
            :reader creator)
   (last-set :initarg :last-set
             :initform 0
             :reader last-set)))

(defclass purpose ()
  ((value :initarg :value
          :initform nil
          :reader value)
   (creator :initarg :creator
            :initform nil
            :reader creator)
   (last-set :initarg :last-set
             :initform 0
             :reader last-set)))

(defclass channel ()
  ((id :initarg :id
       :initform (error ":id is required")
       :reader id)
   (name :initarg :name
         :initform (error ":name is required")
         :reader name)
   (type :initarg :type
          :initform (error ":type is required")
          :reader channel-type)
   (archived-p :initarg :archived-p
          :initform (error ":archived-p is required")
          :reader archived-p)
   (members :initarg :members
            :initform nil
            :reader members)
   (topic :initarg :topic
          :initform (make-instance 'topic)
          :reader topic)
   (purpose :initarg :purpose
            :initform (make-instance 'purpose)
            :reader purpose)))

(defclass im (channel)
  ((type :initform :im
         :reader channel-type)
   (name :initform nil)
   (archived-p :initform nil)
   (members :reader members)
   (user :initarg :user
         :initform (error ":user is required")
         :reader user)))

(defmethod initialize-instance :after ((im im) &key user)
  (setf (slot-value im 'members)
        (list user)))
