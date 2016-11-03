(in-package :cl-user)
(defpackage slsl.user
  (:use :cl)
  (:export #:account
           #:user
           #:bot
           #:name
           #:id
           #:deleted-p
           #:bot-p
           #:icons
           #:profile))
(in-package :slsl.user)

(defclass account ()
  ((id :initarg :id
       :initform (error ":id is required")
       :reader id)
   (name :initarg :name
         :initform (error ":name is required")
         :reader name)
   (bot-p :initarg :bot-p
          :initform (error ":bot-p is required")
          :reader bot-p)
   (deleted-p :initarg :deleted-p
              :initform (error ":deleted-p is required")
              :reader deleted-p)))

(defclass user (account)
  ((profile :initarg :profile
            :initform (error ":profile is required")
            :reader profile)
   (bot-p :initarg :bot-p
          :initform (error ":bot-p is required")
          :reader bot-p)))

(defclass bot (account)
  ((icons :initarg :icons
          :initform (error ":icons is required")
          :reader icons)
   (bot-p :initform t
          :reader bot-p)))
