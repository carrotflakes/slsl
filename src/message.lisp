(in-package :cl-user)
(defpackage slsl.message
  (:use :cl)
  (:export #:message
           #:text
           #:channel
           #:then))
(in-package :slsl.message)

(defclass message ()
  ((channel :initarg :channel
            :initform (error ":channel is required")
            :accessor channel)
   (text :initarg :text
         :initform (error ":text is required")
         :accessor text)
   (then :initarg :then
         :initform nil
         :accessor then)))
