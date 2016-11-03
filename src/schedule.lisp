(in-package :cl-user)
(defpackage slsl.schedule
  (:use :cl)
  (:export #:schedule
           #:name
           #:generator
           #:next-time
           #:then))
(in-package :slsl.schedule)

(defclass schedule ()
  ((name :initarg :name
         :initform (gensym "ANONYMOUS")
         :accessor name)
   (generator :initarg :generator
              :initform (error ":generator is required")
              :accessor generator)
   (next-time :initarg :time
              :initform (error ":next-time is required")
              :accessor next-time)
   (then :initarg :then
         :initform nil
         :accessor then)))
