(in-package :cl-user)
(defpackage slsl.pattern
  (:use :cl :optima.core)
  (:import-from :slsl
                #:obj)
  (:import-from :optima
                #:defpattern)
  (:export #:message))
(in-package :slsl.pattern)


(defpattern message (&rest args)
  `(obj :type "message"
        :channel (slsl.format:channel ,(first args))
        :user (slsl.format:user ,(second args))
        :text ,(third args)
        :ts ,(or (fourth args) '_)))
