(in-package :cl-user)
(defpackage slsl.pattern
  (:use :cl :optima.core)
  (:import-from :slsl
                #:obj)
  (:import-from :optima
                #:defpattern)
  (:export #:event-message))
(in-package :slsl.pattern)


(defpattern event-message (&rest args)
  `(obj :type "message"
             :channel ,(first args)
             :user ,(second args)
             :text ,(third args)
             :ts ,(fourth args)))
