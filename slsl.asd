#|
  This file is a part of slsl project.
  Copyright (c) 2016 Carrotflakes (carrotflakes@gmail.com)
|#

#|
  Author: Carrotflakes (carrotflakes@gmail.com)
|#

(in-package :cl-user)
(defpackage slsl-asd
  (:use :cl :asdf))
(in-package :slsl-asd)

(defsystem slsl
  :version "0.1"
  :author "Carrotflakes"
  :license "LLGPL"
  :depends-on (:alexandria
               :websocket-driver-client
               :jsown
               :dexador
               :optima
               :cl-ppcre
               :bordeaux-threads
               :clocy)
  :components ((:module "src"
                :components
                ((:file "user")
                 (:file "channel")
                 (:file "message")
                 (:file "schedule")
                 (:file "client" :depends-on ("channel" "message" "schedule"))
                 (:file "format" :depends-on ("client" "user"))
                 (:file "slsl" :depends-on ("client" "message"))
                 (:file "webapi" :depends-on ("message" "channel" "client")))))
  :description "A Slack Developer Kit for Common Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op slsl-test))))
