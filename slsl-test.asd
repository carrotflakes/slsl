#|
  This file is a part of slsl project.
  Copyright (c) 2016 carrotflakes (carrotflakes@gmail.com)
|#

(in-package :cl-user)
(defpackage slsl-test-asd
  (:use :cl :asdf))
(in-package :slsl-test-asd)

(defsystem slsl-test
  :author "carrotflakes"
  :license "LLGPL"
  :depends-on (:slsl
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "slsl"))))
  :description "Test system for slsl"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
