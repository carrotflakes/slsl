(in-package :cl-user)
(defpackage slsl-test
  (:use :cl
        :slsl
        :prove))
(in-package :slsl-test)

;; NOTE: To run this test file, execute `(asdf:test-system :slsl)' in your Lisp.

(plan nil)

(defparameter *client* (make-client))

(defparameter *general* (channel *client* "general"))

(post client *general* "Hello")

(on *client*
    (lambda (json)
      (match json
        ((:type "message" :channel *general* :mension-to "carrotflakes")
         (post *general* "Yo"))
        )))

(on *client*
    (:type "message" :channel "general" :menstion-to "carrotflakes")
    (post "general" "Hello"))

(finalize)
