(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:slsl)))

(use-package '(slsl))

(with-connection "your api token"
  (let ((self (slsl.client:self slsl.client:*client*)))

    (on-event* (obj :type "message"
                    :text (slsl.format:mention (list (equal self)) body)
                    :user (and (not (equal self)) user)
                    :channel channel)
               (post channel (format nil "<@~a> ~a" (slsl.user:id user) body)))

    (start)))
