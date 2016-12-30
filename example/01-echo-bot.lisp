(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:slsl)))

(use-package '(slsl))

(with-connection "your api token"
  (let ((self (slsl.client:self slsl.client:*client*)))

    (on-event* (obj :type "message"
                    :text (slsl.format:mention (list (equal self)) body)
                    :user (and (slsl.format:user (not (equal self))) user-id)
                    :channel channel-id)
               (post channel-id (format nil "<@~a> ~a" user-id body)))

    (start)))
