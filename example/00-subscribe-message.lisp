(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:slsl)))

(use-package '(slsl))

(with-connection "your api token"

  (on-event* (obj :type "message"
                  :text text
                  :user user
                  :channel channel)
             (format t "~a~10T~a~20T: ~a~%"
                     (slsl.channel:name channel)
                     (slsl.user:name user)
                     (slsl.format:decode text)))

  (start))
