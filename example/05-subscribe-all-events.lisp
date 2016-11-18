(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:slsl)))

(use-package '(slsl))

(with-connection "your api token"

  (on-event* obj
             (format t "~a~%" obj))

  (start))
