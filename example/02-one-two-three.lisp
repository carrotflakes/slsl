(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:slsl)))

(use-package '(slsl))

(with-connection "your api token"

  (on-event* (obj :type "message"
                  :text "count"
                  :channel channel)
             (on-time*
              '(after :second 1)
              (post channel "one")
              (on-time*
               '(after :second 1)
               (post channel "two")
               (on-time*
                '(after :second 1)
                (post channel "three!")))))

  (start))
