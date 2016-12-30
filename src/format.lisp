(in-package :cl-user)
(defpackage slsl.format
  (:use :cl :optima.core)
  (:import-from :slsl.client
                #:users
                #:channels
                #:*client*
                #:find-user-by-id
                #:find-channel-by-id
                #:find-user-by-name
                #:find-channel-by-name)
  (:import-from :optima
                #:match
                #:multiple-value-match
                #:defpattern)
  (:import-from :alexandria
                #:with-unique-names)
  (:import-from :cl-ppcre
                #:scan
                #:scan-to-strings
                #:all-matches-as-strings)
  (:export #:encode
           #:decode
           #:user-reference
           #:mention
           #:user
           #:channel))
(in-package :slsl.format)


(defun unbracket (text)
  (multiple-value-match (scan-to-strings "<(.)([A-Z0-9]+)>" text)
    ((_ (vector "@" user-id))    (find-user-by-id user-id))
    ((_ (vector "#" channel-id)) (find-channel-by-id channel-id))))

(defun encode (text)
  (apply #'concatenate
         'string
         (encode* text)))

(defun encode* (text &optional (start 0) (end (length text)))
  "https://api.slack.com/docs/message-formatting"
  (multiple-value-bind (start1 end1)
      (scan "<|>|&|[@#]([-a-zA-Z0-9._])+" text :start start :end end) ; ほんとはこうしたい "<|>|&|(?<=^|\s)@(\\w|-|\\.)+" けど cl-ppcre が対応してない
    (if start1
        (cond
          ((null start1)
           (list (subseq text start)))
          ((string= text "<" :start1 start1 :end1 end1)
           (list* (subseq text start start1) "&lt;" (encode* text end1 end)))
          ((string= text ">" :start1 start1 :end1 end1)
           (list* (subseq text start start1) "&gt;" (encode* text end1 end)))
          ((string= text "&" :start1 start1 :end1 end1)
           (list* (subseq text start start1) "&amp;" (encode* text end1 end)))
          (t
           (let* ((symbol (subseq text start1 (1+ start1)))
                  (name (subseq text (1+ start1) end1))
                  (user-or-channel (if (string= symbol "@")
                                       (find-user-by-name name)
                                       (find-channel-by-name name))))
             (if (and (or (= start1 0)
                          (member (aref text (1- start1))
                                  '(#\SPACE #\IDEOGRAPHIC_SPACE #\Tab #\Newline #\Return)))
                      user-or-channel)
                 (list* (subseq text start start1)
                        "<" symbol
                        (if (string= symbol "@")
                            (slsl.user:id user-or-channel)
                            (slsl.channel:id user-or-channel)) ">"
                        (encode* text end1 end))
                 (list* (subseq text start end1)
                        (encode* text end1 end)))))))))


(defvar *users-mention-to* nil)

(defun decode (text)
  (let* ((*users-mention-to* nil)
         (parts (decode* text)))
    (values (format nil "~{~a~}" parts) *users-mention-to*)))

(defun decode* (text &optional (start 0) (end (length text)))
  (multiple-value-bind (start1 end1)
      (scan "&lt;|&gt;|&amp;|<[@#][A-Z0-9]+(?:\\|[^>]+)?>" text :start start :end end) ; TODO channelに対応
    (if start1
        (cond
          ((string= text "&lt;" :start1 start1 :end1 end1)
           (list* (subseq text start start1) "<" (decode* text end1 end)))
          ((string= text "&gt;" :start1 start1 :end1 end1)
           (list* (subseq text start start1) ">" (decode* text end1 end)))
          ((string= text "&amp;" :start1 start1 :end1 end1)
           (list* (subseq text start start1) "&" (decode* text end1 end)))
          (t
           (let* ((symbol (subseq text (+ start1 1) (+ start1 2)))
                  (id (scan-to-strings "[A-Z0-9]+" text :start (+ start1 2)))
                  (user-or-channel (if (string= symbol "@")
                                       (find-user-by-id id)
                                       (find-channel-by-id id))))
             (when (typep user-or-channel 'slsl.user:user)
               (push user-or-channel *users-mention-to*))
             (if user-or-channel
                 (list* (subseq text start start1) symbol
                        (if (string= symbol "@")
                            (slsl.user:name user-or-channel)
                            (slsl.channel:name user-or-channel))
                        (decode* text end1 end))
                 (list* (subseq text start start1)
                        (subseq text start1 end1)
                        (decode* text end1 end))))))
        (list (subseq text start)))))


;;; user-reference pattern: matches list of user references in the message with sub-pattern.
(defstruct (user-reference-pattern (:include constructor-pattern)
                                   (:constructor make-user-reference-pattern (user-list &aux (subpatterns (list user-list))))))

(defmethod constructor-pattern-destructor-sharable-p ((x user-reference-pattern) (y user-reference-pattern))
  t)

(defmethod constructor-pattern-make-destructor ((pattern user-reference-pattern) var)
  (with-unique-names (it)
    (make-destructor :bindings `((,it (and (stringp ,var)
                                           (remove-duplicates (mapcar #'unbracket
                                                                      (all-matches-as-strings "<@[A-Z0-9]+>" ,var))))))
                     :predicate-form t
                     :accessor-forms (list it))))

(defmethod parse-constructor-pattern ((name (eql 'user-reference)) &rest args)
  (apply #'make-user-reference-pattern
         (mapcar #'parse-pattern args)))

(defmethod unparse-pattern ((pattern user-reference-pattern))
  `(user-reference ,@(user-reference-pattern-subpatterns pattern)))


;;; mention pattern: extracts into users and other text, then matches these with each sub-pattern.
;;; e.g. (match "<@FOO>: hello!" ((mention (list (equal user-foo)) text) text)) ; => "hello!"
(defstruct (mention-pattern (:include constructor-pattern)
                            (:constructor make-mention-pattern (&rest subpatterns))))

(defmethod constructor-pattern-destructor-sharable-p ((x mention-pattern) (y mention-pattern))
  t)

(defmethod constructor-pattern-make-destructor ((pattern mention-pattern) var)
  (with-unique-names (it)
    (make-destructor :bindings `((,it (and (stringp ,var)
                                           (multiple-value-match
                                               (scan-to-strings "^[\\s:]*((?:<@[A-Z0-9]+>|[\\s:])*)(.*)$" ,var)
                                             ((_ (vector brackets body))
                                              (list (remove nil
                                                            (mapcar #'unbracket
                                                                    (all-matches-as-strings "<@[A-Z0-9]+>" brackets)))
                                                    body))))))
                     :predicate-form it
                     :accessor-forms `((first ,it) (second ,it)))))

(defmethod parse-constructor-pattern ((name (eql 'mention)) &rest args)
  (apply #'make-mention-pattern
         (mapcar #'parse-pattern args)))

(defmethod unparse-pattern ((pattern mention-pattern))
  `(mention ,@(mention-pattern-subpatterns pattern)))


;;; channel pattern
(defstruct (channel-pattern (:include constructor-pattern)
                            (:constructor make-channel-pattern (&rest subpatterns))))

(defmethod constructor-pattern-destructor-sharable-p ((x channel-pattern) (y channel-pattern))
  t)

(defmethod constructor-pattern-make-destructor ((pattern channel-pattern) var)
  (with-unique-names (it)
    (make-destructor :bindings `((,it (and (stringp ,var)
                                           (find-channel-by-id ,var))))
                     :predicate-form it
                     :accessor-forms (list it))))

(defmethod parse-constructor-pattern ((name (eql 'channel)) &rest args)
  (apply #'make-channel-pattern
         (mapcar #'parse-pattern args)))

(defmethod unparse-pattern ((pattern channel-pattern))
  `(channel ,@(channel-pattern-subpatterns pattern)))


;;; user pattern
(defstruct (user-pattern (:include constructor-pattern)
                            (:constructor make-user-pattern (&rest subpatterns))))

(defmethod constructor-pattern-destructor-sharable-p ((x user-pattern) (y user-pattern))
  t)

(defmethod constructor-pattern-make-destructor ((pattern user-pattern) var)
  (with-unique-names (it)
    (make-destructor :bindings `((,it (and (stringp ,var)
                                           (find-user-by-id ,var))))
                     :predicate-form it
                     :accessor-forms (list it))))

(defmethod parse-constructor-pattern ((name (eql 'user)) &rest args)
  (apply #'make-user-pattern
         (mapcar #'parse-pattern args)))

(defmethod unparse-pattern ((pattern user-pattern))
  `(user ,@(user-pattern-subpatterns pattern)))
