(in-package :cl-user)
(defpackage slsl
  (:use :cl)
  (:import-from :slsl.user
                #:user
                #:bot)
  (:import-from :slsl.channel
                #:topic
                #:purpose
                #:channel
                #:im)
  (:import-from :slsl.message
                #:message)
  (:import-from :slsl.schedule
                #:schedule)
  (:import-from :slsl.client
                #:client
                #:ws
                #:users
                #:channels
                #:enqueue-message
                #:send-messages
                #:add-schedule
                #:dispatch-schedule
                #:start
                #:complete-posting-message
                #:*client*
                #:find-user-by-id
                #:find-channel-by-id)
  (:import-from :slsl.format
                #:encode
                #:decode
                #:user-reference
                #:mention)
  (:import-from :alexandria
                #:with-gensyms
                #:once-only)
  (:import-from :jsown
                #:parse
                #:val)
  (:import-from :optima
                #:match
                #:ematch
                #:defpattern
                #:place)
  (:import-from :websocket-driver
                #:on
                #:start-connection)
  (:import-from :bordeaux-threads
                #:make-lock
                #:with-lock-held)
  (:export #:make-client
           #:channel
           #:user
           #:post
           #:find-user-by-id
           #:find-channel-by-id
           #:recieve
           #:start
           #:with-connection
           #:on-event
           #:on-event*
           #:on-event-once
           #:on-event-once*
           #:on-time
           #:on-time*
           #:remove-event-listener
           #:remove-schedule
           #:obj
           #:*debug-stream*))
(in-package :slsl)


(defvar *event-listener-table* nil)
(defvar *debug-stream* nil)


(defpattern obj (&rest args)
  `(list* :obj
          (and ,@(loop
                    while args
                    for key = (pop args)
                    for value = (pop args)
                    collect `(assoc ,(string-downcase key)
                                    ,value
                                    :test string=)))))


(defun make-user-from-json (json)
  (ematch json
    ((obj :id      id
          :name    name
          :profile (list* :obj profile)
          :deleted deleted-p
          :is_bot bot-p)
     (make-instance 'user
                    :id id
                    :name name
                    :profile profile
                    :deleted-p deleted-p
                    :bot-p bot-p))))

(defun make-bot-from-json (json)
  (ematch json
    ((obj :id      id
          :name    name
          :icons (list* :obj icons)
          :deleted deleted-p)
     (make-instance 'bot
                    :id id
                    :name name
                    :icons icons
                    :deleted-p deleted-p))))

(defun make-topic-form-json (json)
  (ematch json
    ((obj :value value
          :creator creator
          :last_set last-set)
     (make-instance 'topic
                    :value value
                    :creator creator
                    :last-set last-set))))

(defun make-purpose-form-json (json)
  (ematch json
    ((obj :value value
          :creator creator
          :last_set last-set)
     (make-instance 'purpose
                    :value value
                    :creator creator
                    :last-set last-set))))

(defun make-channel-type-from-json (json)
  (ematch json
    ((obj :is_im t) :im)
    ((obj :is_mpim t) :mpim)
    ((obj :is_group t) :private-channel)
    ((obj :is_channel t) :channel)))

(defun make-channel-from-json (users json)
  (ematch json
    ;; im
    ((obj :id id
          :user user
          :is_im t)
     (make-instance 'im
                    :id id
                    :user (find user users :key #'slsl.user:id :test #'string=)))
    ;; interested
    ((obj :id id
          :name name
          :members members
          :is_archived archived-p
          :topic topic
          :purpose purpose)
     (make-instance 'channel
                    :id id
                    :type (make-channel-type-from-json json)
                    :name name
                    :archived-p archived-p
                    :members (mapcar (lambda (user-id)
                                       (find user-id users :key #'slsl.user:id :test #'string=))
                                     (match json ((obj :members members) members)))
                    :topic (make-topic-form-json topic)
                    :purpose (make-purpose-form-json purpose)))
    ;; uninterested
    ((obj :id id
          :name name
          :is_archived archived-p)
     (make-instance 'channel
                    :id id
                    :type (make-channel-type-from-json json)
                    :name name
                    :archived-p archived-p
                    :members nil
                    :topic nil
                    :purpose nil))))


(defun make-client (token)
  (let ((response
         (jsown:parse (dexador:post "https://slack.com/api/rtm.start"
                                    :content `(("token" . ,token))))))
    (unless (val response "ok")
      (error "rtm.start failed: ~a" response))

    (let* ((ws-url (val response "url"))
           (ws-client (wsd:make-client ws-url))
           (users (append (mapcar #'make-user-from-json (val response "users"))
                          (mapcar #'make-bot-from-json (val response "bots"))))
           (channels (mapcar (lambda (x) (make-channel-from-json users x))
                             (append (val response "channels")
                                     (val response "groups")
                                     (val response "ims"))))
           (self (find (val (val response "self") "id")
                       users
                       :key #'slsl.user:id
                       :test #'string=))
           (client (make-instance 'client
                                  :token token
                                  :ws ws-client
                                  :users users
                                  :channels channels
                                  :self self)))
      (wsd:on :error ws-client
              (lambda (json)
                (format *debug-stream* "websocket error: ~a~%" json)))
      client)))


(defmacro with-connection (token &body body)
  `(let ((*client* (make-client ,token))
         (*event-listener-table* (make-hash-table)))
     ,@body))

(defun dispatch-event (event)
  (maphash (lambda (name event-listener)
             (declare (ignorable name))
             (funcall event-listener event))
           *event-listener-table*))


(defun dispose-event (string &aux (obj (parse string)) fall)
  (match obj
    ((obj :reply_to reply-to
          :text text
          :ok t
          :ts timestamp)
     (complete-posting-message *client* reply-to timestamp)
     (setf fall t))

    ((obj :type "message"
          :reply_to reply-to
          :channel channel
          :user user
          :text text
          :ts timestamp)
     (setf fall t))

    ((obj :type "user_change"
          :user (obj :id      id
                     :name    name*
                     :profile (list* :obj profile*)
                     :deleted deleted-p*
                     :is_bot  bot-p*))
     (let ((user (find-user-by-id id)))
       (setf (slot-value user 'slsl.user:name) name*
             (slot-value user 'slsl.user:profile) profile*
             (slot-value user 'slsl.user:deleted-p) deleted-p*
             (slot-value user 'slsl.user:bot-p) bot-p*)))

    ((obj :type "channel_created"
          :channel (obj :id      channel-id
                        :name    channel-name
                        :created channel-created
                        :creator channel-creator)
          :event_ts _)
     (push (make-instance 'channel
                          :id channel-id
                          :type :channel
                          :name channel-name
                          :archived-p nil
                          :members (list (find-user-by-id channel-creator)) ; members?
                          :topic nil
                          :purpose nil)
           (channels *client*)))

    ((obj :type "channel_deleted"
          :channel id
          :event_ts _)
     (setf (channels *client*) (remove (find-channel-by-id id) (channels *client*))))

    ((obj :type "im_created"
          :user user
          :channel (obj :id      channel-id
                        :user    channel-user
                        :is_im   t
                        :created channel-created
                        :creator channel-creator)
          :event_ts _)
       (push (make-instance 'im
                            :id channel-id
                            :user (find-user-by-id channel-user))
             (channels *client*)))

    ((obj :type "channel_joined"
          :channel (obj :id      channel-id
                        :name    channel-name
                        :created channel-created
                        :creator channel-creator
                        :is_archived is-archived
                        :is_general  is-general
                        :is_member is-member
                        :members channel-members
                        :topic (obj :value topic-value
                                    :creator topic-creator
                                    :last_set topic-last-set)
                        :purpose (obj :value purpose-value
                                      :creator purpose-creator
                                      :last_set purpose-last-set)))
     (let ((channel (find-channel-by-id channel-id)))
       (unless channel
         (error "Channel ~a missing!" channel-id))
       (setf (slot-value channel 'slsl.channel:name)
             channel-name
             (slot-value channel 'slsl.channel:archived-p)
             is-archived
             (slot-value channel 'slsl.channel:members)
             (mapcar #'find-user-by-id channel-members)
             (slot-value channel 'slsl.channel:topic)
             (make-instance 'topic
                            :value topic-value
                            :creator topic-creator
                            :last-set topic-last-set)
             (slot-value channel 'slsl.channel:purpose)
             (make-instance 'purpose
                            :value purpose-value
                            :creator purpose-creator
                            :last-set purpose-last-set))))

    ((obj :type "channel_rename"
          :channel (obj :id      channel-id
                        :name    channel-name)
          :event_ts _)
     (let ((channel (find-channel-by-id channel-id)))
       (unless channel
         (error "Channel not found: ~a" channel-id))
       (setf (slot-value channel 'slsl.channel:name) channel-name)))

    ((obj :type "team_join"
          :user user
          :event_ts _)
     (push (make-user-from-json user)
           (users *client*)))

    ((obj :type "bot_added"
          :bot bot
          :event_ts _)
     (push (make-bot-from-json bot)
           (users *client*)))

    ((obj :type "bot_changed"
          :bot (obj :id   id
                    :name name*)
          :event_ts _)
     (let ((bot (find-user-by-id id)))
       (setf (slot-value bot 'slsl.user:name) name*))))
    (unless fall
     (dispatch-event obj)))

(defmacro on-event (name pattern &body body)
  `(progn
     (setf (gethash ',name *event-listener-table*)
           (lambda (event)
             (match event
               (,pattern
                ,@body))))
     ',name))

(defmacro on-event* (pattern &body body)
  `(on-event ,(gensym "ANONYMOUS")
             ,pattern
             ,@body))

(defmacro on-event-once (name pattern &body body)
  `(on-event ,name
             ,pattern
             ,@body
             (remove-event-listener ',name)))

(defmacro on-event-once* (pattern &body body)
  `(on-event-once ,(gensym "ANONYMOUS")
                  ,pattern
                  ,@body))


(defmacro on-time (name spec-form &body body)
  `(add-schedule *client* ',name ,spec-form (lambda () ,@body)))

(defmacro on-time* (spec-form &body body)
  `(on-time ,(gensym "ANONYMOUS")
            ,spec-form
            ,@body))


(defun remove-event-listener (name)
  (remhash name *event-listener-table*))

(defun remove-schedule (name)
  (slsl.client:remove-schedule *client* name))

(defun start (&optional thunk)
  (let ((received-event-queue '())
        (received-event-queue-lock (make-lock)))
    (as:with-event-loop ()
      ;; signal handling
      (as:signal-handler 2 (lambda (sig)
                             (declare (ignore sig))
                             (as:exit-event-loop)))

      ;; enqueue received event
      (on :message (ws *client*)
          (lambda (event)
            (with-lock-held (received-event-queue-lock)
              (push event received-event-queue))))

      (on :error (ws *client*)
          (lambda (error)
            (format t "Got an ws error: ~S~%" error)))

      (on :close (ws *client*)
          (lambda (code reason)
            (format t "Ws closed because '~A' (Code=~A)~%" reason code)))

      ;; start websocket connection
      (start-connection (ws *client*))

      (as:with-interval (0.1)
        ;; event disposing
        (with-lock-held (received-event-queue-lock)
          (loop
             for event in (nreverse received-event-queue)
             do (handler-case (dispose-event event)
                  (error (c)
                    (format t "Error: ~a~%~a~%"
                            c
                            event))))

          (setf received-event-queue '()))

        ;; schedule dispatching
        (dispatch-schedule *client*)

        ;; send event
        (send-messages *client*))

      (when thunk
        ;; user function
        (funcall thunk)))))


(defun user (user-name)
  (find user-name (users *client*)
        :key #'slsl.user:name
        :test #'string=))

(defun channel (channel-name)
  (find channel-name (channels *client*)
        :key #'slsl.channel:name
        :test #'string=))

(defun %post (channel-id text &optional then)
  (enqueue-message *client*
                   (make-instance 'message
                                  :channel channel-id
                                  :text text
                                  :then then)))

(defmacro post (channel-id text &body body)
  (if body
      `(%post ,channel-id ,text :then (lambda () ,@body))
      `(%post ,channel-id ,text)))
