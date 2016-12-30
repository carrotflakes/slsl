(in-package :cl-user)
(defpackage slsl.webapi
  (:use :cl)
  (:import-from :slsl.message
                #:message)
  (:import-from :slsl.client
                #:*client*)
  (:export #:post-message
           #:files-upload
           #:update
           #:reactions-add
           #:reactions-remove))
(in-package :slsl.webapi)

(defun post-message (channel text
                     &key username icon-url icon-emoji
                     &aux (as-user (not (or username icon-url icon-emoji))))
  (jsown:parse
   (dexador:post "https://slack.com/api/chat.postMessage"
                 :content `(("token" . ,(slot-value *client* 'slsl.client::token))
                            ("channel" . ,channel)
                            ("text" . ,text)
                            ,@(when username `(("username" . ,username)))
                            ,@(when icon-url `(("icon_url" . ,icon-url)))
                            ,@(when icon-emoji `(("icon_emoji" . ,icon-emoji)))
                            ,@(when as-user `(("as_user" . ,as-user)))))))

(defun files-upload (filename content &key channels) ; TODO
  (jsown:parse
   (dexador:post "https://slack.com/api/files.upload"
                 :content `(("token" . ,(slot-value *client* 'slsl.client::token))
                            ("content" . ,content)
                            ,@(when channels
                                    `(("channels" . ,(format nil "~{~a~^,~}" channels))))))))

(defun update (channel timestamp text)
  (jsown:parse
   (dexador:post "https://slack.com/api/chat.update"
                 :content `(("token" . ,(slot-value *client* 'slsl.client::token))
                            ("channel" . ,channel)
                            ("ts" . ,timestamp)
                            ("text" . ,text)))))

(defun reactions-add (name
                     &key file file-comment channel timestamp)
  (jsown:parse
   (dexador:post "https://slack.com/api/reactions.add"
                 :content `(("token" . ,(slot-value *client* 'slsl.client::token))
                            ("name" . ,name)
                            ,@(when file `(("file" . ,file)))
                            ,@(when file-comment `(("file_comment" . ,file-comment)))
                            ,@(when channel `(("channel" . ,channel)))
                            ,@(when timestamp `(("timestamp" . ,timestamp)))))))

(defun reactions-remove (name
                         &key file file-comment channel timestamp)
  (jsown:parse
   (dexador:post "https://slack.com/api/reactions.remove"
                 :content `(("token" . ,(slot-value *client* 'slsl.client::token))
                            ("name" . ,name)
                            ,@(when file `(("file" . ,file)))
                            ,@(when file-comment `(("file_comment" . ,file-comment)))
                            ,@(when channel `(("channel" . ,channel)))
                            ,@(when timestamp `(("timestamp" . ,timestamp)))))))
