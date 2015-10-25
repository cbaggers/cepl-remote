(in-package #:cepl-remote)

(defstruct (cepl-remote-server (:constructor %make-cepl-remote-server))
  (socket-server
   nil
   :type t)
  (channel
   (make-instance 'chanl:unbounded-channel)
   :type t))

(defun read-all-remote-messages (server)
  (loop :for message = (chanl:recv (cepl-remote-server-channel server)
                                   :blockp nil)
     :until (null message) :collect message))

;;----------------------------------------------------------------------
;; crap to remove

(defstruct
    (remote-event
      (:include evt::cpl-event
                (source-node cepl-remote)))
  (control-uid (error "control-uid must be provided")
               :type fixnum
               :read-only t)
  (data (make-array 4 :element-type 'single-float
                    :initial-contents '(0s0 0s0 0s0 0s0))
        :type (simple-array single-float (4))
        :read-only t))

(evt:def-named-event-node cepl-remote (e evt:|all-events|
                                         :filter #'remote-event-p
                                         :tags '(:cepl-remote))
  (declare (ignore e))
  nil)

(defun pump-remote-events (server)
  (labels ((dispatch (x)
             (evt:inject-event
              (make-remote-event :control-uid (first x)
                                 :data (second x)))))
    (mapcar #'dispatch (read-all-remote-messages server))))

;;----------------------------------------------------------------------

(defun make-cepl-remote-server (&optional (port 1234))
  (let* ((server (%make-cepl-remote-server))
         (uss (usocket:socket-server
               "127.0.0.1" port #'data-recieved (list server)
               :element-type '(unsigned-byte 8)
               :in-new-thread t
               :reuse-address t
               :multi-threading t)))
    (setf (cepl-remote-server-socket-server server) uss)
    server))

(defun data-recieved (stream cepl-remote-server)
  (handler-case
      (let ((binary-types:*endian* :little-endian))
        (loop :do (chanl:send (cepl-remote-server-channel cepl-remote-server)
                              (read-message stream))))
    (end-of-file (&rest args)
      (format t "Client disconnected~%args:~a~%" args))))

(defun read-message (stream)
  (let ((source-name (read-uint32 stream)))
    (cond
      ((= source-name +announce-source-id+) (handle-announce-source stream))
      ((= source-name +time-sync-id+) (handle-time-sync stream))
      (t (handle-data-event source-name stream)))))

(defun handle-data-event (source-id stream)
  (list source-id (read-vec4 stream)))

(defun read-uint32 (stream)
  (binary-types:read-binary 'binary-types:u32 stream))

(defun read-float32 (stream)
  (ieee-floats:decode-float32 (read-uint32 stream)))

(defun read-vec4 (stream)
  (make-array
   4 :element-type 'single-float
   :initial-contents (loop for i below 4 collect (read-float32 stream))))

(defvar *source-metadata* (make-hash-table))

(defun handle-announce-source (stream)
  (let* ((new-source-id (read-uint32 stream))
         (name-len (read-uint32 stream))
         (name-char-codes (loop for i below name-len collect (read-uint32 stream)))
         (name (format nil "~s ~{~a~}" name-char-codes (mapcar #'code-char name-char-codes))))
    (setf (gethash new-source-id *source-metadata*) (list name))
    (list :new-source-id new-source-id :name name )))

(defun handle-time-sync (stream)
  (let ((client-time (binary-types:read-binary 'binary-types:u64 stream)))
    (format t "time sync ~s" client-time)))


(defconstant +announce-source-id+ 4294967295)
(defconstant +time-sync-id+ 4294967294)

;; ids 0..4294967293 are events from sources
;; +announce-source-id+ is a message defining a new source
;; +time-sync-id+ is a message defining the time sync between
