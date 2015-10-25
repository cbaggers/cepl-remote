(in-package #:cepl-remote)

;;----------------------------------------------------------------------
;; crap to remove

;; (defun pump-remote-events (server)
;;   (labels ((dispatch (x)
;;              (send-x (make-cepl-event x))))
;;     (mapcar #'dispatch (read-all-remote-messages server))))

;;----------------------------------------------------------------------
;; base event

(defstruct+methods cpl-event
  (source-node (error "source-node is mandatory")
          :type cepl-event-node
          :read-only t)
  (timestamp (get-internal-real-time)
             :type fixnum
             :read-only t
             :reader timestamp))

;;----------------------------------------------------------------------
;; event nodes

(defconstant +default-node-name+ :no-human-name)

(defstruct (cepl-event-node (:constructor %make-cepl-event-node)
                              (:conc-name ces-))
  (uid (gensym "EVENT-NODE-")
       :type symbol)
  (name +default-node-name+
        :type symbol)
  (tags nil
        :type list
        :read-only t)
  (subscribers (make-event-node-subscribers) ;; weak refs to consumers
               :type event-node-subscribers
               :read-only t)
  (subscriptions (make-event-node-subscriptions) ;; strong refs to nodes
                 :type event-node-subscriptions
                 :read-only t)
  (filter #'event-no-filter
          :type function
          :read-only t)
  (body #'event-no-body
          :type function
          :read-only t))

(defmethod print-object ((object cepl-event-node) stream)
  (format stream "#<CEPL-EVENT-NODE ~@[:NAME ~S ~]:UID ~s>"
          (ces-name object)
          (ces-uid object)))

(defun event-node-eq (node-a node-b)
  (eq (ces-uid node-a) (ces-uid node-b)))

(defun event-no-filter (e)
  (declare (ignore e) (cpl-event e))
  t)

(defun event-no-body (e)
  (declare (ignore e) (cpl-event e))
  nil)

(defstruct (event-node-subscribers (:conc-name cess-))
  (subscribers nil :type list))

(defstruct (event-node-subscriptions (:conc-name cesp-))
  (subscriptions nil :type list))

(defun make-cepl-event-node (&key name tags (filter #'event-no-filter)
                               (body #'event-no-body))
  (assert (typep filter 'function))
  (assert (typep body 'function))
  (let ((new-node (%make-cepl-event-node
                     :name (or name +default-node-name+)
                     :tags (if (listp tags) tags (list tags))
                     :filter filter
                     :body body)))
    (when (boundp 'event-system-meta-node)
      (push-event-to-event-node (symbol-value 'event-system-meta-node)
                                (make-new-event-node-event :new-node new-node)))
    new-node))

(defun push-event-to-event-node (node event)
  (when (funcall (ces-filter node) event)
    (funcall (ces-body node) event)
    (push-event-to-subscribers node event)))

(defun push-event-to-subscribers (node event)
  (labels ((push-to-subscriber (subscriber)
             (let ((subscribed-node (trivial-garbage:weak-pointer-value subscriber)))
               (when subscribed-node
                   (push-event-to-event-node subscribed-node event)
                 subscriber))))
    (let ((subscribers (cess-subscribers (ces-subscribers node))))
      (mapcar #'push-to-subscriber subscribers))))

;;----------------------------------------------------------------------
;; backend event

;; an event that also contains the backend specific event is represents
(defstruct (cpl-backend-event (:include cpl-event))
  (backend-event (error "backend event is mandatory")
                 :type t
                 :read-only t))

;;----------------------------------------------------------------------
;; meta events

(defvar event-system-meta-node
  (make-cepl-event-node :name :cepl-event-system
                        :tags :cepl-event-system-meta))

(defstruct
    (new-event-node-event
      (:include cpl-event
                (source-node event-system-meta-node)))
  (new-node (error "new-node must be provided")
              :type cepl-event-node
              :read-only t))

;;----------------------------------------------------------------------
;; cepl system events

(defvar cepl-system-event-node
  (make-cepl-event-node
   :name 'cepl-internals
   :tags '(:cepl-internal :system)))

(defstruct (context-created-event (:include cpl-event)))

(defstruct
    (will-quit-event
      (:include cpl-backend-event
                (source-node cepl-system-event-node))))

;;----------------------------------------------------------------------
;; cepl window events

(defvar cepl-window-node
  (make-cepl-event-node
   :name 'cepl-window
   :tags '(:window)))

(defstruct
    (win-event
      (:include cpl-backend-event
                (source-node cepl-window-node))))

;;----------------------------------------------------------------------
;; cepl mouse events

(defvar cepl-mouse-node
  (make-cepl-event-node
   :name 'cepl-mouse
   :tags '(:mouse)))

(defstruct+methods
    (cepl-mouse-event
     (:include cpl-backend-event
               (source-node cepl-mouse-node)))
  (mouse-id (error "mouse-scroll event requires mouse id")
            :type fixnum
            :read-only t
            :reader id))

(defstruct+methods (mouse-scroll (:include cepl-mouse-event))
  (vec (error "mouse-scroll event requires data")
       :type (simple-array single-float (3))
       :read-only t
       :reader cepl-generics::vec))

(defstruct+methods (mouse-button (:include cepl-mouse-event))
  (button (error "mouse-button event requires button name")
          :type keyword
          :read-only t
          :reader button)
  (state (error "mouse-button event requires state name")
         :type keyword
         :read-only t
         :reader state)
  (clicks (error "mouse-button event requires clicks count")
          :type fixnum
          :read-only t
          :reader clicks)
  (pos (error "mouse-button event requires position")
       :type (simple-array single-float (3))
       :read-only t
       :reader cepl-generics::pos))

(defstruct+methods (mouse-motion (:include cepl-mouse-event))
  (state (error "mouse-button event requires state name")
         :type keyword
         :read-only t
         :reader state)
  (delta (error "mouse-button event requires a delta")
         :type (simple-array single-float (3))
         :read-only t
         :reader delta)
  (pos (error "mouse-button event requires position")
       :type fixnum
       :read-only t
       :reader cepl-generics::pos))

;;----------------------------------------------------------------------
;; cepl keyboard events

(defvar cepl-keyboard-node
  (make-cepl-event-node
   :name 'cepl-keyboard
   :tags '(:keyboard)))

(defstruct+methods
    (cepl-keyboard-event
     (:include cpl-backend-event
               (source-node cepl-keyboard-node))))

(defstruct+methods (key (:include cepl-keyboard-event))
  (etype (error "mouse-button event requires etype name")
         :type keyword
         :read-only t
         :reader etype)
  (state (error "mouse-button event requires state name")
         :type keyword
         :read-only t
         :reader state)
  (repeating (error "mouse-button event requires repeating info")
             :type boolean
             :read-only t
             :reader repeating)
  (key (error "mouse-button event requires key name")
       :type keyword
       :read-only t
       :reader key))


;;----------------------------------------------------------------------

(defvar remote-0 (make-cepl-event-node :tags :remote))

(defstruct (remote-event (:include cpl-event))
  (control-uid (error "control-uid must be provided")
               :type fixnum
               :read-only t)
  (data (make-array 4 :initial-contents '(0s0 0s0 0s0 0s0))))

;;----------------------------------------------------------------------
;; named event nodes

(defvar *named-event-nodes* nil)

(defun subscribe (node source-node)
  (assert (typep node 'cepl-event-node))
  (assert (typep source-node 'cepl-event-node))
  (unless (event-node-already-subscribed node source-node)
    (push (trivial-garbage:make-weak-pointer node)
          (cess-subscribers (ces-subscribers source-node)))
    (push source-node (cesp-subscriptions (ces-subscriptions node))))
  source-node)

(defun unsubscribe (node source-node)
  (assert (typep node 'cepl-event-node))
  (assert (typep source-node 'cepl-event-node))
  (let ((subscribers (ces-subscribers source-node)))
    (setf (cess-subscribers subscribers)
          (delete node (cess-subscribers subscribers)
                  :key #'trivial-garbage:weak-pointer-value)))
  (let ((subscriptions (ces-subscriptions source-node)))
    (setf (cesp-subscriptions subscriptions)
          (delete node (cesp-subscriptions subscriptions))))
  source-node)

(defun event-node-already-subscribed (node source-node)
  (member node (cess-subscribers (ces-subscribers source-node))
          :test (lambda (x y)
                  (event-node-eq x
                                 (trivial-garbage:weak-pointer-value y)))))

(defun %move-subscriptions (from to)
  (let ((old-subscribers
             (when from
               (remove nil (mapcar #'trivial-garbage:weak-pointer-value
                                   (cess-subscribers
                                    (ces-subscribers from))))))
        (old-subscriptions
         (when from (cesp-subscriptions (ces-subscriptions from)))))
    ;; recreate all the old subscriptions to and from new node
    (map nil (lambda (x) (subscribe x to)) old-subscribers)
    (map nil (lambda (x) (subscribe to x)) old-subscriptions)
    ;; remove subscriptions from old node
    (map nil (lambda (x) (unsubscribe x from)) old-subscribers)
    (map nil (lambda (x) (unsubscribe from x)) old-subscriptions))
  to)

(defmacro def-named-event-node (name (var parent &key filter tags) &body body)
  `(defparameter ,name
     (let* ((old-node (when (boundp ',name) (symbol-value ',name)))
            (result (make-cepl-event-node
                     :name ',name
                     :tags ',(cepl-utils:listify tags)
                     :filter ,(or filter '(function event-no-filter))
                     :body (lambda (,var) ,@body))))
       (when old-node (%move-subscriptions old-node result))
       (subscribe result ,parent)
       result)))

(def-named-event-node some-events (e cepl-mouse-node :tags :testing)
  (print e))
