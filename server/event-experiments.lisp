(in-package #:cepl-remote)

(defconstant +default-source-name+ :no-human-name)

(defstruct (cepl-event-source (:constructor %make-cepl-event-source))
  (uid (gensym "EVENT-SOURCE-") :type symbol)
  (name +default-source-name+ :type symbol)
  (tags nil :type list :read-only t))

(defun make-cepl-event-source (&key name tags)
  (let ((new-source (%make-cepl-event-source
                     :name (or name +default-source-name+)
                     :tags (if (listp tags) tags (list tags)))))
    (when (boundp 'event-system-meta-source)
      (send-x (make-new-event-source-event :source event-system-meta-source
                                           :new-source new-source)))
    new-source))

;;----------------------------------------------------------------------
;; base event

(defstruct+methods cpl-event
  (source (error "source is mandatory")
          :type cepl-event-source
          :read-only t)
  (timestamp (get-internal-real-time)
             :type fixnum
             :read-only t
             :reader timestamp))

;; an event that also contains the backend specific event is represents
(defstruct (cpl-backend-event (:include cpl-event))
  (backend-event (error "backend event is mandatory")
                 :type t
                 :read-only t))

;;----------------------------------------------------------------------
;; meta events

(defvar event-system-meta-source
  (make-cepl-event-source :name :cepl-event-system
                          :tags :cepl-event-system-meta))

(defstruct
    (new-event-source-event
      (:include cpl-event
                (source event-system-meta-source)))
  (new-source (error "new-source must be provided")
              :type cepl-event-source
              :read-only t))

;;----------------------------------------------------------------------
;; cepl system events

(defvar cepl-system-event-source
  (make-cepl-event-source
   :name 'cepl-internals
   :tags '(:cepl-internal :system)))

(defstruct (context-created-event (:include cpl-event)))

(defstruct
    (will-quit-event
      (:include cpl-backend-event
                (source cepl-system-event-source))))

;;----------------------------------------------------------------------
;; cepl window events

(defvar cepl-window-source
  (make-cepl-event-source
   :name 'cepl-window
   :tags '(:window)))

(defstruct
    (win-event
      (:include cpl-backend-event
                (source cepl-window-source))))

;;----------------------------------------------------------------------
;; cepl mouse events

(defvar cepl-mouse-source
  (make-cepl-event-source
   :name 'cepl-mouse
   :tags '(:mouse)))

(defstruct+methods
    (cepl-mouse-event
     (:include cpl-backend-event
               (source cepl-mouse-source)))
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

(defvar cepl-keyboard-source
  (make-cepl-event-source
   :name 'cepl-keyboard
   :tags '(:keyboard)))

(defstruct+methods (key (:include cepl-mouse-event))
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

(defvar remote-0 (make-cepl-event-source :tags :remote))

(defstruct (remote-event (:include cpl-event))
  (control-uid (error "control-uid must be provided")
               :type fixnum
               :read-only t)
  (data (make-array 4 :initial-contents '(0s0 0s0 0s0 0s0))))


;;----------------------------------------------------------------------

(defun send-x (x) (declare (ignorable x)) nil)

(defun pump-remote-events (server)
  (labels ((dispatch (x)
             (send-x (make-cepl-event x))))
    (mapcar #'dispatch (read-all-remote-messages server))))


(defun make-cepl-event (raw-event)
  (destructuring-bind (element-id vec4) raw-event
    (declare (ignorable element-id vec4))))
