(in-package #:cepl-remote)

;; This turned out to be a dead end.
;; the idea was that there a nodes children (subscribers) would go stale
;; and be disconnected from the node after some amount of time x
;; x was renewed by the child requesting for events
;; a weakly referenced tick system kept updating any nodes that hadnt been
;; gc'd, thus making them resubscribe.
;; this fails as while something is subscribed it is in the tick list and
;; whilst it is in the tick list it refreshes it's subscription

(defun now () (get-internal-real-time))

(let ((uid 0))
  (defun get-new-node-uid ()
    (incf uid)))

(defclass-triv event-node ()
  (uid (get-new-node-uid))
  subscribers ;; allowed as they go stale
  subscriptions
  cached-events
  (event-cache-time 0)
  (max-allowed-cache-time 10000))

(defclass-triv subscriber ()
  node
  death-time)

(defclass-triv subscription ()
  source-node
  (please-keep-for 1000)
  (last-received-time 0))

(defclass-triv cached-event ()
  event
  death-time)

(let ((subscribers nil))
  (defun tick-subscribers () subscribers)
  (defun subscribe-to-ticks (node)
    (push (trivial-garbage:make-weak-pointer node) subscribers)
    node)
  (defun tick ()
    (setf subscribers
          (loop :for s :in subscribers
             :for refreshed = (let ((obj (trivial-garbage:weak-pointer-value s)))
                                (when obj (update-event-node obj) s))
             :if refreshed :collect refreshed))))

(defun test-tick-loop ()
  (loop :do
     (sleep 0.5)
     (tick)))

(defun update-event-node (node &optional no-resubscribe)
  (let ((count (loop :for subscription :in (subscriptions node) :sum
                  (let ((events (request-new-events node subscription
                                                    no-resubscribe)))
                    (loop :for event :in events :sum
                       (progn (cache-event event node)
                              1))))))
    (clean-cached node)
    (when (> count 0)
      (loop :for subscriber :in (subscribers node) :do
         (update-event-node subscriber t)))))

(defun cache-event (event in)
  (push (make-instance
         'cached-event
         :event event
         :death-time (+ (now) (event-cache-time in)))
        (cached-events in)))

(defun subscribe-to (from to)
  (push (make-instance 'subscription :source-node to) (subscriptions from))
  from)

(defun request-new-events (requestor subscription &optional no-resubscribe)
  (with-slots ((from source-node) (since last-received-time) please-keep-for)
      subscription
    (update-event-cache-time subscription)
    (unless no-resubscribe (update-subscriber requestor subscription))
    (get-events-since from since)))

(defmethod get-events-since ((from event-node) since)
  (loop :for cached-event :in (cached-events from)
     :if (> (timestamp (event cached-event)) since)
     :collect (event cached-event)))

(defun update-subscriber (requestor subscription)
  (with-slots ((from source-node) (since last-received-time) please-keep-for)
      subscription
    (let ((death-time (+ (now) please-keep-for))
          (existing-subscriber (find (uid requestor) (subscribers from)
                                     :key (lambda (x) (uid (node x)))
                                     :test #'=)))
      (if existing-subscriber
          (setf (death-time existing-subscriber) death-time)
          (push (make-instance 'subscriber :death-time death-time
                               :node requestor)
                (subscribers from))))))

(defun update-event-cache-time (subscription)
  (with-slots ((node source-node) (time please-keep-for)) subscription
    (let ((time (min time (max-allowed-cache-time node))))
      (when (> time (event-cache-time node) )
        (setf (event-cache-time node) time)))))

(defmethod clean-cached ((node event-node))
  (let ((now (now)))
    (labels ((stale (thing) (< (death-time thing) now)))
      ;; clean cached events
      (setf (cached-events node)
            (remove-if #'stale (cached-events node)))
      ;; clean cached subscribers
      (setf (subscribers node)
            (remove-if #'stale (subscribers node))))))
