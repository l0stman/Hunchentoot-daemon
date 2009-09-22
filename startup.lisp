;; startup.lisp

;; ports that will be listened on localhost:
;;
;;   *htoot-port* - Hunchentoot (via mod_proxy)
;;   *swank-port* - Swank (via ssh+Emacs)

(defparameter *htoot-port* 8080)
(defparameter *swank-port* 4005)
(defparameter *htoot-server* nil)
(defparameter *swank-server* nil)

(setq hunchentoot:*message-log-pathname* #p"/var/lib/hunchentoot/message.log")
(setq hunchentoot:*dispatch-table* (list 'hunchentoot:dispatch-easy-handlers))

(defun sigterm-handler (sig code scp)
  (declare (ignore sig code scp))
  ;; Shut down Swank and anyone else by terminating all threads
  (dolist (thread (sb-thread:list-all-threads))
    (unless (equal sb-thread:*current-thread* thread)
      (sb-thread:terminate-thread thread)))
  (sleep 1)
  (sb-ext:quit))

;; Start a Hunchentoot server listening for connections
(defun start-htoot ()
  (setf *htoot-server*
	(hunchentoot:start (make-instance 'hunchentoot:acceptor :port *htoot-port*)))
  (princ "Hunchentoot server started on port ")
  (princ *htoot-port*) (terpri))

;; Start a Swank server
(defun start-swank ()
  (setf *swank-server*
	(swank:create-server :style :spawn :port *swank-port*))
  (princ "Swank server started on port ")
  (princ *swank-port*) (terpri))

(sb-sys:enable-interrupt sb-unix:sigterm #'sigterm-handler)
(start-htoot)
(require :basics)
