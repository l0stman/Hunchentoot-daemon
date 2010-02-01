;; startup.lisp

;; ports that will be listened on localhost:
;;
;;   *htoot-port* - Hunchentoot (via proxy)
;;   *swank-port* - Swank (via ssh+Emacs)

(defparameter *htoot-port* 8080)
(defparameter *swank-port* 4005)
(defparameter *htoot-server* nil)
(defparameter *swank-server* nil)

(setq hunchentoot:*message-log-pathname* (pathname (posix-getenv "HT_LOG"))
      hunchentoot:*dispatch-table* (list 'hunchentoot:dispatch-easy-handlers))

(defun sigterm-handler (sig code scp)
  (declare (ignore sig code scp))
  ;; Shut down Swank and anyone else by terminating all threads
  (dolist (thread (sb-thread:list-all-threads))
    (unless (equal sb-thread:*current-thread* thread)
      (sb-thread:terminate-thread thread)))
  (sleep 1)
  (sb-ext:quit))

(defun start-htoot ()
  "Start a Hunchentoot server listening for connections."
  (setf *htoot-server*
	(hunchentoot:start
         (make-instance 'hunchentoot:acceptor :port *htoot-port*)))
  (format t "Hunchentoot server started on port ~S~%" *htoot-port*))

(defun start-swank ()
  "Start a Swank server for SLIME."
  (setf *swank-server*
	(swank:create-server :style :spawn
                             :port *swank-port*
                             :coding-system "utf-8-unix"))
  (format t "Swank server started on port ~S~%" *swank-port*))

(sb-sys:enable-interrupt sb-unix:sigterm #'sigterm-handler)
(start-htoot)
