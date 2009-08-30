;; startup.lisp

;; ports that will be listened on (localhost):
;;
;;   *htoot-port* - Hunchentoot (via mod_proxy)
;;   *swank-port* - Swank (via ssh+Emacs)
;;   *magic-port* - listener

(defparameter *htoot-port* 8080)
(defparameter *swank-port* 4005)
(defparameter *magic-port* 6441)

(setf asdf:*central-registry*
	       (list* #p "/usr/local/lib/common-lisp/system-registry/"
		      #p "/usr/local/lib/sbcl/site-systems/"
		      asdf:*central-registry*))

(defun sigterm-handler (sig code scp)
  (declare (ignore sig code scp))
  ;; Shut down Swank and anyone else by terminating all threads
  (dolist (thread (sb-thread:list-all-threads))
    (unless (equal sb-thread:*current-thread* thread)
      (sb-thread:terminate-thread thread)))
  (sleep 1)
  (sb-ext:quit))

(defun start-server ()
  (let (hunchentoot-server swank-server)

    ;; Start a Hunchentoot server listening for connections
    (setf hunchentoot-server
	  (hunchentoot:start (make-instance 'hunchentoot:acceptor :port *htoot-port*)))
    (princ "Hunchentoot server started on port ")
    (princ *htoot-port*) (terpri)

    ;; Start a Swank server
    (setf swank-server
	  (swank:create-server :port *swank-port* :style :spawn :dont-close t))
    (princ "Swank server started on port ")
    (princ *swank-port*) (terpri)

    (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
				 :type :stream :protocol :tcp)))
      ;; Listen on local port *magic-port* for a TCP connection
      (sb-bsd-sockets:socket-bind socket #(127 0 0 1) *magic-port*)
      (sb-bsd-sockets:socket-listen socket 1)
      (princ "Listener started on port ")
      (princ *magic-port*) (terpri)

      ;; When it comes, close the client socket and continue
      (loop do
	   (progn
	     (multiple-value-bind (client-socket addr port)
		 (sb-bsd-sockets:socket-accept socket)
	       (sb-bsd-sockets:socket-close client-socket)))))))

(sb-sys:enable-interrupt sb-unix:sigterm #'sigterm-handler)
(start-server)

;; startup.lisp ends here
