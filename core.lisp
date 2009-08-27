;; core.lisp

;; dump core image for hunchentoot

(require :asdf)
(require :swank)
(require :sb-bsd-sockets)
(require :hunchentoot)
(require :cl-who)

(save-lisp-and-die "/var/lib/hunchentoot/sbcl.core")