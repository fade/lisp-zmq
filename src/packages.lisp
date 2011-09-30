
(defpackage :zeromq
  (:nicknames :zmq)
  (:use :cl :cffi)
  (:export :version

           :init :term :with-context

           :socket :close :with-socket :with-sockets
           :bind

           :getsockopt :setsockopt

           :device

           :msg-init :msg-close :with-msg
           :msg-size
           :msg-copy

           :send :recv

           :zmq-error
           :einval-error :enodev-error :eintr-error :efault-error :enomem-error
           :eagain-error :emfile-error :enotsup-error :eprotonosupport-error
           :enobufs-error :enetdown-error :eaddrinuse-error :eaddrnotavail-error
           :econnrefused-error :einprogress-error :enotsock-error
           :efsm-error :enocompatproto-error :eterm-error :emthread-error)
  (:shadow :close))
