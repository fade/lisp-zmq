
(defpackage :zmq
  (:use :cl :cffi)
  (:export :version

           :init :term :with-context

           :socket :close :with-socket :with-sockets
           :bind :connect

           :getsockopt :setsockopt

           :device

           :msg-init :msg-init-size :msg-init-data :msg-close
           :with-msg-init :with-msg-init-size :with-msg-init-data
           :msg-size :msg-data :msg-data-string :msg-data-array
           :msg-copy :msg-move

           :send :recv
           :with-poll-items
           :poll-items-aref :do-poll-items :poll-item-event-signaled-p
           :poll

           :stopwatch-start :stopwatch-stop :with-stopwatch
           :sleep

           :zmq-error
           :einval-error :enodev-error :eintr-error :efault-error :enomem-error
           :eagain-error :emfile-error :enotsup-error :eprotonosupport-error
           :enobufs-error :enetdown-error :eaddrinuse-error :eaddrnotavail-error
           :econnrefused-error :einprogress-error :enotsock-error
           :efsm-error :enocompatproto-error :eterm-error :emthread-error)
  (:shadow :close :sleep))
