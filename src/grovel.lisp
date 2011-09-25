
(include "zmq.h")
#+win32 (include "Winsock2.h")

(in-package :zeromq)

(ctype size-t "size_t")

#+win32 (ctype win32-socket "SOCKET")

(constantenum error-code
              ;; Stqndard error codes
              ((:einval "EINVAL"))
              ((:enodev "ENODEV"))
              ((:eintr "EINTR"))
              ((:efault "EFAULT"))
              ((:enomem "ENOMEM"))
              ((:eagain "EAGAIN"))
              ((:emfile "EMFILE"))
              ((:enotsup "ENOTSUP"))
              ((:eprotonosupport "EPROTONOSUPPORT"))
              ((:enobufs "ENOBUFS"))
              ((:enetdown "ENETDOWN"))
              ((:eaddrinuse "EADDRINUSE"))
              ((:eaddrnotavail "EADDRNOTAVAIL"))
              ((:econnrefused "ECONNREFUSED"))
              ((:einprogress "EINPROGRESS"))
              ((:enotsock "ENOTSOCK"))
              ;; ZMQ native error codes
              ((:efsm "EFSM"))
              ((:enocompatproto "ENOCOMPATPROTO"))
              ((:eterm "ETERM"))
              ((:emthread "EMTHREAD")))

(bitfield recv-options
          ((:dontwait "ZMQ_DONTWAIT")))

(bitfield send-options
          ((:dontwait "ZMQ_DONTWAIT"))
          ((:sndmore "ZMQ_SNDMORE"))
          ((:sndlabel "ZMQ_SNDLABEL")))

(constantenum socket-type
              ((:pair "ZMQ_PAIR"))
              ((:pub "ZMQ_PUB"))
              ((:sub "ZMQ_SUB"))
              ((:req "ZMQ_REQ"))
              ((:rep "ZMQ_REP"))
              ((:xreq "ZMQ_XREQ"))
              ((:xrep "ZMQ_XREP"))
              ((:pull "ZMQ_PULL"))
              ((:push "ZMQ_PUSH"))
              ((:xpub "ZMQ_XPUB"))
              ((:xsub "ZMQ_XSUB"))
              ((:router "ZMQ_ROUTER"))
              ((:dealer "ZMQ_DEALER")))

(constantenum socket-option
              ((:affinity "ZMQ_AFFINITY"))
              ((:identity "ZMQ_IDENTITY"))
              ((:subscribe "ZMQ_SUBSCRIBE"))
              ((:unsubscribe "ZMQ_UNSUBSCRIBE"))
              ((:rate "ZMQ_RATE"))
              ((:recovery_ivl "ZMQ_RECOVERY_IVL"))
              ((:sndbuf "ZMQ_SNDBUF"))
              ((:rcvbuf "ZMQ_RCVBUF"))
              ((:rcvmore "ZMQ_RCVMORE"))
              ((:fd "ZMQ_FD"))
              ((:events "ZMQ_EVENTS"))
              ((:type "ZMQ_TYPE"))
              ((:linger "ZMQ_LINGER"))
              ((:reconnect_ivl "ZMQ_RECONNECT_IVL"))
              ((:backlog "ZMQ_BACKLOG"))
              ((:reconnect_ivl_max "ZMQ_RECONNECT_IVL_MAX"))
              ((:maxmsgsize "ZMQ_MAXMSGSIZE"))
              ((:sndhwm "ZMQ_SNDHWM"))
              ((:rcvhwm "ZMQ_RCVHWM"))
              ((:multicast_hops "ZMQ_MULTICAST_HOPS"))
              ((:rcvtimeo "ZMQ_RCVTIMEO"))
              ((:sndtimeo "ZMQ_SNDTIMEO"))
              ((:rcvlabel "ZMQ_RCVLABEL")))
