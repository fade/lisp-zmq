
(in-package :zeromq)

(define-foreign-library libzmq
  (t (:default "libzmq")))

(use-foreign-library libzmq)

(defcfun (%memcpy "memcpy") :pointer
  (dst  :pointer)
  (src  :pointer)
  (len  :long))

(defctype socket :pointer)
(defctype context :pointer)
(defctype msg :pointer)

(defcstruct pollitem
  (socket socket)
  (fd #+win32 win32-socket
      #-win32 :int)
  (events :short)
  (revents :short))

(defcstruct msg
  (content :pointer)
  (flags :uchar)
  (vsm-size :uchar)
  (vsm-data :uchar :count #.max-vsm-size))

(defcfun (%bind "zmq_bind") :int
  (socket socket)
  (endpoint :string))

(defcfun (%close "zmq_close") :int
  (socket socket))

(defcfun (%connect "zmq_connect") :int
  (socket socket)
  (endpoint :string))

(defcfun (%errno "zmq_errno") :int)

(defcfun (%getsockopt "zmq_getsockopt") :int
  (socket socket)
  (option-name socket-option)
  (option-value :pointer)
  (option-len (:pointer size-t)))

(defcfun (%init "zmq_init") context
  (io-threads :int))

(defcfun (%msg-close "zmq_msg_close") :int
  (msg msg))

(defcfun (%msg-copy "zmq_msg_copy") :int
  (dest msg)
  (src msg))

(defcfun (%msg-data "zmq_msg_data") :pointer
  (msg msg))

(defcfun (%msg-init-data "zmq_msg_init_data") :int
  (msg msg)
  (data :pointer)
  (size size-t)
  (ffn :pointer)
  (hint :pointer))

(defcfun (%msg-init-size "zmq_msg_init_size") :int
  (msg msg)
  (size size-t))

(defcfun (%msg-init "zmq_msg_init") :int
  (msg msg))

(defcfun (%msg-move "zmq_msg_move") :int
  (dest msg)
  (src msg))

(defcfun (%msg-size "zmq_msg_size") size-t
  (msg msg))

(defcfun (%poll "zmq_poll") :int
  (items (:pointer pollitem))
  (nitems :int)
  (timeout :long))

(defcfun (%recv "zmq_recv") :int
  (socket socket)
  (msg msg)
  (flags recv-options))

(defcfun (%send "zmq_send") :int
  (socket socket)
  (msg msg)
  (flags send-options))

(defcfun (%setsockopt "zmq_setsockopt") :int
  (socket socket)
  (option-name socket-option)
  (option-value :pointer)
  (option-len size-t))

(defcfun (%socket "zmq_socket") socket
  (context context)
  (type socket-type))

(defcfun (%strerror "zmq_strerror") :string
  (errnum :int))

(defcfun (%term "zmq_term") :int
  (context context))

(defcfun (%version "zmq_version") :void
  (major (:pointer :int))
  (minor (:pointer :int))
  (patch (:pointer :int)))

(defcfun (%device "zmq_device") :int
  (device device-type)
  (frontend socket)
  (backend socket))
