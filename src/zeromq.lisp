
(in-package :zeromq)

(defvar *errors* (make-hash-table)
  "A table mapping error nombers to their condition class")

(define-condition zmq-error (error)
  ((code
    :initarg :code
    :reader zmq-error-code
    :documentation "The numeric error code.")
   (description
    :initarg :description
    :reader zmq-error-description
    :documentation "The description of the error."))
  (:report (lambda (condition stream)
             (with-slots (code description) condition
               (format stream "ZMQ error ~A: ~A." code description))))
  (:documentation "A ZMQ error."))

(defmacro define-error (name error-value)
  `(progn
     (define-condition ,name (zmq-error)
       ()
       (:report (lambda (condition stream)
                  (with-slots (description) condition
                    (format stream "ZMQ error: ~A." description))))
       (:documentation ,(concatenate 'string
                                     "The error associated to the "
                                     (symbol-name error-value)
                                     " error code.")))
     (setf (gethash ,error-value *errors*) ',name)))

(define-error einval-error :einval)
(define-error enodev-error :enodev)
(define-error eintr-error :eintr)
(define-error efault-error :efault)
(define-error enomem-error :enomem)
(define-error eagain-error :eagain)
(define-error emfile-error :emfile)
(define-error enotsup-error :enotsup)
(define-error eprotonosupport-error :eprotonosupport)
(define-error enobufs-error :enobufs)
(define-error enetdown-error :enetdown)
(define-error eaddrinuse-error :eaddrinuse)
(define-error eaddrnotavail-error :eaddrnotavail)
(define-error econnrefused-error :econnrefused)
(define-error einprogress-error :einprogress)
(define-error enotsock-error :enotsock)
(define-error efsm-error :efsm)
(define-error enocompatproto-error :enocompatproto)
(define-error eterm-error :eterm)
(define-error emthread-error :emthread)

(defun call-ffi (invalid-value function &rest args)
  "Call a low-level function and check its return value. If the return value
is equal to INVALID-VALUE, a suitable error is signaled. When the error code
tells that the function was interrupted by a signal (EINTR), the function is
called until it succeeds. In any case, the return value of the low-level
function is returned."
  (tagbody retry
     (let ((value (apply function args)))
       (if (eq value invalid-value)
           (let* ((error-code (%errno))
                  (description (%strerror error-code))
                  (keyword (foreign-enum-keyword 'error-code error-code :errorp nil))
                  (condition (gethash keyword *errors* 'zmq-error)))
             (case keyword
               (:eintr (go retry))
               (t (error condition :code (or keyword error-code) :description description))))
           (return-from call-ffi value)))))

(defun version ()
  "Return the version of the ZMQ library, a list of three integers (major,
  minor and patch version)."
  (with-foreign-objects ((%major :int) (%minor :int) (%patch :int))
    (%version %major %minor %patch)
    (list (mem-ref %major :int) (mem-ref %minor :int) (mem-ref %patch :int))))

(defun init (io-threads)
  "Create and return a new context."
  (call-ffi (null-pointer) '%init io-threads))

(defun term (context)
  "Terminate and release a context"
  (call-ffi -1 '%term context))

(defmacro with-context ((var io-threads) &body body)
  `(let ((,var (init ,io-threads)))
     (unwind-protect
          (progn ,@body)
       (term ,var))))

(defun socket (context type)
  "Create and return a new socket."
  (call-ffi (null-pointer)
            '%socket context (foreign-enum-value 'socket-type type)))

(defun close (socket)
  "Close and release a socket."
  (call-ffi -1 '%close socket))

(defmacro with-socket ((var context type) &body body)
  `(let ((,var (socket ,context ,type)))
     (unwind-protect
          (progn ,@body)
       (close ,var))))

(defmacro with-sockets (bindings &body body)
  (if bindings
      `(with-socket ,(car bindings)
         (with-sockets ,(cdr bindings)
           ,@body))
      `(progn ,@body)))

(defvar *socket-options-type* (make-hash-table)
  "A table to store the foreign type of each socket option.")

(defun define-sockopt-type (option type &optional (length (foreign-type-size type)))
  (setf (gethash option *socket-options-type*) (list type length)))

(define-sockopt-type :hwm :uint64)
(define-sockopt-type :swap :int64)
(define-sockopt-type :affinity :uint64)
(define-sockopt-type :identity :char 255)
(define-sockopt-type :rate :int64)
(define-sockopt-type :recovery-ivl :int64)
(define-sockopt-type :recovery-ivl-msec :int64)
(define-sockopt-type :mcast-loop :int64)
(define-sockopt-type :sndbuf :uint64)
(define-sockopt-type :rcvbuf :uint64)
(define-sockopt-type :rcvmore :int64)
(define-sockopt-type :fd #+win32 win32-socket
                         #-win32 :int)
(define-sockopt-type :events :uint32)
(define-sockopt-type :type :int)
(define-sockopt-type :linger :int)
(define-sockopt-type :reconnect-ivl :int)
(define-sockopt-type :backlog :int)
(define-sockopt-type :reconnect-ivl-max :int)

(defun getsockopt (socket option)
  "Get the value currently associated to a socket option."
  (destructuring-bind (type length)
      (gethash option *socket-options-type*)
    (with-foreign-objects ((%value type length) (%size 'size-t))
      (setf (mem-ref %size 'size-t) length)
      (call-ffi -1 '%getsockopt socket option %value %size)
      (case option
        (:identity
         (when (> (mem-ref %size 'size-t) 0)
           (foreign-string-to-lisp %value)))
        (:events
         (foreign-bitfield-symbols 'event-types (mem-ref %value type)))
        (t
         (mem-ref %value type))))))
