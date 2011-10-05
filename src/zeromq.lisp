
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
               (t (error condition :code (or keyword error-code)
                                   :description description))))
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
  "Evaluate BODY in an environment where VAR is binded to a context created
with IO-THREADS threads."
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
  "Evaluate BODY in an environment where VAR is binded to a socket created in
context CONTEXT with type TYPE."
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

(defun bind (socket endpoint)
  "Bind SOCKET to the address ENDPOINT."
  (with-foreign-string (%endpoint endpoint)
    (call-ffi -1 '%bind socket %endpoint)))

(defun connect (socket endpoint)
  "Connect SOCKET to the address ENDPOINT."
  (with-foreign-string (%endpoint endpoint)
    (call-ffi -1 '%connect socket %endpoint)))

(defvar *socket-options-type* (make-hash-table)
  "A table to store the foreign type of each socket option.")

(defun define-sockopt-type (option type &optional (length (foreign-type-size type)))
  (setf (gethash option *socket-options-type*) (list type length)))

(define-sockopt-type :hwm :uint64)
(define-sockopt-type :swap :int64)
(define-sockopt-type :affinity :uint64)
(define-sockopt-type :identity :char 255)
(define-sockopt-type :subscribe :char)
(define-sockopt-type :unsubscribe :char)
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
  (when (member option '(:subscribe :unsubscribe))
    (error "Socket option ~A is write only." option))
  (let ((info (gethash option *socket-options-type*)))
    (unless info
      (error "Unknown socket option ~A." option))
    (destructuring-bind (type length) info
      (with-foreign-objects ((%value type length) (%size 'size-t))
        (call-ffi -1 '%getsockopt socket option %value %size)
        (case option
          (:identity
           (when (> (mem-ref %size 'size-t) 0)
             (foreign-string-to-lisp %value)))
          (:events
           (foreign-bitfield-symbols 'event-types (mem-ref %value type)))
          (t
           (mem-ref %value type)))))))

(defun setsockopt (socket option value)
  "Set the value associated to a socket option."
  (let ((info (gethash option *socket-options-type*)))
    (unless info
      (error "Unknown socket option: ~A." option))
    (destructuring-bind (type length) info
      (case option
        ((:subscribe :unsubscribe :identity)
         (let ((length (length value)))
           (with-foreign-object (%value :char (+ length 1))
             (lisp-string-to-foreign value %value (+ length 1))
             (call-ffi -1 '%setsockopt socket option %value length))))
        (t
         (with-foreign-object (%value type length)
           (setf (mem-ref %value type) (case option
                                         (:events (foreign-bitfield-value
                                                   'event-types value))
                                         (t value)))
           (call-ffi -1 '%setsockopt socket option %value length)))))))

(defun device (type frontend backend)
  "Connect a frontend socket to a backend socket. This function always returns
-1."
  (call-ffi 0 '%device (foreign-enum-value 'device-type type) frontend backend))

(defun msg-init-fill (message data)
  "Initialize fill and return a message. If DATA is a string, convert it to a
byte array."
  (etypecase data
    (string
     (with-foreign-string ((%string length) data)
       (call-ffi -1 '%msg-init-size message length)
       (%memcpy (%msg-data message) %string length)))
    ((simple-array (unsigned-byte 8))
     (with-pointer-to-vector-data (ptr data)
       (let ((length (length data)))
         (call-ffi -1 '%msg-init-size message length)
         (%memcpy (%msg-data message) ptr length))))))

(defun msg-init ()
  "Create and return a new empty message."
  (let ((%message (foreign-alloc 'msg)))
    (handler-case
        (progn
          (call-ffi -1 '%msg-init %message)
          %message)
      (error (cond)
        (foreign-free %message)
        (error cond)))))

(defun msg-init-size (size)
  "Create and return a new message initialized to a fixed size SIZE."
  (let ((%message (foreign-alloc 'msg)))
    (handler-case
        (progn
          (call-ffi -1 '%msg-init-size %message size)
          %message)
      (error (cond)
        (foreign-free %message)
        (error cond)))))

(defun msg-init-data (data)
  "Create and return a new message initialized and filled with DATA."
  (let ((%message (foreign-alloc 'msg)))
    (handler-case
        (progn
          (msg-init-fill %message data)
          %message)
      (error (cond)
        (foreign-free %message)
        (error cond)))))

(defun msg-close (message)
  "Release a message, freeing any memory allocated for the message."
  (unwind-protect
       (call-ffi -1 '%msg-close message)
    (foreign-free message)))

(defmacro with-msg-init ((var) &body body)
  "Evaluate BODY in an environment where VAR is binded to a new empty
  message."
  `(with-foreign-object (,var 'msg)
     (call-ffi -1 '%msg-init ,var)
     (unwind-protect
          (progn ,@body)
       (ignore-errors (call-ffi -1 '%msg-close ,var)))))

(defmacro with-msg-init-size ((var size) &body body)
  "Evaluate BODY in an environment where VAR is binded to a new message of
size SIZE."
  `(with-foreign-object (,var 'msg)
     (call-ffi -1 '%msg-init-size ,var ,size)
     (unwind-protect
          (progn ,@body)
       (ignore-errors (call-ffi -1 '%msg-close ,var)))))

(defmacro with-msg-init-data ((var data) &body body)
  "Evaluate BODY in an environment where VAR is binded to a new message filled
with DATA."
  `(with-foreign-object (,var 'msg)
     (msg-init-fill ,var ,data)
     (unwind-protect
          (progn ,@body)
       (ignore-errors (call-ffi -1 '%msg-close ,var)))))

(defun msg-size (message)
  "Return the size in byte of the content of MESSAGE."
  (%msg-size message))

(defun msg-data (message)
  "Get a foreign pointer on the content of MESSAGE."
  (%msg-data message))

(defun msg-data-array (message)
  "Get the content of MESSAGE as an unsigned byte array."
  (let ((data (%msg-data message)))
    (unless (null-pointer-p data)
      (let* ((length (- (msg-size message) 1))
             (array (make-array length :element-type '(unsigned-byte 8))))
        (with-pointer-to-vector-data (%array array)
          (%memcpy %array data length))
        array))))

(defun msg-data-string (message &key (encoding *default-foreign-encoding*))
  "Get the content of MESSAGE as a character string. The string is decoded
using the character coding schema ENCODING."
  (let ((data (%msg-data message)))
    (unless (null-pointer-p data)
      (foreign-string-to-lisp (%msg-data message)
                              :count (- (%msg-size message) 1)
                              :encoding encoding))))

(defun msg-copy (destination source)
  "Copy the content of the message SOURCE to the message DESTINATION."
  (call-ffi -1 '%msg-copy destination source))

(defun send (socket message &optional flags)
  "Queue MESSAGE to be on SOCKET."
  (call-ffi -1 '%send socket message
            (foreign-bitfield-value 'send-options flags)))

(defun recv (socket message &optional flags)
  "Receive a message from SOCKET and store it in MESSAGE."
  (call-ffi -1 '%recv socket message
            (foreign-bitfield-value 'recv-options flags)))

(defun stopwatch-start ()
  "Start a timer, and return a handle."
  (call-ffi (null-pointer) '%stopwatch-start))

(defun stopwatch-stop (handle)
  "Stop the timer referenced by HANDLE, and return the number of microseconds
  elapsed since the timer was started."
  (%stopwatch-stop handle))

(defmacro with-stopwatch (&body body)
  "Start a timer, evaluate BODY, stop the timer, and return the elapsed time."
  (let ((handle (gensym)))
    `(let ((,handle (stopwatch-start)))
       ,@body
       (stopwatch-stop ,handle))))
