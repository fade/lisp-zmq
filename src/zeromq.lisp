
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

(defun check-return-value (value invalid-value)
  "Check the return value VALUE. If it's equal to INVALID-VALUE, ie. if it
indicates an error, signal a suitable error. If not, return VALUE."
  (if (eq value invalid-value)
      (let* ((error-code (%errno))
             (text (%strerror error-code))
             (keyword (foreign-enum-keyword 'error-code error-code :errorp nil))
             (condition (gethash keyword *errors* 'zmq-error)))
        (error condition :code (or keyword error-code) :text text))
      value))

(defun version ()
  "Return the version of the ZMQ library, a list of three integers (major,
  minor and patch version)."
  (with-foreign-objects ((major :int) (minor :int) (patch :int))
    (%version major minor patch)
    (list (mem-ref major :int) (mem-ref minor :int) (mem-ref patch :int))))

(defun init (io-threads)
  "Create and return a new context."
  (check-return-value (%init io-threads) (null-pointer)))

(defun term (context)
  "Terminate and release a context"
  (check-return-value (%term context) -1))

(defmacro with-context (var io-threads &body body)
  `(let ((,var (init ,io-threads)))
     (unwind-protect
          (progn ,@body)
       (term ,var))))
