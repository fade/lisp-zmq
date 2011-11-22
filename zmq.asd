
(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(defsystem zmq
  :name "zmq"
  :version "1.1.0"
  :author "Nicolas Martyanoff"
  :license "BSD"
  :description "A binding of the zmq transport layer."
  :depends-on (:cffi)
  :in-order-to ((test-op (load-op zmq-test)))
  :components ((:module "src"
                :components ((:file "packages")
                             (cffi-grovel:grovel-file "grovel"
                                                      :depends-on ("packages"))
                             (:file "ffi" :depends-on ("grovel"))
                             (:file "zmq" :depends-on ("ffi"))))))

(defmethod perform ((o asdf:test-op) (c (eql (find-system :zmq))))
  (funcall (intern "RUN!" :5am)
           (intern "MAIN" :zmq-test)))
