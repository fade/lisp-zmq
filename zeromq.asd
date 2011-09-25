
(defsystem zeromq
  :name "zeromq"
  :version "1.0.0"
  :author "Nicolas Martyanoff"
  :license "BSD"
  :description "A binding of the zeromq transport layer."
  :depends-on (:cffi)
  :in-order-to ((test-op (load-op zeromq-test)))
  :components ((:module "src" :components ((:file "packages")
                                           (:file "zeromq" :depends-on ("packages"))))))

(defmethod perform ((o asdf:test-op) (c (eql (find-system :zeromq))))
  (funcall (intern "RUN!" :5am)
           (intern "MAIN" :zeromq-test)))
