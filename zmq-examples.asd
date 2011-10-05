
(defsystem zmq-examples
  :name "zmq-examples"
  :author "Nicolas Martyanoff"
  :license "BSD"
  :description "Examples for the zmq binding."
  :depends-on (:zmq :bordeaux-threads)
  :components ((:module "examples"
                :components ((:file "packages")
                             (:file "official-benchmarks"
                              :depends-on ("packages"))))))
