
(defsystem zeromq-examples
  :name "zeromq-examples"
  :author "Nicolas Martyanoff"
  :license "BSD"
  :description "Examples for the zeromq binding."
  :depends-on (:zeromq)
  :components ((:module "examples"
                :components ((:file "packages")
                             (:file "official-benchmarks"
                              :depends-on ("packages"))))))
