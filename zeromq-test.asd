
(defsystem zeromq-test
  :name "zeromq-test"
  :author "Nicolas Martyanoff"
  :license "BSD"
  :description "Tests for the zeromq binding."
  :depends-on (:zeromq :fiveam)
  :components ((:module "test"
                        :components ((:file "packages")
                                     (:file "suites"
                                            :depends-on ("packages"))))))
