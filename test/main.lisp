
(in-package :zeromq-test)

(in-suite main)

(defun byte-array (contents)
  (make-array (length contents)
              :element-type '(unsigned-byte 8)
              :initial-contents contents))

(test empty-messages
  (zmq:with-msg-init (msg)
    (is (zerop (zmq:msg-size msg)))
    (is (string= (zmq:msg-data-string msg) ""))
    (is (equalp (zmq:msg-data-array msg) (byte-array #())))))

(test uninitialized-messages
  (zmq:with-msg-init-size (msg 16)
    (is (= (zmq:msg-size msg) 16))))

(test string-messages
  (zmq:with-msg-init-data (msg "test")
    (is (= (zmq:msg-size msg) 4))
    (is (string= (zmq:msg-data-string msg) "test"))
    (is (equalp (zmq:msg-data-array msg) (byte-array #(116 101 115 116))))))

(test empty-string-messages
  (zmq:with-msg-init-data (msg "")
    (is (zerop (zmq:msg-size msg)))
    (is (string= (zmq:msg-data-string msg) ""))
    (is (equalp (zmq:msg-data-array msg) (byte-array #())))))

;;; (test unicode-messages
;;;   (zmq:with-msg-init-data (msg "été")
;;;     (is (= (zmq:msg-size msg) 5))
;;;     (is (string= (zmq:msg-data-string msg) "été"))
;;;     (is (equal (zmq:msg-data-array msg) #(195 137 116 195 137)))))

(test binary-messages
  (zmq:with-msg-init-data (msg (byte-array #(97 98 99)))
    (is (= (zmq:msg-size msg) 3))
    (is (string= (zmq:msg-data-string msg) "abc"))
    (is (equalp (zmq:msg-data-array msg) (byte-array #(97 98 99))))))

(test empty-binary-messages
  (zmq:with-msg-init-data (msg (byte-array #()))
    (is (zerop (zmq:msg-size msg)))
    (is (string= (zmq:msg-data-string msg) ""))
    (is (equalp (zmq:msg-data-array msg) (byte-array #())))))

(test copied-messages
  (zmq:with-msg-init-data (msg "test")
    (zmq:with-msg-init (msg-copy)
      (zmq:msg-copy msg-copy msg)
      (is (= (zmq:msg-size msg-copy) 4))
      (is (string= (zmq:msg-data-string msg-copy) "test"))
      (is (equalp (zmq:msg-data-array msg-copy)
                  (byte-array #(116 101 115 116)))))))

(test copied-empty-messages
  (zmq:with-msg-init-data (msg "")
    (zmq:with-msg-init (msg-copy)
      (zmq:msg-copy msg-copy msg)
      (is (zerop (zmq:msg-size msg-copy)))
      (is (string= (zmq:msg-data-string msg-copy) ""))
      (is (equalp (zmq:msg-data-array msg-copy) (byte-array #()))))))

(test int-socket-options
  (zmq:with-context (context 0)
    (zmq:with-socket (socket context :sub)
      (zmq:setsockopt socket :backlog 42)
      (is (= (zmq:getsockopt socket :backlog) 42)))))

(test int64-socket-options
  (zmq:with-context (context 0)
    (zmq:with-socket (socket context :sub)
      (zmq:setsockopt socket :swap 17179869184)
      (is (= (zmq:getsockopt socket :swap) 17179869184)))))

(test uint64-socket-options
  (zmq:with-context (context 0)
    (zmq:with-socket (socket context :sub)
      (zmq:setsockopt socket :hwm 17179869184)
      (is (= (zmq:getsockopt socket :hwm) 17179869184)))))

(test string-socket-options
  (zmq:with-context (context 0)
    (zmq:with-socket (socket context :sub)
      (zmq:setsockopt socket :identity "test")
      (is (string= (zmq:getsockopt socket :identity) "test")))))
