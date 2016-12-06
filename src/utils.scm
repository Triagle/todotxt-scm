(declare (unit todotxt-utils))
(define (write-to-a-file path txt)
  (call-with-output-file path
    (lambda (output-port)
      (format output-port "~a~%" txt))
    #:append))
(define (overwrite-file path txt)
  (call-with-output-file path
    (lambda (output-port)
      (format output-port txt))))
(define (rm-prop k l)
  (remove (lambda (kv) (equal? (car kv) k)) l))
(define (assoc-v k l)
  (let [(kv (assoc k l))]
    (if kv
        (cdr kv)
        '())))
(define (weed l)
  (filter identity l))
