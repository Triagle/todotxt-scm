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
(define (merge-alist l)
  (let loop [(acc '()) (l l)]
    (if (null-list? l)
        acc
        (let* [(h (car l))
               (k (car h))
               (v (cdr h))]
          (loop (cons (cons k (append (list v) (assoc-v k acc))) acc) (cdr l))))))
(define (assoc-or k l default)
  (if (and l (assoc k l))
      (assoc-v k l)
      default))
(define (assoc-or-f k l)
  (assoc-or k l #f))
