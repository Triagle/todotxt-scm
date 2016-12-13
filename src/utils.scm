(declare (unit todotxt-utils))

(define (write-to-a-file path txt)
  ;; Append text txt to file at path
  (call-with-output-file path
    (lambda (output-port)
      (format output-port "~a~%" txt))
    #:append))

(define (overwrite-file path txt)
  ;; Overwrite the file at path with text txt
  (call-with-output-file path
    (lambda (output-port)
      (format output-port txt))))

(define (rm-prop k l)
  ;; Remove the entry associated with k in alist l
  (remove (lambda (kv) (equal? (car kv) k)) l))

(define (assoc-v k l)
  ;; Returns the value of the item at key k in the alist l
  (let [(kv (assoc k l))]
    (if kv
        (cdr kv)
        ;; Rather than crash on a missing key, return an empty list
        '())))

(define (weed l)
  ;; Filter all falsey items from a list
  (filter identity l))
