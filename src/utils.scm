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

(define (assoc-v k l #!key (default '()))
  ;; Returns the value of the item at key k in the alist l
  (let [(kv (assoc k l))]
    (if kv
        (cdr kv)
        ;; Rather than crash on a missing key, return an empty list by default
        default)))

(define (weed l)
  ;; Filter all falsey items from a list
  (filter identity l))
(define (sort-by . sorts)
  (lambda (a b)
    (find (cut (complement equal?) <> 'equal) (map (cut <> a b) sorts))))
(define (rand-char)
  ;; Return one random uppercase character
  (integer->char (+ (random 26) 65)))
(define (list-of n p)
  (let loop [(lst '())
         (c n)
         ]
    (if (zero? c)
        lst
        (loop (cons (p) lst) (- c 1)))))
(define (gen-uuid)
  ;; Generate a random UUID, (useful for permanent referral of tasks)
  ;; The random UUID thing isn't exactly optimal for most use cases, but is basically for our purposes
  ;; For instance, I am not doing any checks on the generating UUID
  ;; The UUID is only a string of 10 uppercase letters
  ;; However considering that the possibility of getting the same UUID twice is (1/26)^10 (7.084 * 10^-15), I frankly don't consider this to be an issue as of yet.
  ;; TODO: Do maybe fix this though at some point
  (list->string (list-of 10 rand-char)))
