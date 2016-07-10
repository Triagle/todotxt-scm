;; jselect library
;; operates on a high level with object like structures of alists
(declare (unit jselect))
(require-extension list-utils)
(define (_jselect attributes conditionals)
  (lambda (from)
    (map (if (equal? attributes '*)
             identity
             (lambda (i) (map (lambda (n) (assoc n i)) attributes))) ((foldr compose identity conditionals) from))))

(define (jselect attributes from . conditionals)
  ((_jselect attributes conditionals) from))
(define (where attribute predicate value)
  (lambda (from)
    (let ((func (cond
                 ((equal? predicate 'isnt) (lambda (a) (not (equal? (cdr (assoc attribute a)) value))))
                 ((equal? predicate 'is) (lambda (a) (equal? (cdr (assoc attribute a)) value)))
                 (#t (lambda (a) (predicate (cdr (assoc attribute a)) value))))))
      (filter func from))))
(define (order-by attribute . condition)
  (lambda (from)
    (sort from
          (if (length=0? condition)
              (lambda (item1 item2) (> (cdr (assoc attribute item1 )) (cdr (assoc attribute item2))))
              (lambda (item1 item2) ((car condition) (cdr (assoc attribute item1 )) (cdr (assoc attribute item2))))))))
