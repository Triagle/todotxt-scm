(declare (unit uri))
(require-extension comparse uri-common)
(use comparse uri-common)
(define (as-decoded p)
  (bind p
        (o result uri-decode-string)))
(define uri-text
  (as-string (one-or-more (none-of* (in (->char-set "/")) item))))
(define link
  (sequence* [(_ (char-seq "todo://")) (l (one-or-more (as-decoded (char-seq-split "/")))) (l* (as-decoded (maybe (as-string (one-or-more item)))))]
             (result (if l*
                         (append l (list l*))
                         l))))
