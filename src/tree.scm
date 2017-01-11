(declare (unit tree))
(require-extension defstruct fmt)
(use defstruct fmt)
(defstruct node
  key value children)
(define (new-node key value)
  (make-node
   key: key
   children: '()
   value: value))
(define (empty-node key)
  (new-node key #f))
(define (node-by-key tree key)
  (find (lambda (node)
          (equal? (node-key node) key)) tree))
;; Tree is a list of nodes
(define (tree-add tree path value)
  (let loop [(path path)
             (branch tree)]
    (let* [(key (car path))
           (descent-node (or (node-by-key branch key) (empty-node key)))]
      (cond
       ;; If the path is finished, return the tree with node updated
       [(= (length path) 1)
        (cons (update-node descent-node
                           value: (cons value (or (node-value descent-node) '()))) (remove (lambda (n)
                                                                                             (equal? (node-key n) key)) branch))]
       ;; If the path is not empty, descend through the tree recursively
       [#t
        ;; Replace the node with key key's children with the recursive value of tree-add
        (cons (update-node descent-node
                           children: (loop (cdr path) (node-children descent-node))) (remove (lambda (n)
                                                                                               (equal? (node-key n) key)) branch))]))))
(define (walk-tree tree fn)
  (let loop [(ident 0)(branch tree)]
    (when (not (null-list? branch))
      (let [(node (car branch))]
        (fn ident node)
        (loop (+ ident 1) (node-children node))
        (loop ident (cdr branch))))))
(define (make-string-with len elt)
  (list->string (make-list len elt)))
(define (print-branch accessor ident node)
  (fmt #t
       (if (= ident 0)
           nl
           "")
       (make-string-with ident #\space)
       (if (= ident 0)
           "─"
           "└"
           )
       "┐ "
       (node-key node)
       nl
       (if (list? (node-value node))
           (fmt-join (cut cat (make-string-with (+ ident 1) #\space) "├ " <> nl) (map accessor (node-value node)))
           "")))
