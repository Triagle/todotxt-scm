(declare (unit todotxt))
(require-extension irregex)
(require-extension defstruct)
(use irregex defstruct utils)
(defstruct task
  ;; (A) 2011-03-02 Call Mum +family @phone
  ;; x Do this really important thing
  id completed completed-date date priority name projects contexts addons)
;; Todo.txt regexes
(define priority-rx  (irregex "\\([A-Z]\\)"))
(define date-rx  (irregex "\\d{4,4}-\\d{2,2}-\\d{2,2}"))
(define project-rx  (irregex "\\+([^\\s]+)"))
(define context-rx  (irregex "@([^\\s]+)"))
(define kv-pair-rx  (irregex "([^\\s]+):([^\\s]+)"))
(define (split-line line)
  (irregex-split (irregex '(+ whitespace)) line))
(define (new-task)
  (make-task
   projects: '()
   contexts: '()
   addons: '()))
(define (parse-task string)
  (let ((tokens (split-line string))
        (task (new-task)))
    (cond

     ((and (>= (length tokens) 3)
           (equal? (substring (car tokens) 0 1) "x")
           (irregex-match date-rx (cadr tokens))
           (irregex-match date-rx (caddr tokens)))
      (begin
        (set-task! task
                   completed: #t
                   completed-date: (cadr tokens)
                   date: (caddr tokens))
        (set! tokens (cdddr tokens))))
     ((and
       (>= (length tokens) 2)
       (equal? (substring (car tokens) 0 1) "x")
       (irregex-match date-rx (cadr tokens)))
      (begin
        (set-task! task
                   completed: #t
                   completed-date: (cadr tokens))))

     ((and
       (>= (length tokens) 1)
       (equal? (substring (car tokens) 0 1) "x"))
      (begin
        (set-task! task
                   completed: #t)
        (set! tokens (cdr tokens))))
     ((and (>= (length tokens) 2) (irregex-match priority-rx (car tokens)) (irregex-match date-rx (cadr tokens)))
      (begin
        (set-task! task
                   priority: (substring (car tokens) 1 2)
                   date: (cadr tokens))
        (set! tokens (cddr tokens))))
     ((and (>= (length tokens) 1) (irregex-match priority-rx (car tokens)))
      (begin
        (set-task! task
                   priority: (substring (car tokens) 1 2))
        (set! tokens (cdr tokens))))
   ((irregex-match date-rx (car tokens)) (begin
                                           (set-task! task date: (car tokens))
                                           (set! tokens (cdr tokens)))))
    (foldl (lambda (acc cur)
              (cond
               ((irregex-match project-rx cur) (update-task acc projects: (cons (substring cur 1) (task-projects acc))))
               ((irregex-match context-rx cur) (update-task acc contexts: (cons (substring cur 1) (task-contexts acc))))
               ((irregex-match kv-pair-rx cur) (let ((kv-pair (irregex-split (irregex ":") cur)))
                                                 (update-task acc addons: (cons (cons (car kv-pair) (cadr kv-pair)) (task-addons acc)))))
               (#t (update-task acc name: (if (task-name acc)
                                              (format #f "~a ~a" (task-name acc) cur)
                                              cur)))))
           task
           (if (and (task-completed task)
                    (irregex-match priority-rx (car tokens)))
               (cdr tokens)
               tokens))))
(define (task-priority<? a b)
  (cond
   ((and (task-priority a) (task-priority b)) (string<=? (task-priority a) (task-priority b)))
   ((not (task-priority a)) #f)
   ((not (task-priority b)) #t)))
(define (task->string task)
  (string-join (filter identity
                       (flatten (list
                                 (if (task-completed task)
                                     "x"
                                     #f)
                                 (if (task-priority task)
                                     (format "(~a)" (task-priority task))
                                     #f)
                                 (task-completed-date task)
                                 (task-date task)
                                 (task-name task)
                                 (map (lambda (x) (string-concatenate (list "+" x)))
                                      (sort (task-projects task) string<?))
                                 (map (lambda (x) (string-concatenate (list "@" x)))
                                      (sort (task-contexts task) string<?))
                                 (map (lambda (x) (format #f "~a:~a" (car x) (cdr x))) (task-addons task))))) " "))
(define (parse-filename file)
  (let loop ((lines (string-split (read-all file) "\n")) (acc '()) (id 1))
    (if (null? lines)
        acc
        (loop (cdr lines) (cons (update-task (parse-task (car lines))
                                             id: id) acc) (+ id 1)))))
(define (format-tasks-to-alists tasks)
  (map task->alist tasks))
