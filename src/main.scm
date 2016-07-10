;; Main CLI interface
(declare (uses jselect todotxt))
(require-extension fmt-color)
(require-extension irregex)
(require-extension fmt)
(require-extension fmt-unicode)
(use fmt fmt-color fmt-unicode irregex utils)
(define (join-structs structs accessing-function joiner)
  (string-join (filter identity (map accessing-function structs)) joiner))
(define (write-to-a-file path txt)
  (call-with-output-file path
    (lambda (output-port)
      (format output-port "~a~%" txt))
    #:append))
(define (overwrite-file path txt)
  (call-with-output-file path
    (lambda (output-port)
      (format output-port txt))))
(define (colour-priority task)
  (let ((priority (task-priority task)))
    (cond
     ((equal? priority "A") (fmt #f (fmt-bold priority)))
     (#t priority))))
(define (run args)
  (let ((action (or (= (length args) 0) (car args)))
        (todo-file (get-environment-variable "TODO_FILE")))
    (cond
     ((or (equal? action "list") (equal? action "listall"))
      (let ((tasks (sort (filter (lambda (x)
                                   (and (or (equal? action "listall") (not (task-completed x)))
                                        (if (= (length (cdr args)) 0)
                                            #t
                                            (irregex-match  (irregex (string-concatenate (list ".*" (irregex-quote (string-join (cdr args) " ")) ".*"))) (task->string x)))))
                                 (parse-filename todo-file)) task-priority<?)))
        (fmt #t (fmt-unicode
                 (tabular
                  "| " (dsp (string-concatenate (list "ID\n" (join-structs tasks (lambda (x) (number->string (task-id x))) "\n")))) " | "
                  (dsp (string-concatenate (list "Priority\n" (join-structs tasks colour-priority "\n")))) " | "
                  (dsp (string-concatenate (list "Task\n" (join-structs tasks task-name "\n")))) " | "
                  (dsp (string-concatenate (list "Projects\n" (join-structs tasks (lambda (x) (string-join (task-projects x) ", ")) "\n")))) " | "
                  (dsp (string-concatenate (list "Contexts\n" (join-structs tasks (lambda (x) (string-join (task-contexts x) ", ")) "\n")))) " |")))))
     ((equal? action "add")
      (if (= (length args) 2)
          (write-to-a-file todo-file (string-join (cdr args) " "))
          (print "Usage: todo add [task-name]")))
     ((equal? action "replace")
      (if (> (length args) 2)
          (let ((new-task (string-join (cddr args) " ")))
            (begin
              (overwrite-file todo-file (string-append (string-join (map task->string
                                                                         (map (lambda (task)
                                                                                (if (= (task-id task) (string->number (cadr args)))
                                                                                    (update-task task
                                                                                                 name: new-task)
                                                                                    task)) (parse-filename todo-file))) "\n") "\n"))))
          (print "Usage: todo replace [task-id] [new-task]")))
     ((equal? action "rm")
      (if (= (length args) 2)
          (begin
            (overwrite-file todo-file (string-append (string-join (map task->string
                                                                                                             (remove (lambda (task)
                                                                                                                       (= (task-id task) (string->number (cadr args)))) (parse-filename todo-file))) "\n") "\n")))
          (print "Usage: todo rm [task-id]")))
     ((equal? action "done")
      (if (= (length args) 2)
          (begin
            (overwrite-file todo-file (string-append (string-join (map (lambda (task)
                                                                                                               (if (= (task-id task) (string->number (cadr args)))
                                                                                                                   (task->string (update-task task
                                                                                                                                              completed: #t))
                                                                                                                   (task->string task)))
                                                                                                             (parse-filename todo-file)) "\n") "\n")))
          (print "Usage: todo done [task-id]")))
     ((equal? action "pri")
      (if (and (= (length args) 3) (or (equal? (caddr args) "-") (irregex-match priority-rx (format #f "(~a)" (caddr args)))))
          (begin
            (overwrite-file todo-file (string-append (string-join (map (lambda (task)
                                                                         (if (= (task-id task) (string->number (cadr args)))
                                                                             (task->string (update-task task
                                                                                                        priority: (if (equal? (caddr args) "-")
                                                                                                                      #f
                                                                                                                      (caddr args))))
                                                                             (task->string task)))
                                                                       (parse-filename todo-file)) "\n") "\n")))
          (print "Usage: todo pri [task-id] [new-priority]")))
     (#t (print "Usage: todo action args")))))
(run (cdr (argv)))
