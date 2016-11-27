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
;; Usage
;; (define-cli-interface args (("list" '(filter) (print "hello world"))))
(define-syntax define-cli-interface
  (syntax-rules ()
    ((_ args (actions* ...) (extension* ...))
       (define-cli-interface args (actions* ... extension* ...)))
    ((_ args (((argument-strings* ...) (argument-names* ...) . body)  actions* ...))
     (if (and (>= (length args) 1) (or (equal? (car args) '(argument-strings* ...)) (member (car args) '(argument-strings* ...))))
         (if (or (equal? '(#f) '(argument-names* ...)) (>= (length (cdr args)) (length '(argument-names* ...))) )
             (begin . body)
             (format #t "Usage: todo ~a ~a~%"
                     (string-join '(argument-strings* ...) "/")
                     (string-join
                      (map (lambda (cur)
                            (format #f "[~a]" cur)) '(argument-names* ...)) " ")))
         (define-cli-interface args (actions* ...))))
    ((_ args ())
     (format #t "Usage: todo [action-name] [action-args]~%"))))
(define (with-task-at-id tasks id thunk)
  (map (lambda (task) (if (= (task-id task) id)
                          (thunk task)
                          task)) tasks))
(define (run args)
  (let* ((action (or (= (length args) 0) (car args)))
         (todo-dir (or (get-environment-variable "TODO_DIR") "./"))
         (todo-file (string-append todo-dir "todo.txt"))
         (tasks (parse-filename todo-file))
         (done-file (string-append todo-dir "done.txt"))
         (done-tasks (parse-filename done-file))
         (report (string-append todo-dir "report.txt"))
         (action-args (cdr args)))
    (define-cli-interface args
      ((("list" "ls" "listall") (#f)
        (let ((tasks (sort (filter (lambda (x)
                                     (and (or (equal? action "listall") (not (task-done x)))
                                          (if (= (length (cdr args)) 0)
                                              #t
                                              (irregex-match  (irregex (string-concatenate (list ".*" (irregex-quote (string-join (cdr args) " ")) ".*"))) (task->string x)))))
                                   (append tasks done-tasks)) task-priority<?)))
          (fmt #t (fmt-unicode
                   (tabular
                    "| " (dsp (string-concatenate (list "ID\n" (join-structs tasks (lambda (x) (if (task-done x)
                                                                                                   "x"
                                                                                                   (fmt #f (num (task-id x))))) "\n")))) " | "
                    (dsp (string-concatenate (list "Priority\n" (join-structs tasks colour-priority "\n")))) " | "
                    (dsp (string-concatenate (list "Task\n" (join-structs tasks (lambda (task)
                                                                                  (if (task-done task)
                                                                                      (fmt #f (fmt-unicode (fmt-green (dsp (task-text task)))))
                                                                                      (task-text task))) "\n")))) " | "

                    (dsp (string-concatenate (list "Projects\n" (join-structs tasks (lambda (x) (string-join (task-project x) ", ")) "\n")))) " | "
                    (dsp (string-concatenate (list "Contexts\n" (join-structs tasks (lambda (x) (string-join (task-context x) ", ")) "\n")))) " | "
                    (dsp (string-concatenate (list "Addons\n" (join-structs tasks (lambda (x)
                                                                                    (string-join (map (lambda (addon)
                                                                                                        (fmt #f (dsp (car addon)) (dsp ":") (dsp (cdr addon)))) (task-property x)) ", "))
                                                                            "\n")))) " |")))))
       (("listproj" "lsprj") (#f)
        (fmt #t (fmt-unicode (dsp (string-join (filter
                                                (lambda (x) (if (= (length action-args) 0)
                                                                #t
                                                                (irregex-match  (irregex (string-concatenate (list ".*" (irregex-quote (string-join action-args " ")) ".*"))) x)))
                                                (delete-duplicates (flatten (map task-project tasks)))) "\n")) nl)))

       (("listcon" "lsc") (#f)
        (fmt #t (fmt-unicode (dsp (string-join (filter
                                                (lambda (x) (if (= (length action-args) 0)
                                                                #t
                                                                (irregex-match  (irregex (string-concatenate (list ".*" (irregex-quote (string-join action-args " ")) ".*"))) x)))
                                                (delete-duplicates (flatten (map task-context tasks)))) "\n")) nl)))

       (("rm" "del") (id)
        (overwrite-file todo-file (format-tasks-as-file
                                   (remove (lambda (task)
                                             (= (task-id task) (string->number (cadr args)))) tasks))))
       (("replace") (id todo)
        (let ((new-task (string-join (cddr args) " ")))
          (overwrite-file todo-file (fmt #f
                                         (dsp
                                          (format-tasks-as-file
                                           (remove (lambda (task)
                                                     (= (task-id task) (string->number (cadr args)))) tasks)))
                                         new-task
                                         nl))))
       (("add" "a") (todo)
        (write-to-a-file todo-file (string-join action-args " ")))
       (("done" "do" "mark" "complete" "tick") (id)
        (write-to-a-file done-file (format-tasks-as-file
                                    (remove (lambda (task)
                                              (not (task-done task))) (with-task-at-id tasks (string->number (cadr args)) (cut update-task <> completed: #t)))))
        (overwrite-file todo-file (format-tasks-as-file (remove (lambda (task)
                                                                  (= (task-id task) (string->number (cadr args)))) tasks))))
       (("add-context" "ac") (id context)
        (overwrite-file todo-file (format-tasks-as-file (with-task-at-id tasks (string->number (car action-args))
                                                                         (lambda (t)
                                                                           (update-task t context: (cons (cadr action-args) (task-context t))))))))

       (("add-project" "ap") (id project)
        (overwrite-file todo-file (format-tasks-as-file (with-task-at-id tasks (string->number (car action-args))
                                                                         (lambda (t)
                                                                           (update-task t project: (cons (cadr action-args) (task-project t))))))))
       (("pri") (id new-priority)
        (if (or (equal? (caddr args) "-") (parse priority (format #f "(~a) " (caddr args))))
            (overwrite-file todo-file (format-tasks-as-file (with-task-at-id tasks (string->number (cadr args))
                                                                             priority: (if (equal? (caddr args) "-")
                                                                                           #f
                                                                                           (caddr args)))))
            (fmt #t (fmt-bold (fmt-red (dsp "Invalid Priority: "))) (dsp (caddr args)) nl)))))))
(if (> (length (argv)) 1)
    (run (cdr (argv)))
    (fmt #t (dsp "todo [action-name] [action-args]") nl))
