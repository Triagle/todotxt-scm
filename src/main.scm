;; Main CLI interface
(declare (uses todotxt todotxt-utils))
(require-extension fmt fmt-unicode comparse irregex fmt-color numbers symbol-utils)
(use fmt fmt-color fmt-unicode irregex utils comparse numbers symbol-utils)
(define (print-application list display-fn join)
  ;; Print a list of the result of display-fn on every item in list
  (fmt #t (fmt-unicode (fmt-join (o dsp display-fn) list "\n") nl)))
(define (cycle-priority task movement default-character)
  ;; Update a tasks priority, keeping it as is if it is the default-character, or increasing it by the amount indicated by movement
  (update-task task priority: (cond
                               ((equal? (task-priority task) default-character) default-character) ;; If the current priority is the default character, keep as is
                               ((not (task-priority task)) default-character) ;; If the task does not have a priority, add it
                               (#t (integer->char (movement (char->integer (task-priority task)) 1)))))) ;; Otherwise manipulate the current priority with the movement function
(define (add-to-todo task property property-fn value)
  (update-task task
               (symbol->keyword property)
               (cons value (property-fn task))))
(define (remove-from-todo task property property-fn value)
  (update-task
   (symbol->keyword property)
   (remove (cut equal? value <>) (property-fn t))))
(define (list-unique-properties tasks property-fn)
  ;; Return a list where each item is a distinct (unique) result of applying property-fn on tasks
  (delete-duplicates (flatten (map property-fn tasks))))
(define (colour-days-out date-str)
  (let [(days-from-now (date-cmp-now date-str))]
    (cond
     [(>= days-from-now 0) fmt-red]
     [(< days-from-now -2) fmt-blue]
     [#t fmt-white])))
(define (open file)
  (system (string-append "xdg-open" " " file)))
(define (as-ids arg)
  (map string->number (string-split arg ",")))
(define (join-structs structs accessing-function joiner)
  (string-join (map ->string (filter identity (map accessing-function structs))) joiner))
(define (colour-priority task)
  (let ((priority (task-priority task)))
    (cond
     ((equal? priority #\A) (fmt #f (fmt-bold priority)))
     (#t priority))))
(define (edit file)
  (let [(editor (or (get-environment-variable "EDITOR") "vi"))]
    (system (string-append editor " " file))))
(define (print-tasks tasks)
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
                                                                                                        (fmt #f
                                                                                                             ((if (and (equal? (car addon) "due") (date-soon (cdr addon)))
                                                                                                                  (o dsp fmt-bold (colour-days-out (cdr addon)))
                                                                                                                  dsp) (car addon)) (dsp ":") (dsp (cdr addon)))) (task-property x)) ", "))
                                                                            "\n")))) " |"))))
(define-syntax define-cli-interface
  (syntax-rules ()
    ((_ args (actions* ...) (extension* ...))
     (define-cli-interface args (actions* ... extension* ...)))
    ((_ args (((argument-strings* ...) (argument-names* ...) . body)  actions* ...))
     (if (and (>= (length args) 1) (or (equal? (car args) '(argument-strings* ...)) (member (car args) '(argument-strings* ...))))
         (cond
          [(>= (length (cdr args)) (length '(argument-names* ...))) (begin . body)]
          [#t (format #t "Usage: todo ~a ~a~%"
                      (string-join '(argument-strings* ...) "/")
                      (string-join
                       (map (cut format #f "[~a]" <>) '(argument-names* ...)) " "))])
         (define-cli-interface args (actions* ...))))


    ((_ args ())
     (format #t "Usage: todo [action-name] [action-args]~%"))))
(define (with-task-at-id tasks id thunk)
  (map (lambda (task) (if (= (task-id task) id)
                          (thunk task)
                          task)) tasks))
(define (with-tasks-at-ids tasks ids thunk)
  (foldr (lambda (id tasks) (with-task-at-id tasks id thunk)) tasks ids))
(define (standard-task-filter filter-args show-all?)
  (lambda (x)
    (and (not (task-inbox x))
         (or show-all? (not (task-done x)) )
         (if (zero? (string-length filter-args))
             (not (task-inbox x))
             (irregex-match  (irregex (string-concatenate (list ".*" (irregex-quote filter-args) ".*"))) (task->string x))))))
(define (get-applicable-todo-directory . fallback-dirs)
  (let [(directories (cons "./" fallback-dirs))]
    (find (o file-exists? (cut string-append <> "todo.txt")) directories)))
(define (run args)
  (let* ((action (or (= (length args) 0) (car args)))
         (todo-dir (get-applicable-todo-directory (get-environment-variable "TODO_DIR")))
         (todo-file (string-append todo-dir "todo.txt"))
         (tasks (parse-filename todo-file))
         (done-file (string-append todo-dir "done.txt"))
         (done-tasks (parse-filename done-file))
         (report (string-append todo-dir "report.txt"))
         (action-args (cdr args)))
    (define-cli-interface args
      ((("list" "ls" "listall") ()
        ;; List tasks in todo file, optionally including done tasks as well.
        (let ((task-count (+ (length tasks) (if (equal? action "listall") ;; If the user is trying to list done tasks, they should be included in the count.
                                                (length done-tasks)
                                                0)))
              ;; Tasks are filtered using the standard task filter, and then sorted by their priority
              (tasks (sort (filter (standard-task-filter (string-join action-args " ") (equal? action "listall"))
                                   (append tasks done-tasks)) task-priority<?)))
          (print-tasks tasks)
          (fmt #t
           "---" nl
           (length tasks) " out of " task-count " task" (if (= task-count 1)
                                                            ""
                                                            "s") " shown." nl)))
       (("next") ()
        ;; Display the next most important task (by priority) in the task list
        ;; Again we filter by the passed arguments and sort by priority
        (let [(tasks (sort (filter (standard-task-filter (string-join action-args " " ) #f) tasks) task-priority<?))]
          (if tasks
              ;; Pop off the top task (because of the sorting this is also the highest priority), and print it in text form.
              (print (task->string (car tasks)))
              (print "No tasks to do next."))))
       (("edit") ()
        ;; Open the todo file in $EDITOR
        (edit todo-file))
       (("inbox" "in") ()
        ;; Similar to ls and next, but the standard task filter is replaced with one that simply filters by tasks that are marked as inbox items
        (let [(tasks (filter task-inbox tasks))]
          (print-tasks tasks)))
       (("refile") (id)
        ;; Overwrite the existing todo file, where the task at id is changed such that it no longer has an inbox status
        (overwrite-file todo-file (format-tasks-as-file (with-task-at-id tasks (string->number (car action-args))
                                                                         (cut update-task <> inbox: #f)))))
       (("listproj" "lsprj") ()
        ;; List all the projects that are present in any task on the todo list
        (print-application (list-unique-properties tasks task-project) identity "\n"))

       (("listcon" "lsc") ()
        ;; List all the contexts that are present in any task on the todo list
        (print-application (list-unique-properties tasks task-context) identity "\n"))

       (("rm" "del") (ids)
        ;; Delete id or ids in todo list, overwriting the original todo file
        (let ((ids (as-ids (car action-args))))
          (overwrite-file todo-file (format-tasks-as-file
                                     (remove (lambda (task)
                                               (member (task-id task) ids)) tasks)))))
       (("replace") (id todo)
        ;; Replace the todo at id with new text todo, overwriting the original todo file. Equivalent to todo rm and then todo add.
        (let ((id (car action-args))
              (todo (cdr action-args)))
          (overwrite-file todo-file (fmt #f
                                         (dsp
                                          (format-tasks-as-file
                                           ;; remove the original todo first.
                                           (remove (lambda (task)
                                                     (= (task-id task) (string->number id))) tasks)))
                                         ;; Append to the end of the file.
                                         (string-join todo " ")
                                         nl))))
       (("addon-modify" "admod" "am") (id key value)
        (let ((id (car action-args))
              (key (cadr action-args))
              (value (caddr action-args)))
          ;; Modify (or add) an addon (property internally) of a todo.
          (overwrite-file todo-file (format-tasks-as-file (with-task-at-id tasks (string->number id)
                                                                           (lambda (t)
                                                                             (update-task t
                                                                                          ;; the property is first removed from the property alist, and then consed to the front.
                                                                                          property: (cons (cons key value) (rm-prop key (task-property t))))))))))
       (("addon-remove" "adrm" "ar") (id key)
        (let ((id (car action-args))
              (key (cadr action-args)))
          ;; Remove an addon of a todo
          (overwrite-file todo-file (format-tasks-as-file (with-task-at-id tasks (string->number id)
                                                                           (lambda (t)
                                                                             (update-task t
                                                                                          property: (rm-prop key (task-property t)))))))))
       (("add" "a") ()
        ;; Simple append text passed as the arguments to a file.
        (write-to-a-file todo-file (string-join action-args " ")))
       (("open" "o") (id)
        ;; Open the attachments of a todo at id, prompting for file selection if necessary
        (let* [(id (string->number (car action-args)))
               ;; Get selected task
               (selected-task (find (lambda (t) (equal? id (task-id t))) tasks))
               ;; Split the "attach" property of the task by , (list separator).
               (attachments (string-split (assoc-v "attach" (task-property selected-task)) ","))]
          (cond
           ;; Base case, no attachments, therefore just print that none were found.
           [(null-list? attachments) (fmt #t "No attachments found for the selected task." nl)]
           ;; If only one attachment, assume the user intends to open it and don't bug them
           [(= (length attachments) 1) (edit (car attachments))]
           [#t (let [(attachment-pair
                      ;; Pair the attachments with a number to represent them for user selection
                      ;; E.g A list '(file1.txt file2.txt) would become '((1 file1.txt) (2 file2.txt))
                      (zip (map (cut + 1 <>) (iota (length attachments))) attachments))]
                 (fmt #t (fmt-join (lambda (attachment)
                                     ;; Convert the now paired attachments into a string form, e.g '(1 file1.txt) -> "1. file1.txt"
                                     (dsp (string-append (->string (car attachment)) ". " (cadr attachment)))) attachment-pair "\n")
                      nl
                      "Select attachment [1-" (length attachments) "]: ")
                 (let [(in (string->number (read-line)))]
                   ;; Open the attachment at id in, read from stdin
                   (open (cadr (find (lambda (attachment) (equal? (car attachment) in)) attachment-pair)))))])))
       (("done" "do" "mark" "complete" "tick") (ids)
        (let ((ids (as-ids (car action-args))))
          ;; Append marked tasks to done file
          (write-to-a-file done-file (format-tasks-as-file
                                      (remove
                                       ;; Remove all tasks that aren't done, thus isolated the newly updated tasks
                                       (complement task-done)
                                       ;; Update tasklist, marking the selected ids as done.
                                       (with-tasks-at-ids tasks ids (cut update-task <> done: #t)))))
          ;; Overwrite the original todo file, removing the newly marked todo items unless a recur property prevents them
          (overwrite-file todo-file (format-tasks-as-file
                                     ;; Schedule a new todo item in the future for all todos at ids that have a recur property
                                     (with-tasks-at-ids
                                      (remove (lambda (task)
                                                ;; Remove only task from the todo list only if
                                                ;; That task is one of the selected tasks from the user
                                                ;; That task also does not have a recur property.
                                                (and (member (task-id task) ids)
                                                     (not (assoc "recur" (task-property task))))) tasks)
                                      ids
                                      (lambda (t)
                                        ;; If the task has a due date and a recur property
                                        (if (and (assoc "recur" (task-property t)) (assoc "due" (task-property t)))
                                            ;; Add the value of the recur property to the due date of the task and return that new task
                                            (begin
                                              (fmt #t (fmt-unicode (fmt-bold "Task is recurrent, adding another in the future") nl) )
                                              (task-due-add t (string->number (assoc-v "recur" (task-property t)))))
                                            ;; Otherwise just return the same task untouched
                                            t)))))))
       (("bump" "promote") (ids)
        (let ((ids (as-ids (car action-args))))
          ;; Cycle the priority of the tasks upwards with ids in ids, giving them a priority of A if they don't already have one
          (overwrite-file todo-file (format-tasks-as-file (with-tasks-at-ids tasks ids (cut cycle-priority <> + #\A))))))

       (("curb" "demote") (ids)
        (let ((ids (as-ids (car action-args))))
          ;; Cycle the priority of the tasks downwards with ids in ids, giving them a priority of Z if they don't already have one
          (overwrite-file todo-file (format-tasks-as-file (with-tasks-at-ids tasks ids (cut cycle-priority <> + #\Z))))))
       (("add-context" "ac") (ids context)
        (let ((ids (as-ids (car action-args)))
              (context (cadr action-args)))
          ;; Add a context to a todo item
          (overwrite-file todo-file (format-tasks-as-file (with-tasks-at-ids tasks ids
                                                                             (cut add-to-todo <> 'context task-context context))))))
       (("rm-context" "rc") (ids context)
        (let ((ids (as-ids (car action-args)))
              (context (cadr action-args)))
          ;; Remove a context to a todo item
          (overwrite-file todo-file (format-tasks-as-file (with-tasks-at-ids tasks ids
                                                                             (cut remove-from-todo <> 'context task-context context))))))
       (("add-project" "ap") (ids project)
        (let ((ids (as-ids (car action-args)))
              (project (cadr action-args)))
          ;; Add a project to a todo item
          (overwrite-file todo-file (format-tasks-as-file (with-tasks-at-ids tasks ids
                                                                             (cut add-to-todo <> 'project task-project project))))))
       (("rm-project" "rp") (ids project)
        (let ((ids (as-ids (car action-args)))
              (project (cadr action-args)))
          ;; Remove a project to a todo item
          (overwrite-file todo-file (format-tasks-as-file (with-tasks-at-ids tasks ids
                                                                             (cut remove-from-todo <> 'project task-project project))))))
       (("log") ()
        ;; Add a todo to the done file, marked as done (logging that you've done something so to speak).
        (write-to-a-file done-file (string-append "x " (string-join action-args " "))))
       (("pri") (id new-priority)
        ;; Set the priority of a todo item
        (let ((id (car action-args))
              (new-priority (cadr action-args)))
          ;; New priority can either be an uppercase character, or "-" which resets the priority.
          (if (or (equal? new-priority "-") (irregex-match "[A-Z]" (format #f "~a" new-priority)))
              ;; Update the priority of the todo item
              (overwrite-file todo-file (format-tasks-as-file (with-task-at-id tasks (string->number id)
                                                                               (cut update-task <> priority: (if (equal? new-priority "-")
                                                                                                                 #f ;; false equates to no priority internally
                                                                                                                 new-priority)))))
              ;; Let the user know that it is an invalid priority
              (fmt #t (fmt-bold (fmt-red (dsp "Invalid Priority: "))) (dsp new-priority) nl))))))))
(if (> (length (argv)) 1)
    (run (cdr (argv)))
    (fmt #t (dsp "todo [action-name] [action-args]") nl))
