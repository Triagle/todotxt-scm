;; Main CLI interface
(declare (uses todotxt todotxt-utils))
(require-extension fmt fmt-unicode comparse irregex fmt-color numbers symbol-utils srfi-19-support)
(use fmt fmt-color fmt-unicode irregex utils comparse numbers symbol-utils srfi-19-support)
(define (print-application list display-fn join)
  ;; Print a list of the result of display-fn on every item in list
  (fmt #t (fmt-unicode (fmt-join (o dsp display-fn) list "\n") nl)))
(define (cycle-priority task movement default-character)
  ;; Update a task's priority, keeping it as is if it is the default-character, or changing it to the result of applying movement
  (update-task task priority: (cond
                               ((equal? (task-priority task) default-character) default-character) ;; If the current priority is the default character, keep as is
                               ((not (task-priority task)) default-character) ;; If the task does not have a priority, add it
                               (#t (integer->char (movement (char->integer (task-priority task)) 1)))))) ;; Otherwise manipulate the current priority with the movement function
(define (add-to-todo task property property-fn value)
  ;; Add value to the beginning of a list which is the property of task, using property-fn to retrieve it's original value
  (update-task task
               (symbol->keyword property)
               (cons value (property-fn task))))
(define (remove-from-todo task property property-fn value)
  ;; Remove a value from a list which is the property of task, using property-fn to retrieve it's original value
  (update-task task
               (symbol->keyword property)
               (remove (cut equal? value <>) (property-fn task))))
(define (list-unique-properties tasks property-fn)
  ;; Return a list where each item is a distinct (unique) result of applying property-fn on all tasks
  (delete-duplicates (flatten (map property-fn tasks))))
(define (colour-days-out date-str)
  ;; Return a colour function that indicates to the user how far away a due date is
  ;; The due date is in string form initially (date-str), however this is parsed internally by date-cmp-now
  (let [(days-from-now (date-cmp-now date-str))] ;; date-cmp-now gets the number of days from now the due date is
    ;; date-cmp-now has one of three possible valid return values
    ;; - A positive number, i.e the date is earlier than now
    ;; - A negative number, i.e the date is later than now
    ;; - 0, i.e the same dates
    ;; The exact number of days is mapped to red, blue and white.
    (cond
     ;; The task is overdue!!
     [(>= days-from-now 0) fmt-red]
     ;; The task is < 2 days out. I like to be warned that a task is impending, so that I can prioritize it.
     [(< days-from-now -2) fmt-blue]
     ;; The task has a due date, but it is too far out to be of much significance.
     [#t fmt-white])))
(define (open file)
  ;; Open file using xdg-open.
  (system (string-append "xdg-open" " " file)))
(define (err summary body)
  ;; Print a standard error message
  ;; The summary of this message is bolded and red, separated from the standard body text by a colon
  (fmt #t (fmt-bold (fmt-red summary)) ": " body nl))
(define (invalid-id-err id)
  ;; Specialized err function for invalid ids specifically
  (err "Invalid id" id))
(define (valid-ids ids)
  ;; Check that a list of parsed ids is valid
  (every identity ids))
(define (as-ids arg)
  ;; Map a list of comma delimited ids to numbers
  ;; e.g "1,2,3" -> '(1 2 3)
  (map string->number (string-split arg ",")))
(define (colour-priority task)
  ;; Colour a task's priority based on it's importance
  ;; The priorities are mapped as follows:
  ;; A -> bolded text
  ;; Everything else -> unchanged
  (let ((priority (task-priority task)))
    (cond
     ((equal? priority #\A) (fmt-bold priority))
     (#t priority))))
(define (edit file)
  ;; Open the file in an appropriate editor. By default this is vi (for compatibility reasons), but can be the value of $EDITOR if set
  (let [(editor (or (get-environment-variable "EDITOR") "vi"))]
    (system (string-append editor " " file))))
(define (column title formatter tasks)
  ;; Return a "column" (a series of newline separated values for the fmt function to turn into table columns) from a list of tasks and a formatter
  ;; E.g (column "hello" identity '("a" "b" "c")) -> "hello\na\nb\nc"
  (cat title "\n" (fmt-join (lambda (x)
                              ;; Either take the value of the formatter on item x, or if that returns #f an empty string ""
                              (dsp (or (formatter x) ""))) tasks "\n")))
(define (print-tasks tasks)
  ;; Print tasks in table form, with each column growing as required
  (fmt #t (fmt-unicode
           (tabular
            "| "
            ;; ID column consists of either the task id (e.g 1) or an 'x' if that task is done
            (column "ID" (lambda (x) (if (task-done x)
                                         "x"
                                         (num (task-id x)))) tasks)
            " | "
            ;; See colour-priority for how this is formatted
            (column "Priority" colour-priority tasks)
            " | "
            ;; If the task is done, the task text is formatted in green, and is otherwise normal text
            (column "Task" (lambda (task)
                             (if (task-done task)
                                 (fmt-unicode (fmt-green (dsp (task-text task))))
                                 (task-text task))) tasks)
            " | "
            ;; Join the projects by ',' (e.g "todo.txt,programming").
            ;; Although the todo.txt gtd mantra frowns upon multiple projects, it can be very useful for delimiting sub projects.
            (column "Projects"
                    (lambda (task) (fmt-join dsp (task-project task) ", ")) tasks)
            " | "
            ;; Join the contexts of a task by ',' (e.g home,computer,mall)
            (column "Contexts" (lambda (task) (fmt-join dsp (task-context task) ", ")) tasks)
            " | "
            ;; Join the properties/addons in the format "key:value,key1:value1"
            ;; The "due" property is treated separately in that it is highlighted depending on the urgency of the due date.
            (column "Addons" (lambda (task)
                               (fmt-join dsp (map (lambda (addon)
                                                    (cat ((if (and (equal? (car addon) 'due) (date? (cdr addon)) (date-soon (cdr addon)))
                                                              ;; If the property is "due" and the due date is due soon, colour it.
                                                              (o fmt-bold (colour-days-out (cdr addon)))
                                                              ;; Otherwise leave as is
                                                              identity) (car addon)) ":" (property-value->string (cdr addon)))) (task-property task)) ", ")) tasks) " |"))))
(define-syntax define-cli-interface
  ;; Simple little macro that defines the style of command line interface
  ;; The syntax is simple

  ;; (define-cli-interface <passed-command-line-arguments>
  ;;     ((("list") () (print "listing"))))
  ;; Every subcommand (todo "ls", todo "add", etc) has three parts
  ;; The action-strings: ("list"). These are the aliases for the subcommand
  ;; The argument-names: (). A simple list of the names of the arguments (an empty list meaning any number of arguments is accepted).
  ;; The body: (print "listing"). The body of the subcommand.
  (syntax-rules ()
    ((_ args (((action-strings* ...) (argument-names* ...) . body)  actions* ...))
     ;; If the first user passed cli argument is within the argument-strings
     (if (member (car args) '(action-strings* ...))
         (cond
          ;; If the length of the args is greater than or equal to the length of required subcommand arguments
          [(>= (length (cdr args)) (length '(argument-names* ...)))
           ;; Execute the subcommand
           (begin . body)]
          [#t
           ;; Otherwise print the generated usage string for this subcommand
           (fmt #t "Usage: todo "
                ;; Present the possible aliases of a given action separated by '/' (e.g "pri/new-priority")
                (fmt-join dsp '(action-strings* ...) "/")
                " "
                ;; Present the arguments to the subcommand (e.g "[id] [new-priority]")
                (fmt-join dsp (map (o (cut cat "[" <> "]")) '(argument-names* ...)) " ")
                nl)])
         (define-cli-interface args (actions* ...))))
    ;; Base case, if the user has specified an action that is unknown
    ((_ args ()) (fmt #t (fmt-unicode (fmt-red (fmt-bold "Unrecognized action: ")) (car args) "." nl)))))
(define (with-task-at-id tasks id thunk)
  ;; Map over tasks, applying thunk to the task whose id matches the one selected.
  (map (lambda (task) (if (= (task-id task) id) ;; If task id matches
                          (thunk task)
                          task)) tasks))
(define (with-tasks-at-ids tasks ids thunk)
  ;; Same as tasks-at-ids, but works with multiple ids
  (foldr (lambda (id tasks) (with-task-at-id tasks id thunk)) tasks ids))
(define (standard-task-filter filter-args show-all?)
  ;; Returns a function that can be passed to filter
  ;; It by default filters out inbox items, as well as done items
  ;; The show-all? boolean can be used to control whether done items are shown as well
  ;; filter-args is a string that is fuzzy matched within the string representation of a task with irregex
  (lambda (x)
    ;; Only keep tasks that:
    ;; Aren't inbox items
    ;; Aren't done (unless show-all? permits done items to be included)
    ;; Match the fuzzy filter (basic substring search) on the string representation of the task
    ;; String representation is like "(A) task @context +project key:value", or as shown in the todo.txt file
    (and (not (task-inbox x))
         (or show-all? (not (task-done x)) )
         (or (zero? (string-length filter-args))
             ;; Fuzzy match
             (substring-index filter-args (task->string x))))))
(define (get-applicable-todo-directory . fallback-dirs)
  ;; Return the first directory in the list (if any) that has a todo.txt file.
  ;; The list always contains the current directory, with the supplied arguments being fallbacks.
  (let [(directories (cons "./" fallback-dirs))]
    (find (lambda (dir)
            (and (file-exists? (string-append dir "todo.txt")) (file-exists? (string-append dir "done.txt")))) directories)))
(define (run args)
  ;; args is a list of strings contained the arguments passed to the executable
  ;; e.g '("pri" "2" "A")
  (let* (;; The first item in the list of arguments is always the action
         (action (or (= (length args) 0) (car args)))
         ;; Get an applicable todo directory to source the todo.txt and done.txt file from
         (todo-dir (or (get-applicable-todo-directory (get-environment-variable "TODO_DIR")) (begin (err "No applicable todo directory found.")
                                                                                                    (exit -1))))
         ;; Path representation of the todo.txt file
         (todo-file (string-append todo-dir "todo.txt"))
         ;; The parsed todo.txt file
         (tasks (parse-filename todo-file))
         ;; Path representation of the done.txt file
         (done-file (string-append todo-dir "done.txt"))
         ;; The parsed done.txt file
         (done-tasks (parse-filename done-file))
         ;; The tail (or cdr) of the arguments is always the arguments of a given action
         (action-args (cdr args)))
    (if (and tasks done-tasks)
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
            (let [(id (string->number (car action-args)))]
              (if id
                  (overwrite-file todo-file (format-tasks-as-file (with-task-at-id tasks id
                                                                                   (cut update-task <> inbox: #f))))
                  (invalid-id-err (car action-args)))))
           (("listproj" "lsprj") ()
            ;; List all the projects that are present in any task on the todo list
            (print-application (list-unique-properties tasks task-project) identity "\n"))

           (("listcon" "lsc") ()
            ;; List all the contexts that are present in any task on the todo list
            (print-application (list-unique-properties tasks task-context) identity "\n"))

           (("rm" "del") (ids)
            ;; Delete id or ids in todo list, overwriting the original todo file
            (let ((ids (as-ids (car action-args))))
              (if (valid-ids ids)
                  (overwrite-file todo-file (format-tasks-as-file
                                             (remove (lambda (task)
                                                       (member (task-id task) ids)) tasks)))
                  (invalid-id-err (car action-args)))))
           (("replace") (id todo)
            ;; Replace the todo at id with new text todo, overwriting the original todo file. Equivalent to todo rm and then todo add.
            (let ((id (string->number (car action-args)))
                  (todo (cdr action-args)))
              (if id
                  (overwrite-file todo-file (fmt #f
                                                 (dsp
                                                  (format-tasks-as-file
                                                   ;; remove the original todo first.
                                                   (remove (lambda (task)
                                                             (= (task-id task) id)) tasks)))
                                                 ;; Append to the end of the file.
                                                 (string-join todo " ")
                                                 nl))
                  (invalid-id-err (car action-args)))))
           (("addon-modify" "admod" "am") (id key value)
            (let ((id (string->number (car action-args)))
                  (key (string->symbol (cadr action-args)))
                  (value (caddr action-args)))
              (cond
               [(not (valid-property-value value)) (err "Invalid property value" value)]
               [(not id) (invalid-id-err (car action-args))]
               [#t (overwrite-file todo-file (format-tasks-as-file (with-task-at-id tasks id
                                                                                    (lambda (t)
                                                                                      (update-task t
                                                                                                   ;; the property is first removed from the property alist, and then consed to the front.
                                                                                                   property: (cons (cons key value) (rm-prop key (task-property t))))))))])))
           (("addon-remove" "adrm" "ar") (id key)
            (let ((id (string->number (car action-args)))
                  (key (string->symbol (cadr action-args))))
              (if id
                  ;; Remove an addon of a todo
                  (overwrite-file todo-file (format-tasks-as-file (with-task-at-id tasks id
                                                                                   (lambda (t)
                                                                                     (update-task t
                                                                                                  property: (rm-prop key (task-property t)))))))
                  (invalid-id-err (car action-args)))))
           (("add" "a") ()
            ;; Simple append text passed as the arguments to a file.
            (write-to-a-file todo-file (string-join action-args " ")))
           (("open" "o") (id)
            ;; Open the attachments of a todo at id, prompting for file selection if necessary
            (let* [(id (string->number (car action-args)))
                   ;; Get selected task
                   (selected-task (find (lambda (t) (equal? id (task-id t))) tasks))
                   ;; The attachments are stored in the attach property of the task
                   (attachments (if selected-task (assoc-v 'attach (task-property selected-task)) #f))]
              (cond
               ;; Invalid id or id is out of bounds
               [(or (not id) (not selected-task)) (invalid-id-err (car action-args))]
               ;; Attachments is a garbage value, i.e not a list of strings or not a string
               [(not (or (string? attachments) (and (list? attachments) (every string? attachments)))) (fmt #t (fmt-bold (fmt-red "Invalid attachment(s): ")) (property-value->string attachments) nl)]
               ;; If only one attachment, assume the user intends to open it and don't bug them
               [(string? attachments) (edit attachments)]
               ;; No attachments, therefore just print that none were found.
               [(null-list? attachments) (fmt #t "No attachments found for the selected task." nl)]
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
              (if (valid-ids ids)
                  (begin
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
                                                               (not (assoc 'recur (task-property task))))) tasks)
                                                ids
                                                (lambda (t)
                                                  ;; If the task has a due date and a recur property
                                                  (if (and (assoc 'recur (task-property t)) (assoc 'due (task-property t)) (date? (assoc-v 'due (task-property t))) (number? (assoc-v 'recur (task-property t))))
                                                      ;; Add the value of the recur property to the due date of the task and return that new task
                                                      (begin
                                                        (fmt #t (fmt-unicode (fmt-bold "Task is recurrent, adding another in the future") nl) )
                                                        (task-due-add t (assoc-v 'recur (task-property t))))
                                                      ;; Otherwise just return the same task untouched
                                                      t))))))
                  (invalid-id-err (car action-args)))))
           (("bump" "promote") (ids)
            (let ((ids (as-ids (car action-args))))
              (if (valid-ids ids)
                  ;; Cycle the priority of the tasks upwards with ids in ids, giving them a priority of A if they don't already have one
                  (overwrite-file todo-file (format-tasks-as-file (with-tasks-at-ids tasks ids (cut cycle-priority <> - #\A))))
                  (invalid-id-err (car action-args)))))

           (("curb" "demote") (ids)
            (let ((ids (as-ids (car action-args))))
              (if (valid-ids ids)
                  ;; Cycle the priority of the tasks downwards with ids in ids, giving them a priority of Z if they don't already have one
                  (overwrite-file todo-file (format-tasks-as-file (with-tasks-at-ids tasks ids (cut cycle-priority <> + #\Z))))
                  (invalid-id-err (car action-args)))))
           (("add-context" "ac") (ids context)
            (let ((ids (as-ids (car action-args)))
                  (context (cadr action-args)))
              (if (valid-ids ids)
                  ;; Add a context to a todo item
                  (overwrite-file todo-file (format-tasks-as-file (with-tasks-at-ids tasks ids
                                                                                     (cut add-to-todo <> 'context task-context context))))
                  (invalid-id-err (car action-args)))))
           (("rm-context" "rc") (ids context)
            (let ((ids (as-ids (car action-args)))
                  (context (cadr action-args)))
              (if (valid-ids ids)
                  ;; Remove a context to a todo item
                  (overwrite-file todo-file (format-tasks-as-file (with-tasks-at-ids tasks ids
                                                                                     (cut remove-from-todo <> 'context task-context context))))
                  (invalid-id-err (car action-args)))))
           (("add-project" "ap") (ids project)
            (let ((ids (as-ids (car action-args)))
                  (project (cadr action-args)))
              (if (valid-ids ids)
                  ;; Add a project to a todo item
                  (overwrite-file todo-file (format-tasks-as-file (with-tasks-at-ids tasks ids
                                                                                     (cut add-to-todo <> 'project task-project project))))
                  (invalid-id-err (car action-args)))))
           (("rm-project" "rp") (ids project)
            (let ((ids (as-ids (car action-args)))
                  (project (cadr action-args)))
              (if (valid-ids ids)
                  ;; Remove a project to a todo item
                  (overwrite-file todo-file (format-tasks-as-file (with-tasks-at-ids tasks ids
                                                                                     (cut remove-from-todo <> 'project task-project project))))
                  (invalid-id-err (car action-args)))))
           (("log") ()
            ;; Add a todo to the done file, marked as done (logging that you've done something so to speak).
            (write-to-a-file done-file (string-append "x " (string-join action-args " "))))
           (("pri") (id new-priority)
            ;; Set the priority of a todo item
            (let ((id (string->number (car action-args)))
                  (new-priority (cadr action-args)))
              (if id
                  (if (or (equal? new-priority "-") (irregex-match "[A-Z]" (format #f "~a" new-priority)))
                      ;; Update the priority of the todo item
                      ;; New priority can either be an uppercase character, or "-" which resets the priority.
                      (overwrite-file todo-file (format-tasks-as-file (with-task-at-id tasks id
                                                                                       (cut update-task <> priority: (if (equal? new-priority "-")
                                                                                                                         #f ;; false equates to no priority internally
                                                                                                                         new-priority)))))
                      ;; Let the user know that it is an invalid priority
                      (err "Invalid Priority" new-priority))
                  (invalid-id-err (car action-args)))))))
        (err "Todo file invalid: " (cat "Todo file at " todo-dir " is missing, damaged, or otherwise unreadable.")))))
(if (> (length (argv)) 1)
    (run (cdr (argv)))
    (fmt #t (dsp "todo [action-name] [action-args]") nl))
