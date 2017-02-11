;; Main CLI interface
(declare (uses todotxt todotxt-utils config uri parse tree))
(require-extension args fmt fmt-unicode comparse irregex fmt-color numbers symbol-utils srfi-19-support srfi-19-time srfi-19-date bindings)
(use args fmt fmt-color fmt-unicode irregex utils comparse numbers symbol-utils srfi-19-support srfi-19-time (prefix bindings b:))

(define id
  (as-number (one-or-more digit)))
(define ids
  (bind (any-of (list-of id) (as-list id))
        (lambda (ids)
          (result (lambda (task)
                    (member (task-id task) ids))))))
(define range
  (sequence* ((lb id) (_ (char-seq "-")) (ub id))
             (result (lambda (task)
                       (and (>= (task-id task) lb) (<= (task-id task) ub))))))
(define text-selector
  (bind (as-string (one-or-more (in -space)))
        (lambda (selector)
          (result (lambda (task)
                    (substring-index selector (task->string task)))))))
(define selector
  (any-of range ids text-selector))
(define (shell-escape str)
  (irregex-replace/all "'" str "'\''"))
;; Default configuration
(define configuration (list
                       (cons 'list-style "table")
                       (cons 'todo-dir #f)
                       (cons 'sorting (list task-priority<? (cut task-property<? <> <> 'due) task-id<?))
                       (cons 'project-colour fmt-yellow)
                       (cons 'highlight-next-action #f)
                       (cons 'context-colour fmt-green)
                       (cons 'property-colour dsp)
                       (cons 'tree-property "project")
                       (cons 'overdue-colour fmt-red)
                       (cons 'columns (list "id" "priority" "task" "projects" "contexts" "properties"))
                       (cons 'priority-colours (list (list #\A fmt-red)))
                       (cons 'time-colours (list (list (make-duration days: 1) fmt-red)))))

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
               (append (property-fn task) (list value))))
(define (remove-from-todo task property property-fn value)
  ;; Remove a value from a list which is the property of task, using property-fn to retrieve it's original value
  (update-task task
               (symbol->keyword property)
               (remove (cut equal? value <>) (property-fn task))))
(define (list-unique-properties tasks property-fn)
  ;; Return a list where each item is a distinct (unique) result of applying property-fn on all tasks
  (delete-duplicates (flatten (map property-fn tasks))))
(define (colour-days-out configuration date-str)
  ;; Return a colour function that indicates to the user how far away a due date is
  ;; The due date is in string form initially (date-str), however this is parsed internally by date-cmp-now
  (let [(time-from-now (date-cmp-now date-str))] ;; date-cmp-now gets the number of days from now the due date is
    ;; date-cmp-now has one of three possible valid return values
    ;; - A positive number, i.e the date is earlier than now
    ;; - A negative number, i.e the date is later than now
    ;; - 0, i.e the same dates
    ;; The exact number of days is mapped to red, blue and white.
    (if (>= (time->days time-from-now) 0)
        (assoc-v 'overdue-colour configuration)
        (cadr (or (find (lambda (time-kv) (> (time->days (time-abs (car time-kv))) (time->days (time-abs time-from-now)))) (assoc-v 'time-colours configuration)) (list #f dsp))))))
(define (open file)
  ;; Open file using xdg-open.
  (system (string-append "xdg-open" " '" (shell-escape file) "'")))
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
(define (colour-for-priority cfg priority)
  ;; Return the colour associated with the priority
  (car (assoc-v priority (assoc-v 'priority-colours cfg) default: (list dsp))))
(define (colour-priority cfg task)
  ;; Colour a task's priority based on it's importance
  ;; The priorities are mapped as follows:
  ;; A -> bolded text
  ;; Everything else -> unchanged
  (let ((priority (task-priority task)))
    ((colour-for-priority cfg priority) (or priority ""))))
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
(define (print-task-as-highlighted configuration task)
  ;; Print a task like the task->string function, with additional colouring in a similar fashion to the original todo.txt client
  (dsp (cat
        ;; Colour the first portion of the task according to it's priority
        ((colour-for-priority configuration (task-priority task)) (cat (if (task-inbox task)
                                                                           "* "
                                                                           "")
                                                                       (if (task-done task)
                                                                           "x "
                                                                           "")
                                                                       (if (task-priority task)
                                                                           (cat "(" (task-priority task) ") ")
                                                                           "")
                                                                       (if (task-completed-date task) (cat (date->str (task-completed-date task)) " ") "")
                                                                       (if (task-date task) (cat (date->str (task-date task)) " ") "")
                                                                       (task-text task)))
        ;; Colour the project, context, and properties of a task according to their colours set by the user
        ((assoc-v 'project-colour configuration) (fmt-join (cut cat " +" <>) (sort (task-project task) string<?)))
        ((assoc-v 'context-colour configuration) (fmt-join (cut cat " @" <>) (sort (task-context task) string<?)))
        (fmt-join (lambda (property)
                    (cat " " ((if (and (equal? (car property) 'due) (date? (cdr property)))
                                  ;; If the property is "due" and the due date is due soon, colour it.
                                  (colour-days-out configuration (cdr property))
                                  ;; Otherwise leave as is
                                  (assoc-v 'property-colour configuration)) (car property)) ":" (property-value->string (cdr property)))) (task-property task)))))
(define (tree-property-adder tree t property)
  (tree-add tree (if (null-list? (property t))
                     '(none)
                     (property t)) t))
(define (tree-of-tasks tasks property)
  (foldl
   (cut tree-property-adder <> <> property)
   '()
   tasks))
(define (print-tasks-as-highlighted configuration tasks)
  ;; Print a list of tasks as highlighted output, prepending and padding the task id to each
  (fmt #t (fmt-unicode
           (tabular
            ""
            (fmt-join (cut cat <> nl) (map (cut task-id <>) tasks))
            " "
            (fmt-join (cut cat <> nl) (map (cut print-task-as-highlighted configuration <>) tasks))))))
(define tree-property-set
  (list
   (cons "project" task-project)
   (cons "context" task-context)))
(define (print-tasks-as-tree configuration tasks)
  (let [(tree-property (assoc-v (assoc-v 'tree-property configuration) tree-property-set))]
    (if tree-property
     (walk-tree (tree-of-tasks (reverse tasks) tree-property) (cut print-branch (lambda (task)
                                                                    (cat (task-id task) ". " (print-task-as-highlighted configuration task))) <> <>))
     (err "Invalid property" (assoc-v 'tree-property-set configuration)))))
(define (column-set configuration)
  (list
   (cons "id" (lambda (x) (cat (if (task-done x)
                             "x "
                             "") (num (task-id x)))))
   (cons "priority" (cut colour-priority configuration <>))

   (cons "task" (lambda (task)
                  (if (task-done task)
                      (fmt-unicode (fmt-green (dsp (task-text task))))
                      (task-text task))))
   (cons "projects"
         (lambda (task) (fmt-join dsp (task-project task) ", ")))
   (cons "contexts" (lambda (task) (fmt-join dsp (task-context task) ", ")))
   (cons "properties" (lambda (task)
                        (fmt-join dsp (map (lambda (property)
                                             (cat ((if (and (equal? (car property) 'due) (date? (cdr property)))
                                                       ;; If the property is "due" and the due date is due soon, colour it.
                                                       (colour-days-out configuration (cdr property))
                                                       ;; Otherwise leave as is
                                                       identity) (car property)) ":" (property-value->string (cdr property)))) (task-property task)) ", ")))
   )
  )
(define (capitalize str)
  (list->string (b:bind (c . rest) (string->list str)
                        (cons (char-upcase c) rest))))
(define (print-tasks-as-table cfg tasks)
  ;; Print tasks in table form, with each column growing as required
  (fmt #t (fmt-unicode
           (apply tabular (append '("| ")
                                  (butlast (foldr (lambda (column-name acc)
                                                    (let [(column-formatter (assoc column-name (column-set cfg)))]
                                                      (if column-formatter
                                                          (append acc (list (column (capitalize (car column-formatter)) (cdr column-formatter) tasks) " | "))
                                                          acc))) '() (reverse (assoc-v 'columns cfg))))
                                  '(" |"))))))
(define (style-lookup list-style)
  ;; return the task printing function associated with the list-style
  (assoc-v list-style
           (list (cons "table"  print-tasks-as-table)
                 (cons "tree" print-tasks-as-tree)
                 (cons "highlighted"  print-tasks-as-highlighted))
           ;; If the user has specified an unknown task printing scheme, default to the table output
           default: print-tasks-as-table))
(define (print-tasks configuration tasks)
  ;; Use the appropriate user set task printing function to print a list of tasks
  (let [(formatter (style-lookup (assoc-v 'list-style configuration)))]
    (formatter configuration tasks)))
(define (explain-bindings command arguments)
  ;; Return an auto-generated documentation for a given subcommand, when supplied the arguments in the form of bindings
  ;; E.g (explain-bindings "replace" (id . todo)) -> "replace <id> [todo...]"
  ;; In this syntax, <a> denoted a compulsory argument, [b...] is an optional argument that takes the remainder of the arguments supplied, and [ignored argument] is an ignored argument
  ;; The binding list is recursively iterated over with each binding mapped according to it's semantic meaning
  ;; All "_" bindings should be reported as an ignored argument
  ;; In the case that a binding follows the form (a . b) a is reported as a required argument, and b an optional argument taking the remainder of arguments supplied
  ;; E.g (a . b) -> "<a> [b...]"
  ;; In all other cases the binding is reported as a compulsory argument
  (let loop ((bindings (if (list? arguments) ;; Reduce the number of cases to consider in the cond statement by ensuring bindings is always a list
                           arguments
                           (list arguments)))
             (help command))
    (cond ((null-list? bindings) help)
          (#t (b:bind (cur-binding . rest-bindings) bindings
                      (cond
                       ((pair? cur-binding) (fmt #f help (loop (list (car cur-binding)) "") " [" (cdr cur-binding) "...]"))
                       ((equal? cur-binding '_) (loop rest-bindings (string-append help " [ignored argument]")))
                       (#t (loop rest-bindings (fmt #f help " <" cur-binding ">") ))))))))
(define-syntax define-opt
  (ir-macro-transformer
   (lambda (expr inject compare)
     ;; For some reason I cannot use a bind statement in a macro so this ugly hack is how I go about binding expression properly
     (let* [(expr (cdr expr))
            (argv (car expr))
            (expr (cdr expr))
            (action-names (car expr))
            (expr (cdr expr))
            (action-opts (cons '(args:make-option (help) #:none "See this help message") (car expr)))
            (expr (cdr expr))
            (action-arguments (car expr))
            (expr (cdr expr))
            (help-message (car expr))
            (body (cdr expr))]
       `(cons (list ,@action-names) (lambda ()
                                      (receive (,(inject 'options) ,(inject 'operands)) (args:parse (cdr ,argv) (list ,@action-opts) #:unrecognized-proc args:ignore-unrecognized-options)
                                        (cond
                                         [(alist-ref 'help ,(inject 'options)) (fmt #t  ,help-message nl (explain-bindings (car ,argv) ',action-arguments) nl (args:usage (list ,@action-opts)))]
                                         [(and (member (car ,argv) ',action-names) ((b:bindable? ,action-arguments) ,(inject 'operands)))
                                          (b:bind ,action-arguments ,(inject 'operands)
                                                  ,@body)]
                                         [#t (print "Invalid argument count for " (car ,argv))]))))))))
(define-syntax define-options
  (ir-macro-transformer
   (lambda (e i c)
     (let* [(e (cdr e))
            (argv (car e))
            (forms (map (lambda (form) (append (list (i 'define-opt) argv) form)) (cdr e)))]
       `(begin
          (let ((options (list ,@forms)))
            (if (or (null-list? ,argv) (equal? (car ,argv) "subcommands"))
                (fmt #t "Run todo <subcommand> --help to get a detailed explanation on it's usage." nl "Subcommands:" nl (fmt-join (lambda (subcommand-names) (cat (string-join subcommand-names ", ") nl))
                                                                                                                                   (map car options)))
                ((cdr (find (lambda (kv) (member (car ,argv) (car kv))) options))))))))))
(define (task-at tasks id)
  (find (lambda (task) (= (task-id task) id)) tasks))
(define (with-task-at-id tasks id thunk)
  ;; Map over tasks, applying thunk to the task whose id matches the one selected.
  (map (lambda (task) (if (= (task-id task) id) ;; If task id matches
                          (thunk task)
                          task)) tasks))
(define (with-tasks-at-ids tasks ids thunk)
  ;; Same as tasks-at-ids, but works with multiple ids
  (foldr (lambda (id tasks) (with-task-at-id tasks id thunk)) tasks ids))
(define (with-tasks-at-selector tasks selector thunk)
  (map (lambda (task)
           (if (selector task)
               (thunk task)
               task)) tasks))
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
            (and dir (file-exists? (string-append dir "todo.txt")) (file-exists? (string-append dir "done.txt")))) directories)))
(define (maybe-cons el lst tst)
  (if (tst el)
      (cons el lst)
      lst))
(define (run args)
  ;; args is a list of strings contained the arguments passed to the executable
  ;; e.g '("pri" "2" "A")
  (let* (;; Configuration
         (configuration (append (or (parse-config-file (get-environment-variable "TODO_CONFIG")) '()) configuration))
         ;; The first item in the list of arguments is always the action
         (action (or (= (length args) 0) (car args)))
         ;; Get an applicable todo directory to source the todo.txt and done.txt file from
         (todo-dir (or (get-applicable-todo-directory (assoc-v 'todo-dir configuration default: #f)
                                                      (get-environment-variable "TODO_DIR")) (begin (print (assoc-v 'todo-dir configuration default: #f))
                                                                                                    (exit -1))))
         ;; Path representation of the todo.txt file
         (todo-file (string-append todo-dir "todo.txt"))
         ;; The parsed todo.txt file
         (tasks (parse-filename todo-file))
         ;; Path representation of the done.txt file
         (done-file (string-append todo-dir "done.txt"))
         ;; The parsed done.txt file
         (done-tasks (parse-filename done-file)))
    (if (and tasks done-tasks)
        (let ((done-tasks (map (lambda (task) (update-task task id: (+ (length tasks) (task-id task)))) done-tasks)))
(define-options args
  [("list" "ls" "listall") ((args:make-option (style) (required: "STYLE") "set the listing style")
                            (args:make-option (by-property) (required: "PROPERTY") "Make tree based on PROPERTY (e.g context, project)")) action-args "List todo items based on an optional filter (action-args)."
           (let ((task-count (+ (length tasks) (if (equal? action "listall") ;; If the user is trying to list done tasks, they should be included in the count.
                                                   (length done-tasks)
                                                   0)))
                 ;; Tasks are filtered using the standard task filter, and then sorted by their priority
                 (tasks (sort (filter (standard-task-filter (string-join action-args " ") (equal? action "listall"))
                                      (append tasks done-tasks)) (apply sort-by (assoc-v 'sorting configuration))))
                 (configuration (foldr (cut maybe-cons <> <> (o identity cdr))
                                       configuration
                                       (list (cons 'list-style (alist-ref 'style options)) (cons 'tree-property (alist-ref 'by-property options))))))
             (print-tasks configuration tasks)
             (fmt #t
                  "---" nl
                  (length tasks) " out of " task-count " task" (if (= task-count 1)
                                                                   ""
                                                                   "s") " shown." nl))]
          [("next") ((args:make-option (highlighted) #:none "highlight next action")) action-args "Select the next most urgent task matching an optional filter (action-args)."
           (let [(tasks (sort (filter (standard-task-filter (string-join action-args " " ) #f) tasks) (apply sort-by (assoc-v 'sorting configuration))))]
             (if tasks
                 ;; Pop off the top task (because of the sorting this is also the highest priority), and print it in text form.
                 (if (alist-ref 'highlighted options)
                     (fmt #t (print-task-as-highlighted configuration (car tasks)) nl)
                     (print (task->string (car tasks))))
                 (print "No tasks to do next.")))]
          [("edit") () _ "Open your todo file in $EDITOR"
           (edit todo-file)]
          (("inbox" "in") ((args:make-option (style) (required: "STYLE") "set the listing style")) action-args "View task inbox with optional filter (action-args)."
           ;; Similar to ls and next, but the standard task filter is replaced with one that simply filters by tasks that are marked as inbox items
           (let [(tasks (filter task-inbox tasks))
                 (configuration (if (alist-ref 'style options)
                                    (cons (cons 'list-style (alist-ref 'style options)) configuration)
                                    configuration))]
             (print-tasks configuration tasks)))
          (("refile") () (ids) "Refile (remove inbox item status of) task at id."
           ;; Overwrite the existing todo file, where the task at id is changed such that it no longer has an inbox status
           (let [(ids (as-ids ids))]
             (if (valid-ids ids)
                 (overwrite-file todo-file (format-tasks-as-file (with-tasks-at-ids tasks ids
                                                                                    (cut update-task <> inbox: #f
                                                                                         date: (current-date)
                                                                                         ))))
                 (invalid-id-err id))))
          (("listproj" "lsprj") () _ "List all projects in task list."
           ;; List all the projects that are present in any task on the todo list
           (print-application (list-unique-properties tasks task-project) identity "\n"))
          (("listcon" "lsc") () _ "List all contexts in task list."
           ;; List all the contexts that are present in any task on the todo list
           (print-application (list-unique-properties tasks task-context) identity "\n"))
          (("rm" "del") () (qualifier) "Remove task(s) at id(s)."
           ;; Delete id or ids in todo list, overwriting the original todo file
           (let ((selector (parse selector qualifier)))
             (overwrite-file todo-file (format-tasks-as-file
                                        (remove selector tasks)))))
          (("revive") () (qualifier) "Unmark a completed task as completed."
           (let ((selector (parse selector qualifier)))
             (let ((selected-tasks (filter selector done-tasks)))
               (overwrite-file done-file (format-tasks-as-file
                                          (remove (cut member <> selected-tasks)  done-tasks)))
               (write-to-a-file todo-file (format-tasks-as-file (map (cut update-task <> done: #f completed-date: #f) selected-tasks))))))
          (("replace") () (id . todo) "Replace task at id with todo."
           ;; Replace the todo at id with new text todo, overwriting the original todo file. Equivalent to todo rm and then todo add.
           (let ((id (string->number id)))
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
                 (invalid-id-err id))))
          (("property-modify" "pmod" "pm") () (id key value) "Modify the value of property whose key is key, of task at id."
           (let ((id (string->number id))
                 (key (string->symbol key)))
             (cond
              [(not (valid-property-value value)) (err "Invalid property value" value)]
              [(not id) (invalid-id-err id)]
              [#t (overwrite-file todo-file (format-tasks-as-file (with-task-at-id tasks id
                                                                                   (lambda (t)
                                                                                     (update-task t
                                                                                                  ;; the property is first removed from the property alist, and then consed to the front.
                                                                                                  property: (cons (cons key value) (rm-prop key (task-property t))))))))])))
          (("property-remove" "prm" "pr") () (id key) "Remove property whose key is key from task at id."
           (let ((id (string->number id))
                 (key (string->symbol key)))
             (if id
                 ;; Remove an property of a todo
                 (overwrite-file todo-file (format-tasks-as-file (with-task-at-id tasks id
                                                                                  (lambda (t)
                                                                                    (update-task t
                                                                                                 property: (rm-prop key (task-property t)))))))
                 (invalid-id-err id))))

          (("add" "a") () action-args "Add task."
           ;; Simple append text passed as the arguments to a file.
           (write-to-a-file todo-file (task->string
                                       (update-task (new-task)
                                                    date: (current-date)
                                                    text: (string-join action-args " ")))))
          (("capture" "c") () action-args "Capture (add to inbox) a task."
           ;; Capture a todo as an inbox item directly
           (write-to-a-file todo-file (task->string
                                       (update-task (new-task)
                                                   inbox: #t
                                                   date: (current-date)
                                                   text: (string-join action-args " ")))))

          (("cat" "cat-all") () _ "Dumb print all tasks."
           ;; Print the raw text contents of either the todo.txt file, or done.txt file
           (if (equal? action "cat")
               (print (read-all todo-file))
               (print (read-all todo-file) "\n" (read-all done-file))))
          (("open" "o") () (id) "Open an attachment for task at id."
           ;; Open the attachments of a todo at id, prompting for file selection if necessary
           (let* [(id (string->number id))
                  ;; Get selected task
                  (selected-task (find (lambda (t) (equal? id (task-id t))) tasks))
                  ;; The attachments are stored in the attach property of the task
                  (attachments (if selected-task (assoc-v 'attach (task-property selected-task)) #f))]
             (cond
              ;; Invalid id or id is out of bounds
              [(or (not id) (not selected-task)) (invalid-id-err id)]
              ;; Attachments is a garbage value, i.e not a list of strings or not a string
              [(not (or (string? attachments) (and (list? attachments) (every string? attachments)))) (fmt #t (fmt-bold (fmt-red "Invalid attachment(s): ")) (property-value->string attachments) nl)]
              ;; If only one attachment, assume the user intends to open it and don't bug them
              [(string? attachments) (open attachments)]
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
          (("done" "do" "mark" "complete" "tick") () (qualifier) "Mark task as done."
           (let ((selector (parse selector qualifier)))
             (begin
               ;; Append marked tasks to done file
               (write-to-a-file done-file (format-tasks-as-file
                                           (remove
                                            ;; Remove all tasks that aren't done, thus isolated the newly updated tasks
                                            (complement task-done)
                                            ;; Update tasklist, marking the selected ids as done.
                                            (with-tasks-at-selector tasks selector (lambda (t) (update-task t done: #t inbox: #f
                                                                                                            date: (or (task-date t) (current-date))
                                                                                                            completed-date: (current-date)))))))
               ;; Overwrite the original todo file, removing the newly marked todo items unless a recur property prevents them
               (overwrite-file todo-file (format-tasks-as-file
                                          ;; Schedule a new todo item in the future for all todos at ids that have a recur property
                                          (with-tasks-at-selector
                                           (remove (lambda (task)
                                                     ;; Remove only task from the todo list only if
                                                     ;; That task is one of the selected tasks from the user
                                                     ;; That task also does not have a recur property.
                                                     (and (selector task)
                                                          (not (assoc 'recur (task-property task))))) tasks)
                                           selector
                                           (lambda (t)
                                             ;; If the task has a due date and a recur property
                                             (if (and (assoc 'recur (task-property t)) (assoc 'due (task-property t)) (date? (assoc-v 'due (task-property t))) (time? (assoc-v 'recur (task-property t))))
                                                 ;; Add the value of the recur property to the due date of the task and return that new task
                                                 (begin
                                                   (fmt #t (fmt-unicode (fmt-bold "Task is recurrent, adding another in the future") nl) )
                                                   (task-due-add t (assoc-v 'recur (task-property t))))
                                                 ;; Otherwise just return the same task untouched
                                                 t))))))))
          (("recently") ((args:make-option (duration d) (required: "DRTN") "set the duration that counts as recent")
                         (args:make-option (style) (required: "STYLE") "set the style to print recent tasks in")) _ "View recently completed tasks."
           (let* [(duration (if (or (not (alist-ref 'duration options)) (not (parse duration (alist-ref 'duration options))))
                                (make-duration days: 7)
                                (parse duration (alist-ref 'duration options))))
                  (tasks (filter (lambda (task)
                                   (and (task-completed-date task) (< (time->days (date-cmp-now (task-completed-date task))) (time->days duration)))) done-tasks))
                  (configuration (if (alist-ref 'style options)
                                     (cons (cons 'list-style (alist-ref 'style options)) configuration)
                                     configuration))]
             (fmt #t "Showing completed tasks for the last " (or (alist-ref 'duration options) "week") "." nl)
             (print-tasks configuration tasks)))
          (("stagnant" "old" "stuck") ((args:make-option (duration d) (required: "DRTN") "set the duration that counts as recent")
                                       (args:make-option (style) (required: "STYLE") "set the style to print recent tasks in")) _ "View old unfinished tasks"
           (let* [(duration (if (or (not (alist-ref 'duration options)) (not (parse duration (alist-ref 'duration options))))
                                (make-duration days: 7)
                                (parse duration (alist-ref 'duration options))))
                  (tasks (filter (lambda (task)
                                   (and (task-date task)
                                        (> (time->days (date-cmp-now (task-date task))) (time->days duration)))) tasks))
                  (configuration (if (alist-ref 'style options)
                                     (cons (cons 'list-style (alist-ref 'style options)) configuration)
                                     configuration))]
             (fmt #t "Showing tasks older than " (or (alist-ref 'duration options) "a week") "." nl)
             (print-tasks configuration tasks)))
          (("bump" "promote") () (qualifier) "Bump (raise task priority by one) task."
           (let ((selector (parse selector qualifier)))
             ;; Cycle the priority of the tasks upwards with ids in ids, giving them a priority of A if they don't already have one
             (overwrite-file todo-file (format-tasks-as-file (with-tasks-at-selector tasks selector (cut cycle-priority <> - #\A))))))
          (("curb" "demote") () (qualifier) "Curb (lower task priority by one) task."
           (let ((selector (parse selector qualifier)))
             ;; Cycle the priority of the tasks downwards with ids in ids, giving them a priority of Z if they don't already have one
             (overwrite-file todo-file (format-tasks-as-file (with-tasks-at-selector tasks selector (cut cycle-priority <> + #\Z))))))
          (("add-context" "ac") () (qualifier context) "Add context to a task."
           (let ((selector (parse selector qualifier)))
             ;; Add a context to a todo item
             (overwrite-file todo-file (format-tasks-as-file (with-tasks-at-selector tasks selector
                                                                                (cut add-to-todo <> 'context task-context context))))))
          (("rm-context" "rc") () (qualifier context) "Remove a context from a task."
           (let ((selector (parse selector qualifier)))
             ;; Remove a context to a todo item
             (overwrite-file todo-file (format-tasks-as-file (with-tasks-at-selector tasks selector
                                                                                     (cut remove-from-todo <> 'context task-context context))))))

           (("add-project" "ap") () (qualifier project) "Add a project to a task."
            (let ((selector (parse selector qualifier)))
              ;; Add a project to a todo item
              (overwrite-file todo-file (format-tasks-as-file (with-tasks-at-selector tasks selector
                                                                                      (cut add-to-todo <> 'project task-project project))))))
           (("rm-project" "rp") () (qualifier project) "Remove a project from task."
            (let ((selector (parse selector qualifier)))
              ;; Remove a project to a todo item
              (overwrite-file todo-file (format-tasks-as-file (with-tasks-at-selector tasks selector
                                                                                 (cut remove-from-todo <> 'project task-project project))))))
           (("log") () action-args "Log (add a task marked already as completed) a task."
            ;; Add a todo to the done file, marked as done (logging that you've done something so to speak).
            (write-to-a-file done-file (string-append "x " (string-join action-args " "))))
           (("pri") () (qualifier new-priority) "Set the priority of a task."
            ;; Set the priority of a todo item
            (let ((selector (parse selector qualifier)))
              (if (or (equal? new-priority "-") (irregex-match "[A-Z]" (format #f "~a" new-priority)))
                  ;; Update the priority of the todo item
                  ;; New priority can either be an uppercase character, or "-" which resets the priority.
                  (overwrite-file todo-file (format-tasks-as-file (with-tasks-at-selector tasks selector
                                                                                          (cut update-task <> priority: (if (equal? new-priority "-")
                                                                                                                            #f ;; false equates to no priority internally
                                                                                                                            new-priority)))))
                  ;; Let the user know that it is an invalid priority
                  (err "Invalid Priority" new-priority))))))
        (err "Todo file invalid: " (cat "Todo file at " todo-dir " is missing, damaged, or otherwise unreadable.")))))
(let [(args (argv))]
  (run (or (parse link (string-join (cdr args) " ")) (cdr args))))
