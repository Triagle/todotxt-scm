(declare (unit todotxt))
(declare (uses todotxt-utils parse))
(require-extension defstruct comparse srfi-19-date srfi-19-time fmt numbers srfi-19-io srfi-19-support)
(use comparse defstruct utils srfi-14 srfi-19-date srfi-19-time fmt numbers srfi-19-io srfi-19-support)
(defstruct task
  ;; (A) 2011-03-02 Call Mum +family @phone
  ;; x Do this really important thing
  inbox id done completed-date date priority text project context property)
(define (new-task)
  ;; Returns a task with the default values filled out
  (make-task
   text: ""
   project: '()
   context: '()
   property: '()))
(define legal-text
  ;; Legal text is define as a repeated string of characters of any type bar whitespace
  (as-string (repeated (in -space))))
(define completed
  ;; This indicates that a task is a completed item, matching "x "
  ;; It returns a function that toggles a passed task's completed state to true
  (bind (char-seq "x ")
        (lambda (_) (result (cut update-task <> done: #t)))))
(define inbox
  ;; This indicates that a task is an inbox item, matching "* "
  ;; It returns a function that toggles a passed task's inbox state to true
  (bind (char-seq "* ")
        (lambda (_) (result (cut update-task <> inbox: #t)))))
(define completed-date
  ;; Completed date matches a date string and then returns a function updates the value of the completed date in a passed task to the parsed results
  (bind date
        (lambda (date)
          (result (cut update-task <> completed-date: date)))))
(define done
  ;; done is a section that marks a task as done
  ;; It is made up of two parts, the actual completion indicator (completed), and the completion date (completion-date).
  ;; The grammar looks like "x [2016-03-11]" where the completed date is optional but must always follow the completion indicator if included
  (sequence completed (maybe completed-date)))
(define priority-char
  ;; priority-char matches a single uppercase letter, returning a function that updates the priority of the passed task
  ;; To the value of that uppercase letter as a character
  (bind (char-seq-match "[A-Z]")
        (lambda (str)
          (result (cut update-task <> priority: (car (string->list str)))))))
(define priority
  ;; Priority matches the "([A-Z]) "
  (enclosed-by (is #\() priority-char (char-seq ") ")))
(define (denoted-by p)
  ;; denoted-by takes p and returns the legal-text following p.
  ;; For instance (parse (denoted-by (is #\+)) "+project test") would return "project"
  (preceded-by p legal-text))
(define context
  ;; A context is any string of legal text prefixed by "@"
  ;; On success this returns a function that updates the task to include the parsed context.
  ;; Note that because of the way the parser is implemented it is fully possible to have contexts with "@" in them like "@shop@car"
  (bind (denoted-by (is #\@))
        (lambda (context)
          (result (lambda (t)
                    (update-task t context: (cons context (task-context t))))))))
(define project
  ;; A project is any string of legal text prefixed by "+"
  ;; On success this returns a function that updates the task to include the parsed project.
  ;; Note that because of the way the parser is implemented it is fully possible to have projects with "+" in them like "+shop+car"
  (bind (denoted-by (is #\+))
        (lambda (project)
          (result (lambda (t)
                    (update-task t
                      project: (cons project (task-project t))))))))
(define property-key-text
  ;; Property values are constrained to a simpler subset of legal text, notably omitting ":" and ","
  (as-string (one-or-more (in (char-set-difference char-set:graphic (->char-set " :"))))))
(define property-value-text
  (as-string (one-or-more (in (char-set-difference char-set:graphic (->char-set " ,"))))))
(define property-value-literal
  ;; A property literal is either a date, and number, or some text
  (any-of date duration number property-value-text))
(define property-list
  (list-of property-value-literal))
(define property-value
  ;; This is the value portion of the key value pair in a property
  ;; It can either be a literal value, or list of literal values
  (any-of property-list property-value-literal))
(define property
  ;; A property is a key value pair that describes a property of a task
  ;; An example might be "key:value", "date:2016-12-13", or "recur:1"
  ;; The key is parsed as a symbol, while the value is interpreted as either a date, number, or string
  ;; On success this returns a function that prepends the key and value to the task property alist.
  (sequence* ((k (as-symbol property-key-text)) (_ (char-seq ":")) (v property-value))
             (result (lambda (t) (update-task t
                                              property: (cons (cons k v) (task-property t)))))))
(define text
  ;; Text is the catch-all definition of a section of a todo
  ;; It includes things like "remember" "to" "get" "the" "milk"
  ;; On successful parsing, this returns a function that takes in a task
  ;; and returns that same task with some more text appended to it
  (bind (none-of* property context project legal-text)
        (lambda (res)
          (result (lambda (t)
                    (update-task t
                                 text: (string-trim-right (string-append res " " (task-text t)))))))))
(define generic-section
  ;; A generic section describes the contents of the todo (excluding priority, done status, and inbox status) as either consisting
  ;; of a property, a context, a project, or some text ordered in terms of how strict the grammar definition of each possibility is.
  ;; Sections are split by " ".
  (any-of property context project text))
(define todo
  ;; todo describes a section and some whitespace that separates it from the next section.
  ;; e.g "hello " would return a function that adds the text "hello" to the task's text.
  (sequence* ((t generic-section) (_ non-mandatory-whitespace+newline))
             (result t)))
(define task
  ;; The outline definition of task
  ;; The syntax of task looks:
  ;; Note: Anything in "[]" is optional, and anything in "()" is required
  ;; [* ] [x YYYY-MM-DD] [(A)] [YYYY-MM-DD] (get milk [+shopping] [@shops] [price:2.00])

  ;; Due to the functional nature of the parser, the result of each distinct parsed section is a function that takes in a task and returns
  ;; That same task with properties changed according to specifics about the section
  ;; For instance the priority section returns a function upon successful parsing that takes in a task and returns that same task with the priority set to
  ;; the parsed priority.

  ;; This functional approach is chosen to make the todo parsing simple (as opposed to the original idea of using alists) and modular in that the parser wouldn't break
  ;; if something suddenly changed, it is simply applying functions and doesn't care about the function's operation.
  (sequence* ((p (maybe priority))
              (state (maybe (any-of
                      inbox
                      done)))
              (_ (maybe whitespace))
              (start-date (maybe (bind (sequence* ((d date)
                                                   (_ whitespace))
                                                  (result d))
                                       (lambda (date)
                                         (result (cut update-task <> date: date))))))
              (t* (repeated todo until: end-of-input)))
             (let [(fns (weed (flatten (list p state start-date t*))))]
               (result (foldr (cut <> <>) (new-task) fns)))))
(define (valid-property-value value)
  ;; return a truthy value if the value passed is considered a valid property value, and #f otherwise
  (parse property-value value))
(define (date->str date)
  ;; Format a date as a string in the format YYYY-MM-DD
  (format-date #f "~Y-~m-~d" date))
(define (time->days time)
  ;; Convert a time interval into an interval of days
  (/ (time->seconds time) 86400))
(define (duration->string duration)
  (let loop ((days (time->days duration))
             (duration-string "")
             (converter '((365 "y") (30 "m") (7 "w") (1 "d"))))
    (if (null-list? converter)
        duration-string
        (let* [(duration-of-days (caar converter))
               (duration-denoter (cadar converter))
               (number-of-duration (floor (/ days duration-of-days)))]
          (if (zero? number-of-duration)
              (loop days duration-string (cdr converter))
              (loop (- days (* duration-of-days number-of-duration)) (string-append duration-string (->string number-of-duration) duration-denoter) (cdr converter)))))))
(define (date-soon date)
  ;; Return a boolean indicating whether a date is due relatively soon
  ;; Overdue date are also considered "soon"
  (if date
      (let [(now (current-date))]
        ;; Soon is defined as 3 days away
        (or (date>=? now date) (< (abs (time->days (date-difference now date))) 3)))
      #f))
(define (date-cmp-now date)
  ;; Return the difference in days between now and date
  (if date
      (let [(now (current-date))]
        (date-difference now date))))
(define (task-due-add task duration #!key (from (current-date)))
  ;; Add days to the task's due date, or if the due date is prior to the current date, to the current date
  (let [(task-date (assoc-v 'due (task-property task)))]
    (if task-date
        (update-task task
                     property: (cons (cons 'due (date->str
                                                  (date-add-duration (if (date>? from task-date)
                                                                         from
                                                                         task-date)
                                                                     duration)))
                                     (rm-prop 'due (task-property task))))
        task)))
(define (task-priority<? a b)
  ;; Compare two tasks by priority
  (cond
   ((equal? (task-priority a) (task-priority b)) 'equal)
   ((and (task-priority a) (task-priority b)) (char<=? (task-priority a) (task-priority b)))
   ((not (task-priority a)) #f)
   ((not (task-priority b)) #t)))
(define (property-value->string value)
  ;; Convert a property value into a string by matching all possible types.
  ;; Property values can either be a list, date, string, or number
  (cond
   [(time? value) (duration->string value)]
   [(list? value) (fmt-join (o dsp property-value->string) value ",")]
   [(date? value) (date->str value)]
   ;; This case handles numbers as well (->string works on many types besides)
   [#t (->string value)]))
(define (task->string task)
  ;; Convert a task to a string by iterating over the task's properties and converting each to a string
  (fmt #f
       (if (task-priority task)
           (cat "(" (task-priority task) ") ")
           "")
       (cond
        [(task-inbox task) "* "]
        [(task-done task) "x "]
        [#t ""])
       (if (task-completed-date task) (cat (date->str (task-completed-date task)) " ") "")
       (if (task-date task) (cat (date->str (task-date task)) " ") "")
       (task-text task)
       (fmt-join (cut cat " +" <>) (task-project task))
       (fmt-join (cut cat " @" <>) (sort (task-context task) string<?))
       (fmt-join (lambda (kv) (cat " " (symbol->string (car kv)) ":" (property-value->string (cdr kv)))) (task-property task))))
(define (parse-filename file)
  ;; Parse file (a todo.txt format), returning a list of all the tasks
  (if (file-exists? file) ;; Don't attempt to parse if file doesn't exist
      ;; A recursive loop is used to attach an id to each task based on it's position in the file.
      (let loop ((lines (filter (lambda (x) (any (complement (cut char-set-contains? char-set:whitespace <>))
                                                 (string->list x))) ;; This weeds out any lines that only contain whitespace characters
                                (string-split (read-all file) "\n")))
                 (acc '())
                 (id 1))
        (if (null? lines)
            (reverse acc)
            (loop (cdr lines) (cons (update-task (parse task (car lines))
                                                 id: id) acc) (+ id 1))))
      #f))
(define (format-tasks-as-file tasks)
  ;; Convert a list of task structs to strings, joining them with newlines so as to be saved to disk
  (string-append (string-join (map task->string tasks) "\n") "\n"))
(define (task-id<? a b)
  (< (task-id a) (task-id b)))
(define (property<? a b)
  ((cond
    [(and (date? a) (date? b)) date<?]
    [(and (number? a) (number? b)) <]
    [(and (time? a) (time? b)) time<?]
    [(and (string? a) (string? b)) string<?]
    [#t (lambda (a b) #f)]) a b))
(define (task-age<? a b)
  (cond
   [(equal? (task-date a) (task-date b)) 'equal]
   [(not (task-date a)) #f]
   [(not (task-date b)) #t]
   [#t (date<? (task-date a) (task-date b))]))
(define (task-property<? a b property)
  (cond
   [(not (assoc property (task-property a))) #f]
   [(not (assoc property (task-property b))) #t]
   [(equal? (assoc-v property (task-property a)) (assoc-v property (task-property b))) 'equal]
   [#t (property<? (assoc-v property (task-property a)) (assoc-v property (task-property b)))])
  )
(define (task-state task)
  (let [(state (assoc-v 'state (task-property task)
                        default: (if (task-done task)
                                     '("complete")
                                     '("todo"))))]
    (map string->symbol (if (list? state) state (list state)))))
(define (task-permid task)
  ;; Return a tasks permanent id
  (assoc-v 'permid (task-property task) default: #f))
(define (task-blocks task)
  ;; Return what other tasks task blocks
  (let [(block-list (assoc-v 'blocks (task-property task) default: '()))]
    (if (list? block-list)
        block-list
        (list block-list))))
(define (task-blocked? task task-list)
  ;; Returns a list of tasks that are blocking the task "task"
  (let [(permid (task-permid task))]
    (if (not permid)
        #f
        (not (null-list? (filter (lambda (t) (member permid (task-blocks t))) task-list))))))
(define (block-task task blocked-by)
  ;; Return the same two tasks, however with the first blocking the second via it's permid (created if needed).
  (let [(permid (or (task-permid task) (gen-uuid)))]
    (list (update-task task
                       property: (cons (cons 'permid permid)
                                       (rm-prop 'permid (task-property task))))
          (update-task blocked-by
                       property: (cons (cons 'blocks (cons permid (task-blocks blocked-by)))
                                       (rm-prop 'permid (task-property task)))))))

;; Todo list manipulation
