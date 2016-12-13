(declare (unit todotxt))
(declare (uses todotxt-utils))
(require-extension defstruct comparse srfi-19-date srfi-19-time fmt numbers srfi-19-io srfi-19-support)
(use comparse defstruct utils srfi-14 srfi-19-date srfi-19-time fmt numbers srfi-19-io srfi-19-support)
(defstruct task
  ;; (A) 2011-03-02 Call Mum +family @phone
  ;; x Do this really important thing
  inbox id done completed-date date priority text project context property)
(define (leap-year? year)
  ;; Return #t if year is a leap year, #f otherwise
  (cond
   ((> (modulo year 4) 0) #f)
   ((> (modulo year 100) 0) #t)
   ((> (modulo year 400) 0) #f)
   (#t #t)))
(define (valid-day year month day)
  ;; Returns #t if the day is valid for the given month and year
  (case month
    ((2) (<= day (if (leap-year? year) 29 28)))
    ((4 6 9 11) (<= day 30))
    (else (<= day 31))))
(define (new-task)
  ;; Returns a task with the default values filled out
  (make-task
   text: ""
   project: '()
   context: '()
   property: '()))
(define space
  ;; space aliases the char-set:whitespace variable
  char-set:whitespace)
(define -space
  ;; -space defines a charset that is the inverse of whitespace (i.e every character but whitespace characters)
  (char-set-difference char-set:graphic char-set:whitespace))
(define legal-text
  ;; Legal text is define as a repeated string of characters of any type bar whitespace
  (as-string (repeated (in -space))))
(define digit
  ;; Checks if character is in the digit character set
  (in char-set:digit))
(define (as-number c)
  ;; Takes the parser result as a string, and converts that string to a number
  (bind (as-string c)
        (o result string->number)))
(define (digits n)
  ;; Returns n digit characters as an integer
  (as-number (repeated digit n)))
(define dash
  ;; A literal "-"
  (char-seq "-"))
(define date
  ;; date parses a date string, returning the result as an srfi-19 date object
  ;; The grammar for this looks like 2016-03-12, in the YYYY-MM-DD format.
  ;; The day month and year is automatically checked for validity
  (sequence* ((y (digits 4)) (_ dash) (m (digits 2)) (_ dash) (d (digits 2)))
             (if (or (> m 12) (= 0 m) (= 0 d) (not (valid-day y m d)))
                 fail
                 (result (make-date 0 0 0 0 d m y)))))
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
(define whitespace
  ;; whitespace matches one or more whitespace characters
  (as-string (one-or-more (in space))))
(define non-mandatory-whitespace
  ;; non-mandatory-whitespace matches zero or more whitespace characters
  (as-string (zero-or-more (in space))))
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
(define property-text
  ;; Property values are constrained to a simpler subset of legal text, notably omitting ":" and ","
  (as-string (one-or-more (in (char-set-difference char-set:graphic (->char-set " :,"))))))
(define property-value-literal
  ;; A property literal is either a date, and number, or some text
  (any-of date (as-number (repeated digit until: (in space))) property-text))
(define property-list
  ;; Property list is a sequence of property literal values, separated by ",".
  (bind (sequence (one-or-more (sequence* [(list-item property-value-literal) (_ (is #\,))]
                                (result list-item))) property-value-literal)
        (o result flatten)))
(define property-value
  ;; This is the value portion of the key value pair in a property
  ;; It can either be a literal value, or list of literal values
  (any-of property-list property-value-literal))
(define (as-symbol parser)
  ;; Return the parser's result as a symbol
  (bind (as-string parser)
        (o result string->symbol)))
(define property
  ;; A property is a key value pair that describes a property of a task
  ;; An example might be "key:value", "date:2016-12-13", or "recur:1"
  ;; The key is parsed as a symbol, while the value is interpreted as either a date, number, or string
  ;; On success this returns a function that prepends the key and value to the task property alist.
  (sequence* ((k (as-symbol property-text)) (_ (char-seq ":")) (v property-value))
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
  (sequence* ((t generic-section) (_ non-mandatory-whitespace))
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
  (sequence* ((inbox (maybe inbox))
              (d (maybe done))
              (_ (maybe whitespace))
              (p (maybe priority))
              (start-date (maybe (bind (sequence* ((d date)
                                                   (_ whitespace))
                                                  (result d))
                                       (lambda (date)
                                         (result (cut update-task <> date: date))))))
              (t* (repeated todo until: end-of-input)))
             (let [(fns (weed (flatten (list inbox d p start-date t*))))]
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
        (time->days (date-difference now date)))))
(define (task-due-add task days)
  ;; Add days to the task's due date, or if the due date is prior to the current date, to the current date
  (let [(task-date (assoc-v 'due (task-property task)))
        (today (current-date))]
    (if task-date
        (update-task task
                     property: (cons (cons 'due (date->str
                                                  (date-add-duration (if (date>? today task-date)
                                                                         today
                                                                         task-date)
                                                                     (make-duration days: days))))
                                     (rm-prop 'due (task-property task))))
        task)))
(define (task-priority<? a b)
  ;; Compare two tasks by priority
  (cond
   ((and (task-priority a) (task-priority b)) (char<=? (task-priority a) (task-priority b)))
   ((not (task-priority a)) #f)
   ((not (task-priority b)) #t)))
(define (property-value->string value)
  ;; Convert a property value into a string by matching all possible types.
  ;; Property values can either be a list, date, string, or number
  (cond
   [(list? value) (fmt-join (o dsp property-value->string) value ",")]
   [(date? value) (date->str value)]
   ;; This case handles numbers as well (->string works on many types besides)
   [#t (->string value)]))
(define (task->string task)
  ;; Convert a task to a string by iterating over the task's properties and converting each to a string
  (fmt #f
       (if (task-inbox task)
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
       (task-text task)
       (fmt-join (cut cat " +" <>) (sort (task-project task) string<?))
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
;; Todo list manipulation
