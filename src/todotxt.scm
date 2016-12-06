(declare (unit todotxt))
(declare (uses todotxt-utils))
(require-extension defstruct comparse srfi-19-date srfi-19-time fmt numbers srfi-19-io)
(use comparse defstruct utils srfi-14 srfi-19-date srfi-19-time fmt numbers srfi-19-io)
(defstruct task
  ;; (A) 2011-03-02 Call Mum +family @phone
  ;; x Do this really important thing
  inbox id done completed-date date priority text project context property)
;; Todo.txt regexes
(define (new-task)
  (make-task
   text: ""
   project: '()
   context: '()
   property: '()))
(define space
  char-set:whitespace)
(define -space
  (char-set-difference char-set:graphic char-set:whitespace))
(define legal-text
  (as-string (repeated (in -space))))
(define digit
  (in char-set:digit))
(define (as-number c)
  (bind (as-string c)
        (o result string->number)))
(define (digits n)
  (as-number (repeated digit n)))
(define dash
  (char-seq "-"))
(define date
  (sequence* ((y (digits 4)) (_ dash) (m (digits 2)) (_ dash) (d (digits 2)))
             (result (list y m d))))
(define completed
  (bind (char-seq "x ")
        (lambda (_) (result (lambda (t) (update-task t done: #t))))))
(define inbox
  (bind (char-seq "* ")
        (lambda (_) (result (lambda (t) (update-task t inbox: #t))))))
(define whitespace
  (as-string (one-or-more (in space))))
(define non-mandatory-whitespace
  (as-string (zero-or-more (in space))))
(define done
  (sequence completed  (maybe (bind date
                                    (lambda (date)
                                      (result (lambda (t) (update-task t
                                                                       completed-date: date))))))))
(define priority-char
  (bind (char-seq-match "[A-Z]")
        (lambda (str)
          (result (lambda (t)
                    (update-task t
                                 priority: (car (string->list str))))))))
(define priority
  (enclosed-by (is #\() priority-char (char-seq ") ")))
(define (denoted-by p)
  (preceded-by p legal-text))
(define context
  (bind (denoted-by (is #\@))
        (lambda (context)
          (result (lambda (t)
                    (update-task t
                     context: (cons context (task-context t))))))))
(define project
  (bind (denoted-by (is #\+))
        (lambda (project)
          (result (lambda (t)
                    (update-task t
                      project: (cons project (task-project t))))))))
(define property-text
  (as-string (repeated (in (char-set-difference char-set:graphic (->char-set " :"))))))
(define property
  (sequence* ((k property-text) (_ (char-seq ":")) (v legal-text))
             (result (lambda (t) (update-task t
                                              property: (cons (cons k v) (task-property t)))))))
(define text
  (bind (none-of* property context project legal-text)
        (lambda (res)
          (result (lambda (t)
                    (update-task t
                                 text: (string-trim-right (string-append res " " (task-text t)))))))))
(define generic-section
  (any-of property context project text))
(define todo
  (sequence* ((t generic-section) (_ non-mandatory-whitespace))
             (result t)))
(define task
  (sequence* ((inbox (maybe inbox))
              (d (maybe done))
              (_ (maybe whitespace))
              (p (maybe priority))
              (start-date (maybe (bind (sequence* ((d date)
                                             (_ whitespace))
                                                  (result d))
                                       (lambda (date)
                                         (result (lambda (t) (update-task t
                                                              date: date)))))))
              (t* (repeated todo until: end-of-input)))
             (let [(fns (weed (flatten (list inbox d p start-date t*))))]
               (result (foldr (cut <> <>) (new-task) fns)))))
(define (date->str date)
  (fmt #f (pad-char #\0 (num (car date)) "-" (pad/left 2 (num (cadr date))) "-" (pad/left 2 (num (caddr date))))))
(define (time->days time)
  (/ (time->seconds time) 86400))
(define (tdate->date date-obj)
  (if date-obj
      (make-date 0 0 0 0 (caddr date-obj) (cadr date-obj) (car date-obj))
      #f))
(define (date->datestr date-obj)
  (format-date "~Y-~m-~d" date-obj))

(define (date-soon date-str)
  ;; Note that date at this point is a string
  (let [(date (parse date date-str))]
    (if date
     (let [(now (current-date))
           (date (tdate->date date))]
       (or (date>=? now date) (< (abs (time->days (date-difference now date))) 3)))
     #f)))
(define (task-due-add task days)
  (let [(task-date (tdate->date (cdr (parse date
                                            (assoc-v "due" (task-property task))))))
        (today (current-date))]
    (if task-date
        (update-task task
                     property: (cons (cons "due" (date->datestr
                                                  (date-add-duration (if (date>? today task-date)
                                                                         today
                                                                         task-date)
                                                                     (make-duration days: days))))
                                     (rm-prop "due" (task-property task))))
        task)))
(define (task-priority<? a b)
  (cond
   ((and (task-priority a) (task-priority b)) (char<=? (task-priority a) (task-priority b)))
   ((not (task-priority a)) #f)
   ((not (task-priority b)) #t)))
(define (task->string task)
  (string-join (filter identity
                       (flatten (list
                                 (if (task-inbox task)
                                     "*"
                                     #f)
                                 (if (task-done task)
                                     "x"
                                     #f)
                                 (if (task-priority task)
                                     (format "(~a)" (task-priority task))
                                     #f)
                                 (if (task-completed-date task) (date->str (task-completed-date task)) #f)
                                 (if (task-date task) (date->str (task-date task)) #f)
                                 (task-text task)
                                 (map (o string-concatenate (cut list "+" <>))
                                      (sort (task-project task) string<?))
                                 (map (o string-concatenate (cut list "@" <>))
                                      (sort (task-context task) string<?))
                                 (map (lambda (x) (format #f "~a:~a" (car x) (cdr x)))
                                      (task-property task))))) " "))
(define (parse-filename file)
  (let loop ((lines (filter (lambda (x) (any (complement (cut char-set-contains? char-set:whitespace <>))
                                               (string->list x)))
                            (string-split (read-all file) "\n")))
             (acc '())
             (id 1))
    (if (null? lines)
        (reverse acc)
        (loop (cdr lines) (cons (update-task (parse task (car lines))
                                             id: id) acc) (+ id 1)))))
(define (format-tasks-to-alists tasks)
  (map task->alist tasks))
(define (format-tasks-as-file tasks)
  (string-append (string-join (map task->string tasks) "\n") "\n"))
;; Todo list manipulation
