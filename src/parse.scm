(declare (unit parse))
(require-extension comparse numbers srfi-19-date srfi-19-time symbol-utils)
(use comparse numbers srfi-19-date srfi-19-time symbol-utils)
;; Defines some commmon types to both the configuration file format and the todo file format
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
(define space
  ;; space aliases the char-set:whitespace variable
  (char-set-difference char-set:whitespace (->char-set "\r\n")))
(define space+newline
  char-set:whitespace)
;; Non mandatory whitespace
(define non-mandatory-whitespace
  (zero-or-more (in space)))
;; Non
(define non-mandatory-whitespace+newline
  (zero-or-more (in space+newline)))
(define whitespace
  ;; whitespace matches one or more whitespace characters
  (as-string (one-or-more (in space))))
(define dash
  ;; A literal "-"
  (char-seq "-"))
(define -space
  ;; -space defines a charset that is the inverse of whitespace (i.e every character but whitespace characters)
  char-set:graphic)
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
(define (duration-modifier n)
  (bind (in (->char-set "dwmy"))
        (lambda (character)
          (result (* (case character
                       ((#\d) 1)
                       ((#\w) 7)
                       ((#\m) 30)
                       ((#\y) 365)) n)))))
(define duration
  (bind (one-or-more (sequence* ((n (as-number (one-or-more digit))) (days (duration-modifier n)))
                                (result days)))
        (lambda (days)
          (result (make-duration days: (reduce + 0 days))))))
(define date
  ;; date parses a date string, returning the result as an srfi-19 date object
  ;; The grammar for this looks like 2016-03-12, in the YYYY-MM-DD format.
  ;; The day month and year is automatically checked for validity
  (sequence* ((y (digits 4)) (_ dash) (m (digits 2)) (_ dash) (d (digits 2)))
             (if (or (> m 12) (= 0 m) (= 0 d) (not (valid-day y m d)))
                 fail
                 (result (make-date 0 0 0 0 d m y)))))
(define (list-of p #!key (sep (char-seq ",")))
  (sequence* [(p* (one-or-more (sequence* [(list-item p) (_ sep)]
                                         (result list-item))))
             (p p)]
             (result (append p* (list p)))))
(define number
  (bind (as-number (one-or-more (in (->char-set "0123456789-+."))))
        (lambda (n)
          (if n
              (result n)
             fail))))
(define (as-symbol parser)
  ;; Return the parser's result as a symbol
  (bind (as-string parser)
        (o result string->symbol)))
