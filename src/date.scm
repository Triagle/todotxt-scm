;; A date parsing alternative to srfi-19
;; Why:
;; Chicken Scheme's regex is *really* slow
;; This is because the code that compiles the regular expressions is much slower with the new irregex approach than the PCRE approach.
;; The srfi-19 module uses regex to parse locale information from environment variables.
;; As a consequence the following code runs in 150ms!
;;
;; (require-extension srfi-19-time)
;; (use srfi-19-time)
;; (print "hello world")
;;
;; Note that I did not call any functions from that package, they are evaluated at (why??!??!??) the load time.
;; Therefore I take an alternative route with a simple c ffi to implement a simpler and faster basic datetime library
;; using only the stuff I need and nothing I don't.
;; NOTE: Don't use this anywhere else :P
(module dates
    (date
     date-year
     date-month
     date-day
     current-date
     date-add)
  (import scheme chicken extras)
  (require-extension comparse defstruct numbers bind)
  (use (prefix bind cb:))
  (cb:bind* "#ifndef CHICKEN\n #include <time.h>\n #endif")
  (cb:bind-opaque-type c_time "struct tm")
  (cb:bind* "struct tm* make_ctime() {time_t now = time(NULL);return localtime(&now);}")
  (cb:bind* "int ctime_year(struct tm* time) {return time->tm_year + 1900;}")
  (cb:bind* "int ctime_month(struct tm* time) {return time->tm_mon + 1;}")
  (cb:bind* "int ctime_day(struct tm* time) {return time->tm_mday;}")
  (cb:bind* "double diff_cdate(struct tm* time1, struct tm* time2) {return difftime(mktime(time1), mktime(time2));}")
  ;; (bind "void time(c_time *)")
  (define (current-date)
    (make_ctime))
  (define (date-year date)
    (ctime_year date))
  (define (date-month date)
    (ctime_month date))
  (define (date-day date)
    (ctime_day date))
  (define (date-diff date1 date2)
    (diff_cdate date1 date2)))
