(declare (unit config))
(declare (uses parse))
(require-extension comparse fmt fmt-color )
(use comparse fmt fmt-color utils)
(define space
  ;; space aliases the char-set:whitespace variable
  (char-set-difference char-set:whitespace (->char-set "\r\n")))
(define space+newline
  char-set:whitespace)
(define non-mandatory-whitespace
  (zero-or-more (in space)))
(define non-mandatory-whitespace+newline
  (zero-or-more (in space+newline)))
(define key-charset
  (char-set-difference char-set:graphic (->char-set "=")))
(define key
  (as-string (repeated (in key-charset))))
(define (text #!key (subset ""))
  (as-string (zero-or-more (in (char-set-difference char-set:printing (->char-set subset))))))
(define string-literal
  (enclosed-by (is #\") (text subset: "\"\r\n") (is #\")))
(define (as-list p)
  (bind p (o result list)))
(define (array value)
  (sequence* ((_ (is #\[))
              (_ non-mandatory-whitespace+newline)
              (a (any-of (list-of value sep: (sequence non-mandatory-whitespace+newline (is #\,) non-mandatory-whitespace+newline)) (as-list value)))
              (_ non-mandatory-whitespace+newline)
              (_ (is #\])))
             (result a)))
(define character
  (enclosed-by (is #\') (in (char-set-difference char-set:printing (->char-set "'"))) (is #\')))
(define colour-set

  (list (cons "green" fmt-green)
        (cons "red" fmt-red)
        (cons "blue" fmt-blue)
        (cons "cyan" fmt-cyan)
        (cons "yellow" fmt-yellow)
        (cons "magenta" fmt-magenta)
        (cons "white" fmt-white)
        (cons "black" fmt-black)))
(define attribute-set
  (list (cons "bold-" fmt-bold)
        (cons "underline-" fmt-underline)))
(define colours
  (sequence* [(attr (maybe (apply any-of (map (o char-seq car) attribute-set))))
              (colour (apply any-of (map (o char-seq car) colour-set)))]
             (let [(colour (assoc-v colour colour-set default: #f))
                   (attr-v (assoc-v attr attribute-set default: #f))]
               (cond
                [(not colour) fail]
                [(and attr (not (assoc-v attr-v attribute-set))) fail]
                [#t (result (o (or attr-v dsp) colour))]))))
(define colour
  (preceded-by (is #\:) colours))
(define value
  (recursive-parser
   (any-of
    duration
    date
    number
    character
    colour
    (array value)
    string-literal)))
(define comment
  (skip (sequence (is #\#) non-mandatory-whitespace (zero-or-more (in (char-set-difference char-set:printing (->char-set "\r\n")))))))
(define line
  (sequence* [(_ non-mandatory-whitespace+newline)
              (key (as-symbol key))
              (_ non-mandatory-whitespace)
              (_ (is #\=))
              (_ non-mandatory-whitespace)
              (value value)
              (_ non-mandatory-whitespace)
              (_ (maybe comment))
              (_ non-mandatory-whitespace+newline)
              ]
             (result (cons key value))))
(define newline-char
  (char-seq "\n"))
(define lines
  (bind (repeated (any-of comment line) until: end-of-input)
        (lambda (res)
          (result (filter (complement (cut equal? #t <>)) res)))))
(define section-header
  (as-symbol (enclosed-by (is #\[) (text subset: "\r\n[]") (is #\]))))
(define section
  (sequence* [(name section-header) (_ newline-char) (pairs lines) ]
             (result (cons name (list (filter (complement (cut equal? <> #t)) pairs))))))
(define document
  (repeated section until: end-of-input))
(define (parse-config-file file)
  (if (and (string? file) (file-exists? file))
      (parse lines (read-all file))
      #f))
