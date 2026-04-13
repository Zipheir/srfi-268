(define-library (srfi 268 read)
  (export read-array
          flatten-contents)
  (import (scheme base)
	  (scheme case-lambda)
	  (scheme read)
          (scheme write)
          (only (srfi 1) append-map)
          (srfi 231)
          )
  (begin
    (define storage-class-symbols
      `((char . ,char-storage-class)
        (s8   . ,s8-storage-class)
	(s16  . ,s16-storage-class)
	(s32  . ,s32-storage-class)
	(s64  . ,s64-storage-class)
	(u1   . ,u1-storage-class)
	(u8   . ,u8-storage-class)
	(u16  . ,u16-storage-class)
	(u32  . ,u32-storage-class)
	(u64  . ,u64-storage-class)
	(f8   . ,f8-storage-class)
	(f16  . ,f16-storage-class)
	(f32  . ,f32-storage-class)
	(f64  . ,f64-storage-class)
	(c64  . ,c64-storage-class)
	(c128 . ,c128-storage-class)))

    (define (check-arg who pred x)
      (unless (pred x)
	(error (string-append who ": invalid argument")
	       x)))

    ;; Read the next char & raise an error if it's not in
    ;; valid-chars.
    (define (consume valid-chars)
      (let ((x (read-char)))
	(cond ((eof-object? x)
	       (parsing-error "unexpected EOF"))
	      ((not (memv x valid-chars))
	       (parsing-error "invalid character" x)))))

    (define (consume-tag-prefix)
      (consume '(#\#))
      (consume '(#\a #\A)))

    (define (parsing-error msg . irritants)
      (apply error
	     (string-append "read-array: " msg)
	     irritants))

    ;; Parse an array tag from *port* and return an appropriate
    ;; storage class.
    (define (class-symbol->storage-class sym)
      (cond ((assv sym storage-class-symbols) => cdr)
	    (else generic-storage-class)))

    (define (parse-tag)
      (consume-tag-prefix)
      (if (eqv? #\( (peek-char))  ; elided type?
          generic-storage-class
          (let ((class-sym (read)))
            (unless (symbol? class-sym)
              (parsing-error "invalid array tag" class-sym))
            (class-symbol->storage-class class-sym))))

    (define (parse-bounds)
      (let ((bounds (read)))
	(unless (list? bounds)
	  (parsing-error "invalid bounds spec" bounds))
	(map (lambda (b)
	       (check-bounds b)
	       (cond ((integer? b) (vector 0 b))  ; upper bound only
		     ((list? b) (list->vector b))))
	     bounds)))

    (define (check-bounds b)
      ;; Just check if *b* has the right type and leave the numerical
      ;; checks to 'make-interval'.
      (unless (or (integer? b)
	          (and (list? b) (= 2 (length b))))
	(parsing-error "invalid bounds spec element" b)))

    ;; It would be easier to use list*->array to build the new
    ;; array here, but we need finer-grained control over the
    ;; interval.
    ;; Thus it seems to be necessary to pad & flatten the nested
    ;; contents and then pass them to list->array (which, unlike
    ;; list*->array, takes an interval).
    (define (build-array interval storage-class contents)
      (list->array interval
                   (flatten-contents (interval-dimension interval)
                                     contents)
                   storage-class))

    (define (flatten ls d)
      (if (and (positive? d) (pair? ls) (pair? (car ls)))
          (append-map (lambda (x) (flatten x (- d 1))) ls)
          ls))

    (define (flatten-contents dimension nested-ls)
      (display "dimension is ")
      (display dimension)
      (newline)
      (let lp ((ls nested-ls) (d dimension))
        (cond
         ((positive? d)
          (if (null? ls)
              (lp '() (- d 1))
              (lp (car ls) (- d 1))))
         (else
          (if (zero? dimension)
              (list nested-ls)
              (flatten nested-ls (- dimension 1)))))))

    (define read-array
      (case-lambda
	((port)
         (check-arg "read-array" input-port? port)
         (parameterize ((current-input-port port))
           (read-array)))
        (()
         (let* ((class (parse-tag))
                (bounds (parse-bounds))
                (contents (read)))
           (unless (list? contents)
             (parsing-error "invalid array contents" contents))
           (build-array (apply make-interval bounds)
                        class
                        contents)))))

  ))
