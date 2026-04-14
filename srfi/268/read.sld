;;; The contents-list parsing code was snarfed from Bradley Lucier's
;;; SRFI 231 sample implementation.
(define-library (srfi 268 read)
  (export read-array transform-bounds)
  (import (scheme base)
	  (scheme case-lambda)
	  (scheme char)
	  (scheme read)
	  (scheme write)
          (prefix (srfi 1) srfi-1:)
          (prefix (srfi 231) srfi-231:)
          )
  (begin
    (define storage-class-symbols
      `((char . ,srfi-231:char-storage-class)
        (s8   . ,srfi-231:s8-storage-class)
	(s16  . ,srfi-231:s16-storage-class)
	(s32  . ,srfi-231:s32-storage-class)
	(s64  . ,srfi-231:s64-storage-class)
	(u1   . ,srfi-231:u1-storage-class)
	(u8   . ,srfi-231:u8-storage-class)
	(u16  . ,srfi-231:u16-storage-class)
	(u32  . ,srfi-231:u32-storage-class)
	(u64  . ,srfi-231:u64-storage-class)
	(f8   . ,srfi-231:f8-storage-class)
	(f16  . ,srfi-231:f16-storage-class)
	(f32  . ,srfi-231:f32-storage-class)
	(f64  . ,srfi-231:f64-storage-class)
	(c64  . ,srfi-231:c64-storage-class)
	(c128 . ,srfi-231:c128-storage-class)))

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
	    (else (parsing-error "invalid array type" sym))))

    (define (parse-tag)
      (consume-tag-prefix)
      (if (char-alphabetic? (peek-char))  ; type present?
          (let ((class-sym (read)))
            (unless (symbol? class-sym)
              (parsing-error "invalid array tag" class-sym))
            (class-symbol->storage-class class-sym))
          srfi-231:generic-storage-class))  ; type elided

    ;; Split *bounds* into two corresponding vectors of lower
    ;; and upper bounds.
    (define (transform-bounds bounds)
      (let-values (((lowers uppers)
		    (srfi-1:unzip2
		     (map (lambda (b)
			    (check-bounds b)
			    (if (integer? b)
				(list 0 b)
				b))
			  bounds))))
	 (values (list->vector lowers) (list->vector uppers))))

    (define (parse-bounds)
      (let ((bounds (read)))
	(unless (list? bounds)
	  (parsing-error "invalid bounds spec" bounds))
        (transform-bounds bounds)))

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
      (let ((dimension (srfi-231:interval-dimension interval)))
	(unless (check-nested-list dimension contents)
	  (parsing-error "contents list is the wrong shape"
			 interval
			 contents))
	(srfi-231:list->array interval
		              (flatten-nested-list
			       (srfi-231:interval-dimension interval)
			       contents)
		     storage-class)))

    (define (flatten-nested-list dimension nested-list)
      (case dimension
	((0) (list nested-list))
	((1) (list-copy nested-list))
	(else
	 (srfi-1:append-map
	  (lambda (l)
	    (flatten-nested-list (- dimension 1) l))
	  nested-list))))

    (define (check-nested-list dimension nested-data)
      (if (eqv? dimension 0)
	  '()
	  (and (list? nested-data)
	       (let ((len (length nested-data)))
		 (cond ((eqv? len 0) '())
		       ((eqv? dimension 1) (list len))
		       (else
			(let* ((sublists
				(map (lambda (l)
				       (check-nested-list
					(- dimension 1)
					l))
				     nested-data))
			       (first (car sublists)))
			  (and first
			       (srfi-1:every
				(lambda (l)
				  (equal? first l))
			 	(cdr sublists))
			       (cons len first)))))))))

    (define read-array
      (case-lambda
	((port)
         (check-arg "read-array" input-port? port)
         (parameterize ((current-input-port port))
           (read-array)))
        (()
         (let*-values (((class) (parse-tag))
                       ((lowers uppers) (parse-bounds))
                       ((contents) (read)))
           (build-array (srfi-231:make-interval lowers uppers)
                        class
                        contents)))))
  ))
