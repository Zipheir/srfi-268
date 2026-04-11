(define-library (srfi 268)
  (export read-array
          write-array
          )
  (import (scheme base)
	  (scheme case-lambda)
	  (scheme read)
          (scheme write)
          (srfi 231)
          )
  (begin
    ;;; Error reporting

    (define (check-arg who pred x)
      (unless (pred x)
	(error (string-append who ": invalid argument")
	       x)))

    ;;; Unparsing

    ;; Associates the storage classes defined by SRFI 231 with
    ;; the appropriate array tags.
    (define storage-classes
      `((,char-storage-class . char)
        (,s8-storage-class . s8)
	(,s16-storage-class . s16)
	(,s32-storage-class . s32)
	(,s64-storage-class . s64)
	(,u1-storage-class  . u1)
	(,u8-storage-class  . u8)
	(,u16-storage-class . u16)
	(,u32-storage-class . u32)
	(,u64-storage-class . u64)
	(,f8-storage-class  . f8)
	(,f16-storage-class . f16)
	(,f32-storage-class . f32)
	(,f64-storage-class . f64)
	(,c64-storage-class . c64)
	(,c128-storage-class . c128)))

    (define (write-array-to-port array port)
      (check-arg "write-array" array? array)
      (check-arg "write-array" output-port? port)
      (write-tag array port)
      (write-bounds array port)
      (display " " port)
      (write-contents array port))

    (define (write-tag array port)
      (display "#a" port)
      ;; Try to determine the rest of the tag from *array*'s storage
      ;; class.  This is far from foolproof.
      (cond ((assv (array-storage-class array)
		   storage-classes) =>
	      (lambda (p)
		(display (cdr p) port)))
	    ;; Unknown or generic storage class
	    (else #t)))

    (define (write-bounds array port)
      (let* ((iv (array-domain array))
	     (dimension (interval-dimension iv))
	     (write-dim-bounds
	      (lambda (d)
		(display "(" port)
		(display (interval-lower-bound iv d) port)
		(display " " port)
		(display (interval-upper-bound iv d) port)
		(display ")" port))))
	(display "(" port)
	;; Print bounds pair for each dimension of iv.
	(do ((i 0 (+ i 1)))
	    ((= i dimension))
	  (write-dim-bounds i)
	  (when (< i (- dimension 1))
	    (display " " port)))
	(display ")" port)))

    (define (write-contents array port)
      (display (array->list* array) port))

    (define write-array
      (case-lambda
        ((array)
         (write-array-to-port array (current-output-port)))
        ((array port)
         (write-array-to-port array port))))

    ;;; Parsing

    (define (consume-tag-prefix port)
      (let ((c (read-char port)))
        (unless (eqv? c #\#)
          (parsing-error "invalid tag character (expected #)" c)))
      (let ((c (read-char port)))
	(unless (memv c '(#\a #\A))
	  (parsing-error "invalid tag character (expected a or A)" c))))

    (define (parsing-error msg . irritants)
      (apply error
	     (string-append "read-array: " msg)
	     irritants))

    (define (read-array-from-port port)
      (check-arg "read-array" input-port? port)
      (let* ((class (parse-tag port))
	     (bounds (parse-bounds port))
	     (contents (read port)))
	(unless (list? contents)
	  (error "read-array: invalid array contents"
		 contents))
	(build-array (apply make-interval bounds)
		     class
		     contents)))

    ;; Parse an array tag from *port* and return an appropriate
    ;; storage class.
    (define (parse-tag port)
      (consume-tag-prefix port)
      (let ((class-sym (read)))
        (unless (symbol? class-sym)
	  (parsing-error "invalid array tag" class-sym))
        (class-symbol->storage-class class-sym)))

    ;; TODO
    (define (parse-bounds port)
      #f)

    ;; TODO
    (define (class-symbol->storage-class sym)
      #f)

    ;; It would be easier to use list*->array to build the new
    ;; array here, but we need finer-grained control over the
    ;; interval.
    (define (build-array interval storage-class contents)
      (let* ((A (make-specialized-array interval storage-class))
             (set-entry! (array-setter A)))
	;; TODO: set all entries from *contents*
	A))

    (define read-array
      (case-lambda
        (() (read-array-from-port (current-input-port)))
	((port) (read-array-from-port port))))

  ))
