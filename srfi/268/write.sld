(define-library (srfi 268 write)
  (export write-array)
  (import (scheme base)
	  (scheme case-lambda)
          (scheme write)
          (prefix (srfi 231) srfi-231:)
          )
  (begin
    ;;; Error reporting

    (define (check-arg who pred x)
      (unless (pred x)
	(error (string-append who ": invalid argument")
	       x)))

    ;; Associates the storage classes defined by SRFI 231 with
    ;; the appropriate array tags.
    (define storage-classes
      `((,srfi-231:char-storage-class . char)
        (,srfi-231:s8-storage-class . s8)
	(,srfi-231:s16-storage-class . s16)
	(,srfi-231:s32-storage-class . s32)
	(,srfi-231:s64-storage-class . s64)
	(,srfi-231:u1-storage-class  . u1)
	(,srfi-231:u8-storage-class  . u8)
	(,srfi-231:u16-storage-class . u16)
	(,srfi-231:u32-storage-class . u32)
	(,srfi-231:u64-storage-class . u64)
	(,srfi-231:f8-storage-class  . f8)
	(,srfi-231:f16-storage-class . f16)
	(,srfi-231:f32-storage-class . f32)
	(,srfi-231:f64-storage-class . f64)
	(,srfi-231:c64-storage-class . c64)
	(,srfi-231:c128-storage-class . c128)
	(,srfi-231:generic-storage-class . "")))

    (define (write-tag array)
      (display "#a")
      ;; Try to determine the rest of the tag from *array*'s storage
      ;; class.  This is far from foolproof.
      (cond ((assv (srfi-231:array-storage-class array)
		   storage-classes) =>
	      (lambda (p)
		(display (cdr p))))
	    (else
	     (display "Warning: write-array couldn't determine storage \
		       class."
		      (current-error-port))
	     (newline (current-error-port)))))

    (define (write-bounds array)
      (let* ((iv (srfi-231:array-domain array))
	     (dimension (srfi-231:interval-dimension iv))
	     (write-dim-bounds
	      (lambda (d)
		(display "(")
		(display (srfi-231:interval-lower-bound iv d))
		(display " ")
		(display (srfi-231:interval-upper-bound iv d))
		(display ")"))))
	(display "(")
	;; Print bounds pair for each dimension of iv.
	(do ((i 0 (+ i 1)))
	    ((= i dimension))
	  (write-dim-bounds i)
	  (when (< i (- dimension 1))
	    (display " ")))
	(display ")")))

    (define (write-contents array)
      (write (srfi-231:array->list* array)))

    (define write-array
      (case-lambda
        ((array port)
	 (check-arg "write-array" output-port? port)
	 (parameterize ((current-output-port port))
	   (write-array array)))
	((array)
	 (check-arg "write-array" srfi-231:array? array)
	 (write-tag array)
	 (write-bounds array)
	 (display " ")
	 (write-contents array))))
  ))
