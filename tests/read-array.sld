(define-library (tests read-array)
  (export run-tests)
  (import (scheme base)
	  (srfi 1)
          (srfi 64)
          (srfi 231)
          (srfi 268 read)
	  (tests util)
          )
  (begin
    (define (string->array s)
      (call-with-port (open-input-string s) read-array))

    (define (run-tests)
      (test-group "read-array"
	(test-assert "1d generic array"
	  (array=? (list*->array 1 '(1 2 3))
		   (string->array "#a(3) (1 2 3)")))
	(test-assert "1d generic array, both bounds given"
	  (array=? (list*->array 1 '(1 2 3))
		   (string->array "#a((0 3)) (1 2 3)")))
	(test-assert "1d generic array, uppercase tag"
	  (array=? (list*->array 1 '(1 2 3))
		   (string->array "#A(3) (1 2 3)")))
	(test-assert "1d generic array, whitespace variation (1)"
	  (array=? (list*->array 1 '(1 2 3))
		   (string->array "#a (3) (1 2 3)")))
	(test-assert "1d generic array, whitespace variation (2)"
	  (array=? (list*->array 1 '(1 2 3))
		   (string->array "#a(3)(1 2 3)")))
	(test-assert "1d u16 array"
	  (array=? (list*->array 1 '(1 2 3) u16-storage-class)
		   (string->array "#au16(3) (1 2 3)")))
	(test-assert "1d u16 array, both bounds given, whitespace var."
	  (array=? (list*->array 1 '(1 2 3) u16-storage-class)
		   (string->array "#au16\t((0 3))\t(1 2 3)")))
	(test-assert "1d generic array, nonzero lower bound"
	  (array=? (list->array (make-interval '#(4) '#(7))
				'(1 2 3))
		   (string->array "#a((4 7)) (1 2 3)")))
	(test-assert "2d generic array"
	  (array=? (list*->array 2 '((a b) (c d)))
		   (string->array "#A(2 2) ((a b) (c d))")))
	(test-assert "2d generic array, mixed bound types"
	  (array=? (list*->array 2 '((a b) (c d)))
		   (string->array "#a((0 2) 2) ((a b) (c d))")))
	(test-assert "2x2x3 u16 array"
	  (array=? (list->array (make-interval '#(2 2 3))
				(iota 12)
				u16-storage-class)
		   (string->array
		    "#au16(2 2 3) (((0 1 2) (3 4 5))
				   ((6 7 8) (9 10 11)))")))
	(test-assert "2x2x3 u16 array, nonzero lower bounds"
	  (array=? (list->array (make-interval '#(1 2 3)
					       '#(3 4 6))
				(iota 12)
				u16-storage-class)
		   (string->array
		    "#Au16((1 3) (2 4) (3 6))
			  (((0 1 2) (3 4 5))
			   ((6 7 8) (9 10 11)))")))
	(test-assert "0d generic array"
	  (array=? (list*->array 0 1)
		   (string->array "#a() 1")))
	(test-assert "0d char array"
	  (array=? (list*->array 0 #\b char-storage-class)
		   (string->array "#achar() #\\b")))

	;;; Erroneous syntax

	(test-error "bad tag (1)" #t
	  (string->array "%a(3) (1 2 3)"))
	(test-error "bad tag (2)" #t
	  (string->array "#b(3) (1 2 3)"))
	(test-error "bad bounds (1)" #t
	  (string->array "#a(3.3) (1 2 3)"))
	(test-error "bad bounds (2)" #t
	  (string->array "#au16(Z) (1 2 3)"))
	(test-error "bad bounds (3)" #t
	  (string->array "#a((3 0)) (1 2 3)"))
	(test-error "junk characters between bounds & datum" #t
	  (string->array "#a(3)Z(1 2 3)"))
	))
    ))
