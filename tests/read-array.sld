;;; SPDX-FileCopyrightText: 2026 John Cowan, Per Bothner, Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT
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
	(test-assert "1d char array"
	  (array=? (list*->array 1 '(#\a #\b #\c) char-storage-class)
		   (string->array "#achar(3) (#\\a #\\b #\\c)")))
	(test-assert "1d s8 array"
	  (array=? (list*->array 1 '(1 -2 3) s8-storage-class)
		   (string->array "#as8(3) (1 -2 3)")))
	(test-assert "1d s16 array"
	  (array=? (list*->array 1 '(1 -2 3) s16-storage-class)
		   (string->array "#as16(3) (1 -2 3)")))
	(test-assert "1d s32 array"
	  (array=? (list*->array 1 '(1 -2 3) s32-storage-class)
		   (string->array "#as32(3) (1 -2 3)")))
	(test-assert "1d s64 array"
	  (array=? (list*->array 1 '(1 -2 3) s64-storage-class)
		   (string->array "#as64(3) (1 -2 3)")))
	(test-assert "1d u1 array"
	  (array=? (list*->array 1 '(1 0 0) u1-storage-class)
		   (string->array "#au1(3) (1 0 0)")))
	(test-assert "1d u8 array"
	  (array=? (list*->array 1 '(1 2 3) u8-storage-class)
		   (string->array "#au8(3) (1 2 3)")))
	(test-assert "1d u16 array"
	  (array=? (list*->array 1 '(1 2 3) u16-storage-class)
		   (string->array "#au16(3) (1 2 3)")))
	(test-assert "1d u32 array"
	  (array=? (list*->array 1 '(1 2 3) u32-storage-class)
		   (string->array "#au32(3) (1 2 3)")))
	(test-assert "1d u64 array"
	  (array=? (list*->array 1 '(1 2 3) u64-storage-class)
		   (string->array "#au64(3) (1 2 3)")))
	(test-assert "1d f8 array"
	  (array=? (list*->array 1 '(1.0 2.3 3.8) f8-storage-class)
		   (string->array "#af8(3) (1.0 2.3 3.8)")))
	(test-assert "1d f16 array"
	  (array=? (list*->array 1 '(1.0 2.3 3.8) f16-storage-class)
		   (string->array "#af16(3) (1.0 2.3 3.8)")))
	(test-assert "1d f32 array"
	  (array=? (list*->array 1 '(1.0 2.3 3.8) f32-storage-class)
		   (string->array "#af32(3) (1.0 2.3 3.8)")))
	(test-assert "1d f64 array"
	  (array=? (list*->array 1 '(1.0 2.3 3.8) f64-storage-class)
		   (string->array "#af64(3) (1.0 2.3 3.8)")))
	(test-assert "1d c64 array"
	  (array=? (list*->array 1
				 '(1.0+2.0i 2.3 3.8@0.4)
				 c64-storage-class)
		   (string->array "#ac64(3) (1.0+2.0i 2.3 3.8@0.4)")))
	(test-assert "1d c128 array"
	  (array=? (list*->array 1
				 '(1.0+2.0i 2.3 3.8@0.4)
				 c128-storage-class)
		   (string->array "#ac128(3) (1.0+2.0i 2.3 3.8@0.4)")))
	(test-assert "1d u16 array, both bounds given, whitespace var."
	  (array=? (list*->array 1 '(1 2 3) u16-storage-class)
		   (string->array "#au16\t((0 3))\t(1 2 3)")))
	(test-assert "1d generic array, nonzero lower bound"
	  (array=? (list->array (make-interval '#(4) '#(7))
				'(1 2 3))
		   (string->array "#a((4 7)) (1 2 3)")))
	(test-assert "1d generic array, bounds in binary"
	  (array=? (list->array (make-interval '#(#b100) '#(#b111))
				'(1 2 3))
		   (string->array "#a((#b100 #b111)) (1 2 3)")))
	(test-assert "1d generic array, explicit decimal radix in bounds"
	  (array=? (list->array (make-interval '#(4) '#(7))
				'(1 2 3))
		   (string->array "#a((#d4 #d7)) (1 2 3)")))
	(test-assert "1d generic array, bounds in octal"
	  (array=? (list->array (make-interval '#(#o4) '#(#o7))
				'(1 2 3))
		   (string->array "#a((#o4 #o7)) (1 2 3)")))
	(test-assert "1d generic array, bounds in hex"
	  (array=? (list->array (make-interval '#(#x4) '#(#x7))
				'(1 2 3))
		   (string->array "#a((#x4 #x7)) (1 2 3)")))
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
