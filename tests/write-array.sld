;;; SPDX-FileCopyrightText: 2026 John Cowan, Per Bothner, Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT
(define-library (tests write-array)
  (export run-tests)
  (import (scheme base)
          (srfi 64)
          (srfi 231)
          (srfi 268 read)
          (srfi 268 write)
	  (tests util)
          )
  (begin
    ;; Write array *A* and read the output back in with read-array.
    (define (write-&-read-array A)
      (let ((s (call-with-port
       		(open-output-string)
       		(lambda (port)
	 	  (write-array A port)
	 	  (get-output-string port)))))
	(call-with-port (open-input-string s) read-array)))

    (define (array-wr-equals? A)
      (array=? A (write-&-read-array A)))

    (define (run-tests)
      (test-group "write-array"
	(test-assert "1d generic array"
	  (array-wr-equals? (list*->array 1 '(1 2 3))))
	(test-assert "1d char array"
	  (array-wr-equals?
	   (list*->array 1 '(#\a #\b #\c) char-storage-class)))
	(test-assert "1d s8 array"
	  (array-wr-equals?
	   (list*->array 1 '(1 -2 3) s8-storage-class)))
	(test-assert "1d s16 array"
	  (array-wr-equals?
	   (list*->array 1 '(1 -2 3) s16-storage-class)))
	(test-assert "1d s32 array"
	  (array-wr-equals?
	   (list*->array 1 '(1 -2 3) s32-storage-class)))
	(test-assert "1d s64 array"
	  (array-wr-equals?
	   (list*->array 1 '(1 -2 3) s64-storage-class)))
	(test-assert "1d u1 array"
	  (array-wr-equals?
	   (list*->array 1 '(1 0 0) u1-storage-class)))
	(test-assert "1d u8 array"
	  (array-wr-equals?
	   (list*->array 1 '(1 2 3) u8-storage-class)))
	(test-assert "1d u16 array"
	  (array-wr-equals?
	   (list*->array 1 '(1 2 3) u16-storage-class)))
	(test-assert "1d u32 array"
	  (array-wr-equals?
	   (list*->array 1 '(1 2 3) u32-storage-class)))
	(test-assert "1d u64 array"
	  (array-wr-equals?
	   (list*->array 1 '(1 2 3) u64-storage-class)))
	(test-assert "1d f8 array"
	  (array-wr-equals?
	   (list*->array 1 '(1.0 2.3 3.8) f8-storage-class)))
	(test-assert "1d f16 array"
	  (array-wr-equals?
	   (list*->array 1 '(1.0 2.3 3.8) f16-storage-class)))
	(test-assert "1d f32 array"
	  (array-wr-equals?
	   (list*->array 1 '(1.0 2.3 3.8) f32-storage-class)))
	(test-assert "1d f64 array"
	  (array-wr-equals?
	   (list*->array 1 '(1.0 2.3 3.8) f64-storage-class)))
	(test-assert "1d c64 array"
	  (array-wr-equals?
	   (list*->array 1 '(1.0+2.0i 2.3 3.8@0.4) c64-storage-class)))
	(test-assert "1d c128 array"
	  (array-wr-equals?
	   (list*->array 1 '(1.0+2.0i 2.3 3.8@0.4) c128-storage-class)))
	(test-assert "2d generic array"
	  (array-wr-equals? (list*->array 2
					  '((1 2) (a b)))))
	(test-assert "2d s16 array"
	  (array-wr-equals? (list*->array 2
					  '((-1 2) (-4 5))
					  s16-storage-class)))
	(test-assert "2d f64 array"
	  (array-wr-equals?
	   (list->array (make-interval '#(2 3))
			'(23.0 10.0 11.0 13.0 14.0 4.0)
			f64-storage-class)))
	(test-assert "2d generic array, nonzero lower bounds"
	  (array-wr-equals?
	   (list->array (make-interval '#(1 3) '#(4 6))
			'(a b c d e f g h i))))
	(test-assert "0d generic array"
	  (array-wr-equals? (list*->array 0 1)))
	(test-assert "0d char array"
	  (array-wr-equals? (list*->array 0 #\a char-storage-class)))
      ))
  ))
