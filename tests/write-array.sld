(define-library (tests write-array)
  (export run-tests)
  (import (scheme base)
          (srfi 64)
          (srfi 231)
          (srfi 268 read)
          (srfi 268 write)
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

    ;; A crude notion of equality, but sufficient for our purposes.
    (define (array=? A B)
      (and (eqv? (array-storage-class A) (array-storage-class B))
	   (interval= (array-domain A) (array-domain B))
	   (equal? (array->list A) (array->list B))))

    (define (array-wr-equals? A)
      (array=? A (write-&-read-array A)))

    (define (run-tests)
      (test-group "write-array"
	(test-assert "1d generic array (1)"
	  (array-wr-equals? (list*->array 1 '(1 2 3))))
      ))
  ))
