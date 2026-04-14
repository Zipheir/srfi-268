;;; SPDX-FileCopyrightText: 2026 John Cowan, Per Bothner, Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT
(define-library (tests util)
  (export array=?
          )
  (import (scheme base)
          (srfi 231)
          )
  (begin
    ;; A crude notion of equality, but sufficient for our purposes.
    (define (array=? A B)
      (and (eqv? (array-storage-class A)
                 (array-storage-class B))
           (interval= (array-domain A)
		      (array-domain B))
           (equal? (array->list A)
                   (array->list B))))
  ))
