;;; SPDX-FileCopyrightText: 2026 John Cowan, Per Bothner, Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT
(define-library (srfi 268)
  (export read-array write-array)
  (import (srfi 268 read)
	  (srfi 268 write)))
