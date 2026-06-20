;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs
(define/curried ((define-numeric-vector-hash len sub) v)
  "Wrap SRFI 160 `@vector-hash' procedures"
  (let ([width (fxmin 256 (len v))])
    (let loop ([k 0] [acc 0])
      (if (fx=? k width)
	  (abs (floor (real-part (inexact->exact acc))))
	  (loop (fx+ k 1) (fx+ acc (sub v k)))))))
