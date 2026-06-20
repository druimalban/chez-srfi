;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs
(define/curried-case (define-numeric-vector-make-generator sub len)
  "Wrap SRFI 160 `make-@vector-generator' procedures"
  (recur [(v) (recur v 0 (len v))]
         [(v start) (recur v start (len v))]
         [(v start end)
          (assert-start-nat who start)
          (assert-end-nat who end)
          (assert/who who
                      (fx<? start end)
                      "source end (~a) must be greater than or equal to source start (~a), both less than length"
                      end start)
          (make-coroutine-generator
           (lambda (yield)
	     (let loop ([k start])
               (when (fx<? k end)
                 (yield (sub v k))
                 (loop (fx+ k 1))))))]))
