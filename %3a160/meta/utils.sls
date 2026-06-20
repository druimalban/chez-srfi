;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs
(library (srfi :160 meta utils)
  (export format-vector-type
          ;;
          compare-lengths all-same-length? total-length
	  vectorised-subscript
          ;;
          positive-fixnum?
          negative-fixnum?
          nonnegative-fixnum?
          exact?
          inexact?
          exact-integer?
          inexact-integer?
          magnitude>? magnitude<?
          ;;
          identity compose thunk make-range)
  (import (rename (rnrs base (6))
                  (exact? r6rs:exact?)
                  (inexact? r6rs:inexact?))
          (only (rnrs arithmetic fixnums (6))
                fxpositive? fxnegative? fixnum?
                fx=? fx>? fx<? fx+ fx-)
          (rnrs syntax-case (6))
          (only (rnrs lists (6)) memq)
          (only (srfi :1 lists) iota)
          (srfi :28 basic-format-strings))

  (define like-vowels
    (string->list "faeiohlmnrstvwxyzFAEIOHLMNRSTVWXYZ"))

  (define (one-of str)
    (if (and (fx>? (string-length str) 0)
             (memq (string-ref str 0) like-vowels))
        "an"
        "a"))

  (define (format-vector-type fmt-str type-of)
    #| We're actually formatting this twice. |#
    (let ([template (format "~a ~a" (one-of type-of) type-of)])
      (format fmt-str template)))

  (define (compare-lengths len comp v . vs)
    "Apply length and run a comparison like min/max, and return the result or #f"
    (let loop ([src vs] [prev (len v)])
      (if (null? src)
	  prev
	  (let ([res (comp prev (len (car src)))])
	    (and res
		 (loop (cdr src) res))))))

  (define (all-same-length? len v . vs)
    (apply compare-lengths
	   len
	   (lambda (prev curr)
	     (and (fx=? prev curr)
		  curr))
	   v vs))

  (define (total-length len vs)
    (apply compare-lengths len + vs))

  (define (vectorised-subscript sub vs i)
    (map (lambda (v) (sub v i))
	 vs))

  (define (positive-fixnum? x)
    (and (fixnum? x)
         (fxpositive? x)))

  (define (negative-fixnum? x)
    (and (fixnum? x)
         (fxnegative? x)))

  (define (nonnegative-fixnum? x)
    (and (fixnum? x)
         (not (fxnegative? x))))

  (define (exact? x)
    (and (number? x) (r6rs:exact? x)))

  (define (inexact? x)
    (and (number? x) (r6rs:inexact? x)))

  (define (exact-integer? x)
    (and (integer? x)
         (r6rs:exact? x)))

  (define (inexact-integer? x)
    (and (integer? x)
         (r6rs:inexact? x)))

  (define (magnitude>? x y)
    (> (magnitude x) (magnitude y)))

  (define (magnitude<? x y)
    (< (magnitude x) (magnitude y)))

  (define (compose f . rest)
    (if (null? rest)
	f
	(let ([g (apply compose rest)])
	  (lambda args
	    (call-with-values (lambda () (apply g args)) f)))))

  (define identity values)

  (define-syntax thunk
    (lambda (stx)
      (syntax-case stx ()
	[(_) #'(lambda () (values))]
	[(_ body0 body ...) #'(lambda () body0 body ...)])))

  (define (make-range A B)
    (if (or (not (fixnum? A)) (not (fixnum? B)) (fx<? B A))
	(error 'make-range "range must be from fixnums A to B where A <= B")
	(iota (fx- B A) A)))

  ;;;
  ); define library
