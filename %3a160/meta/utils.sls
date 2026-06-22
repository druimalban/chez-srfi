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
          nonnegative-fixnum?
          exact?
          inexact?
          exact-integer?
          inexact-integer?
          magnitude>? magnitude<?
          ;;
          identity compose thunk make-range
          ;;
          raised-with? reraise
          define/who define/case-who
          assert/who
          guarded-body
          ;;
          assert-start-nat assert-end-nat
          assert-start<=end assert-bounds
          assert-index-nat assert-index-bounds
          sub-append-triple)
  (import (only (chezscheme) assertion-violationf errorf with-implicit format)
          (rename (rnrs base (6))
                  (exact? r6rs:exact?)
                  (inexact? r6rs:inexact?))
          (only (rnrs arithmetic fixnums (6))
                fxpositive? fxnegative? fixnum?
                fx=? fx>=? fx>? fx<? fx<=? fx+ fx-)
          (only (rnrs conditions (6))
                who-condition? condition-who
                irritants-condition? condition-irritants
                message-condition? condition-message)
          (only (rnrs control (6)) case-lambda when unless)
          (only (rnrs exceptions (6)) guard raise raise-continuable)
          (only (rnrs lists (6)) memq)
          (rnrs syntax-case (6))
          (only (srfi :1 lists) iota))

  (define like-vowels
    (string->list "faeiohlmnrstvwxyzFAEIOHLMNRSTVWXYZ"))

  (define (one-of str)
    (if (and (fx>? (string-length str) 0)
             (memq (string-ref str 0) like-vowels))
        "an"
        "a"))

  (define (format-vector-type fmt-str type-of)
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

  (define raised-with?
    (case-lambda
     [(ex who)
      (and (who-condition? ex)
           (eq? (condition-who ex) who))]
     [(ex who . who*)
      (and (who-condition? ex)
           (eq? (condition-who ex) (cons who who*)))]))

  (define-syntax (guarded-body stx)
    (syntax-case stx ()
      [(_ who (ident ...) body0 ... body*)
       #'(guard
          (ex [(and (who-condition? ex)
                    (memq (condition-who ex) (list (quote ident) ...)))
               (cond [(and (irritants-condition? ex) (message-condition? ex))
                      (apply assertion-violationf who (condition-message ex)
                             (condition-irritants ex))]
                     [(message-condition? ex)
                      (assertion-violationf who (condition-message ex))]
                     [else
                      (raise-continuable ex)])]
              [else
               (raise-continuable ex)])
          body0 ... body*)]))

  (define-syntax (reraise stx)
    (syntax-violation #f "invalid use of auxilliary syntax" stx))

  (define-syntax (define/who stx)
    (syntax-case stx (reraise)
      [(k (bind-to . args) body0 ... body* (reraise ident ...))
       (identifier? #'bind-to)
       (with-implicit (k who)
                      #'(define bind-to
                          (let ([who (quote bind-to)])
                            (lambda args
                              (guarded-body who (ident ...) body0 ... body*)))))]))

  (define-syntax (define/case-who stx)
    (syntax-case stx (reraise)
      [(k bind-to
          [args0 body0 ... body*]
          ...
          [argsN bodyN ... bodyN*]
          (reraise ident ...))
       (identifier? #'bind-to)
       (with-implicit (k who)
                      #'(define bind-to
                          (let ([who (quote bind-to)])
                            (case-lambda
                             [args0 (guarded-body who (ident ...) body0 ... body*)]
                             ...
                             [argsN (guarded-body who (ident ...) bodyN ... bodyN*)]))))]))

  (define-syntax (assert/who stx)
    (syntax-case stx ()
      [(_ who expression message)
       #'(unless expression
	   (assertion-violationf who message))]
      [(_ who expression message irritant ...)
       #'(unless expression
           (assertion-violationf who message
                                 irritant ...))]))

  (define assert-start-nat
    (case-lambda
     [(who start)
      (assert/who who
                  (nonnegative-fixnum? start) "start ~a is not a non-negative fixnum"
                  start)]
     [(who start prefix)
      (assert/who who
                  (nonnegative-fixnum? start)
                  (format "~a start ~~a is not a non-negative fixnum" prefix) start)]))

  (define assert-end-nat
    (case-lambda
     [(who end)
      (assert/who who (nonnegative-fixnum? end) "end ~a is not a non-negative fixnum" end)]
     [(who end prefix)
      (assert/who who
                  (nonnegative-fixnum? end)
                  (format "~a end ~~a is not a non-negative fixnum" prefix) end)]))

  (define assert-start<=end
    (case-lambda
     [(who start end)
      (assert/who who (fx<=? start end) "end ~a must be greater than or equal to start ~a" end start)]
     [(who start end anno)
      (assert/who who
                  (fx<=? start end)
                  (format "~a end ~~a must be greater than or equal to ~a start ~~a" anno anno)
                  end start)]))

  (define (assert-bounds who end width v)
    (assert/who who (fx<=? end width) "end ~a overflows ~a"
                end v))

  (define assert-index-nat
    (case-lambda
     [(who k)
      (assert/who who
                  (nonnegative-fixnum? k)
                  "index ~a is not a non-negative fixnum"
                  k)]
     [(who k prefix)
      (assert/who who
                  (nonnegative-fixnum? k)
                  (format "~a index ~~a is not a non-negative fixnum" prefix) k)]))

  (define assert-index-bounds
    (lambda (who k size v)
      (assert/who who (fx>=? size k) "index ~a overflows ~a"
                  k v)))

  (define (sub-append-triple who lst)
    (if (and (list? lst) (fx=? (length lst) 3))
        (apply values lst)
        (assert/who who #f
                    "expected triple of vector, start and end"
                    #;lst)))

  ;;;
  ); define library
