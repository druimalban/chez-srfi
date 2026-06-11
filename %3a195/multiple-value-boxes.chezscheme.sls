;; Copyright (C) Marc Nieper-Wißkirchen (2020).  All Rights Reserved.
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
#!r6rs
(library (srfi :195 multiple-value-boxes)
  (export ;; SRFI 111 boxes
   box box? unbox set-box!
   ;; SRFI 195 multiple-value boxes
   box-arity unbox-value set-box-value!)
  (import (only (chezscheme) errorf assertion-violationf)
          (rename (rnrs base (6))
                  (list-ref ex:list-ref))
          (only (rnrs arithmetic fixnums (6)) fx+ fx=?)
          (only (rnrs conditions (6)) who-condition? condition-who)
          (only (rnrs exceptions (6)) guard raise-continuable)
          (only (rnrs lists (6)) memq)
          (only (rnrs io simple (6)) display)
          (only (rnrs mutable-pairs (6)) set-car!)
          (rnrs records syntactic (6))
          (rnrs syntax-case (6))
          (prefix (srfi :111 boxes) ex:))

  (define (box . v*)
    (ex:box v*))

  (define (box? bx)
    (and (ex:box? bx)
         (list? (ex:unbox bx))))

  (define-syntax (define-app-unbox stx)
    (syntax-case stx ()
      [(k bind-to (op . extra-args) catch* ...)
       (let ([catch-these (syntax->datum #'(unbox op catch* ...))])
         #`(define (bind-to bx . extra-args)
             (guard (ex [(and (who-condition? ex)
                              (memq (condition-who ex)
                                    `(,unbox ,(quote op) ,(quote catch*) ...)
                                    #;#,'catch-these))
                         (assertion-violationf (quote bind-to)
                                               "~a is not a multiple value box"
                                               bx)]
                        [else
                         (raise-continuable ex)])
                (op (ex:unbox bx) . extra-args))))]))

  (define (set-box! b . v*)
    (ex:set-box! b v*))

  (define (values-of lst) (apply values lst))

  (define (list-set! lst i obj)
    (let loop ((k 0) (kdr lst))
      (if (null? kdr)
          (errorf 'set-box-value!
                  "index ~a exceeds the length of ~a"
                  i lst)
          (if (fx=? k i)
              (set-car! kdr obj)
              (loop (fx+ k 1) (cdr kdr))))))

  (define (list-ref lst i)
    (guard (ex [(and (who-condition? ex)
                     (eq? (condition-who ex) 'list-ref))
                (errorf 'unbox-value
                        "~a is not a valid index for box values ~a"
                        i lst)]
               [else
                (raise-continuable ex)])
      (ex:list-ref lst i)))

  (define-app-unbox unbox          (values-of) values)
  (define-app-unbox box-arity      (length))
  (define-app-unbox unbox-value    (list-ref k))     ; -> list-ref (unbox b) k
  (define-app-unbox set-box-value! (list-set! k obj)); -> list-set! (unbox b) k obj

  ;;;
  ); library
