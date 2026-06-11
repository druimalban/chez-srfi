;;  Marc Nieper-WiCopyrightkirchen (2020).
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
;; ---------------------
;; Modifications for R6RS; minimal error-handling: D. Guthrie, Glasgow, 2026.
;;
#!r6rs
(import (srfi :64 testing)
        (srfi :195 multiple-value-boxes)
        (srfi :210 multiple-values))

(define-syntax test-values
  (syntax-rules ()
    ((test-values (expected ...) test-expr)
     (test-equal (list expected ...) (list/mv test-expr)))
    ((test-values test-name (expected ...) test-expr)
     (test-equal test-name (list expected ...) (list/mv test-expr)))))

(define-syntax test-expect-error
  (syntax-rules ()
    ((test-expect-error
      label
      body ... body*
      (irr ...)
      msg)
     (guard
      (ex ((and (assertion-violation? ex)
		(who-condition? ex)
		(irritants-condition? ex)
		(message-condition? ex)
		(eq? (condition-who ex) (quote label)))
           (test-assert
	    (and (string=? (condition-message ex) msg)
		 (equal? (condition-irritants ex) (list irr ...)))))
	  ((and (assertion-violation? ex)
		(who-condition? ex)
		(irritants-condition? ex)
		(eq? (condition-who ex) (quote label)))
	   (test-equal (condition-irritants ex) (list irr ...)))
          (else
           (test-assert #f)))
      body ... body*
      (test-assert #f)))))

(test-begin "SRFI 210")

(test-equal "abc" (apply/mv string #\a (values #\b #\c)))

(test-equal "abcd" (call/mv string (values #\a #\b) (values #\c #\d)))

(test-equal '(a b c) (list/mv 'a (values 'b 'c)))

(test-equal '#(a b c) (vector/mv 'a (values 'b 'c)))

(test-values ('a 'b 'c) (unbox (box/mv 'a (values 'b 'c))))

(test-equal 'b (value/mv 1 'a (values 'b 'c)))

(test-equal 3 (coarity (values 'a 'b 'c)))

(test-equal '(a (b))
            (let ((x #f) (y #f))
              (set!-values (x . y) (values 'a 'b))
              (list x y)))

(test-equal 5
            (with-values (values 4 5)
              (lambda (a b) b)))

(test-equal '(a (b))
            (case-receive (values 'a 'b)
                          ((x) #f)
                          ((x . y) (list x y))))

(test-values (3 5 7) (bind/mv (values 1 2 3)
                              (map-values (lambda (x) (* 2 x)))
                              (map-values (lambda (x) (+ 1 x)))))

(test-values ('a 'b 'c)
             (list-values '(a b c)))
(test-expect-error list-values
                   (list-values 'a)
                   ('a)
                   "~s is not a proper list")

(test-values ('a 'b 'c)
             (vector-values '#(a b c)))
(test-expect-error vector-values
                   (vector-values 'a)
                   ('a)
                   "~s is not a vector")

(test-values ('a 'b 'c)
             (box-values (box 'a 'b 'c)))
(test-expect-error box-values
                   (box-values 'a)
                   ('a)
                   "~s is not a box")

(test-equal 'b (value 1 'a 'b 'c))
(test-expect-error value
                   (value 19 'a 'b 'c)
                   (19 '(a b c))
                   "index ~s is out of range for values ~{~A ~}")
(test-expect-error value
                   (value 'k 'a 'b 'c)
                   ('k)
                   "index ~s was not a non-negative integer")
(test-expect-error value
                   (value -19 'a 'b 'c)
                   (-19)
                   "index ~s was not a non-negative integer")

(test-values (1 2 3) (identity 1 2 3))

(test-values (3 5 7)
             (let ((f (map-values (lambda (x) (* 2 x))))
                   (g (map-values (lambda (x) (+ x 1)))))
               ((compose-left f g) 1 2 3)))

(test-values (4 6 8)
             (let ((f (map-values (lambda (x) (* 2 x))))
                   (g (map-values (lambda (x) (+ x 1)))))
               ((compose-right f g) 1 2 3)))

(test-values (#t #f #t) ((map-values odd?) 1 2 3))

(test-values (3 6 9)
             (bind/list '(1 2 3) (map-values (lambda (x) (* 3 x)))))
(test-expect-error bind/list
                   (bind/list 'a)
                   ('a)
                   "~s is not a proper list")

(test-values (3 6 9)
             (bind/box (box 1 2 3) (map-values (lambda (x) (* 3 x)))))
(test-expect-error bind/box
                   (bind/box 'a)
                   ('a)
                   "~s is not a box")

(test-values (3 2)
             (bind 1 (lambda (x) (values (* 3 x) (+ 1 x)))))

(test-end) 
