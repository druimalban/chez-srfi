;; Copyright © Marc Nieper-Wißkirchen (2020).
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation files
;; (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice (including the
;; next paragraph) shall be included in all copies or substantial
;; portions of the Software.
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
(library (srfi :210 multiple-values)
  (export apply/mv
          call/mv
          list/mv
          vector/mv
          box/mv
          value/mv
          coarity
          set!-values
          with-values
          case-receive
          bind/mv
          list-values
          vector-values
          box-values
          value
          identity
          compose-left
          compose-right
          map-values
          bind/list
          bind/box
          bind)
  (import (only (chezscheme) errorf assertion-violationf)
          (rnrs base (6))
          (only (rnrs arithmetic fixnums (6)) fx=?)
          (only (rnrs conditions (6))
                who-condition? message-condition? irritants-condition?
                condition-who condition-message condition-irritants)
          (only (rnrs control (6)) case-lambda)
          (only (rnrs exceptions (6)) guard raise-continuable)
          (rnrs records syntactic (6))
          (rnrs syntax-case (6))
          (srfi :195 multiple-value-boxes))

  ;;;;;;;;;;;;
  ;; Syntax ;;
  ;;;;;;;;;;;;

  (define-syntax apply/mv
    (syntax-rules ()
      ((apply/mv operator operand1 ... producer)
       (letrec-syntax
           ((aux (syntax-rules ()
                   ((aux %operator () ((%operand1 arg1) (... ...)) %producer)
                    (let-values (((proc) %operator)
                                 ((arg1) %operand1) (... ...)
                                 (args %producer))
                      (apply proc arg1 (... ...) args)))
                   ((aux %operator (%operand1 operand2 (... ...)) (temp (... ...)) %producer)
                    (aux %operator (operand2 (... ...)) (temp (... ...) (%operand1 arg1))
                         %producer)))))
         (aux operator (operand1 ...) () producer)))))

  (define-syntax call/mv
    (syntax-rules ()
      ((call/mv consumer producer1 ...)
       (letrec-syntax
           ((aux (syntax-rules ()
                   ((aux %consumer () ((%producer1 args1) (... ...)))
                    (let-values (((proc) %consumer)
                                 (args1 %producer1) (... ...))
                      (apply proc (append args1 (... ...)))))
                   ((aux %consumer (%producer1 producer2 (... ...)) (temp (... ...)))
                    (aux %consumer (producer2 (... ...)) (temp (... ...) (%producer1 args1)))))))
         (aux consumer (producer1 ...) ())))))

  (define-syntax list/mv
    (syntax-rules ()
      ((list/mv element1 ... producer)
       (apply/mv list element1 ... producer))))

  (define-syntax vector/mv
    (syntax-rules ()
      ((vector/mv element1 ... producer)
       (apply/mv vector element1 ... producer))))

  (define-syntax box/mv
    (syntax-rules ()
      ((box/mv element1 ... producer)
       (apply/mv box element1 ... producer))))

  (define-syntax value/mv
    (syntax-rules ()
      ((value/mv index operand1 ... producer)
       (apply/mv value index operand1 ... producer))))

  (define-syntax coarity
    (syntax-rules ()
      ((coarity producer)
       (let-values ((res producer))
         (length res)))))

  (define-syntax set!-values
    (syntax-rules ()
      ((set!-values (var1 ...) producer)
       (letrec-syntax
           ((aux (syntax-rules ()
                   ((aux () ((%var1 temp1) (... ...)) %producer)
                    (let-values (((temp1 (... ...) . temp*) %producer))
                      (set! %var1 temp1) (... ...)))
                   ((aux (%var1 var2 (... ...)) (temp (... ...)) %producer)
                    (aux (var2 (... ...)) (temp (... ...) (%var1 temp1)) %producer)))))
         (aux (var1 ... ) () producer)))
      ((set!-values (var1 ... . var*) producer)
       (letrec-syntax
           ((aux (syntax-rules ()
                   ((aux () ((%var1 temp1) (... ...) (%var* temp*)) %producer)
                    (let-values (((temp1 (... ...) . temp*) %producer))
                      (set! %var1 temp1) (... ...)
                      (set! %var* temp*)))
                   ((aux (%var1 var2 (... ...)) (temp (... ...)) %producer)
                    (aux (var2 (... ...)) (temp (... ...) (%var1 temp1)) %producer)))))
         (aux (var1 ... var*) () producer)))
      ((set!-values var* producer)
       (let-values ((temp*) producer)
         (set! var* temp*)))))

  (define-syntax with-values
    (syntax-rules ()
      ((with-values producer consumer)
       (apply/mv consumer producer))))

  (define-syntax case-receive
    (syntax-rules ()
      ((case-receive producer clause ...)
       (with-values producer
         (case-lambda clause ...)))))

  (define-syntax bind/mv
    (syntax-rules ()
      ((bind/mv producer transducer ...)
       (bind/list (list/mv producer) transducer ...))))

  ;;;;;;;;;;;;;;;;
  ;; Procedures ;;
  ;;;;;;;;;;;;;;;;

  (define-syntax reraise
    (syntax-rules ()
      ((_ who on body ... body*)
       (guard
        (ex ((and (who-condition? ex)
                  (message-condition? ex)
                  (irritants-condition? ex)
                  (eq? (condition-who ex) (quote on)))
             (apply assertion-violationf
                    (quote who)
                    (condition-message ex)
                    (condition-irritants ex)))
            (else
             (raise-continuable ex)))
        body ... body*))))

  (define (box-values bx)
    (reraise box-values
             unbox
      (unbox bx)))

  (define (list-values lis)
    (reraise list-values
             apply
      (apply values lis)))

  (define (vector-values vec)
    (reraise vector-values
             vector->list
      (list-values (vector->list vec))))

  (define (value k . objs)
    (guard
     (ex ((and (who-condition? ex)
               (irritants-condition? ex)
               (message-condition? ex)
               (eq? (condition-who ex) 'list-ref))
          (cond ((and
                  (fx=? 2 (length (condition-irritants ex)))
                  (integer? k)
                  (not (negative? k)))
                 (apply assertion-violationf
                        (quote value)
                        "index ~s is out of range for values ~{~A ~}"
                        (condition-irritants ex)))
                (else
                 (assertion-violationf
                  (quote value)
                  "index ~s was not a non-negative integer"
                  k))))
         (else
          (raise-continuable ex)))
      (list-ref objs k)))

  (define identity values)

  (define compose-left
    (case-lambda
     (() identity)
     ((transducer . transducers)
      (let f ((transducer transducer) (transducers transducers))
        (if (null? transducers)
            transducer
            (let ((composition (f (car transducers) (cdr transducers))))
              (lambda args
                (apply/mv composition (apply transducer args)))))))))

  (define compose-right
    (case-lambda
     (() identity)
     ((transducer . transducers)
      (let f ((transducer transducer) (transducers transducers))
        (if (null? transducers)
            transducer
            (let ((composition (f (car transducers) (cdr transducers))))
              (lambda args
                (apply/mv transducer (apply composition args)))))))))

  (define (map-values proc)
    (lambda args
      (list-values (map proc args))))

  (define bind/list
    (case-lambda
     ((lis)
      (reraise bind/list
               list-values
        (list-values lis)))
     ((lis transducer)
      (reraise bind/list
               apply
        (apply transducer lis)))
     ((lis transducer . transducers)
      (reraise bind/list
               apply
        (apply bind/list
               (list/mv (apply transducer lis))
               transducers)))))

  (define (bind/box bx . transducers)
    (reraise bind/box
             unbox
      (apply bind/list
             (list/mv (unbox bx))
             transducers)))

  (define (bind obj . transducers)
    (apply bind/list (list obj) transducers))

  ); library
