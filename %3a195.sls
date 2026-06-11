#!r6rs
(library (srfi :195)
  (export ;; SRFI 111 boxes
          box box? unbox set-box!
          ;; SRFI 195 multiple-value boxes
          box-arity unbox-value set-box-value!)
  (import (srfi :195 multiple-value-boxes)))
