#!r6rs
(library (srfi :210)
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
  (import (srfi :210 multiple-values)))
