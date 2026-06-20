;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs

#|
   Note, this definition requires the lexical form being printed correctly by
   the Scheme system, so it isn't portable.

   A portable version of this might be defined in the same way as the Chez
   Scheme record type printing by taking a string or atom corresponding to
   the prefix in the lexical form (e.g. `u8' for a u8vector) and then formatting
   the string "#~a~a" with that and the representation to list (e.g. u8vector->list).

   So in the u8vector example, we would print `#u8(0 1 2 3 ...)'.
|#
(define/curried-case (define-write-numeric-vector)
  "Wrap SRFI 160 `write-@vector' procedures"
  ([(v)      (display v (current-output-port))]
   [(v port) (display v (current-output-port))]))
