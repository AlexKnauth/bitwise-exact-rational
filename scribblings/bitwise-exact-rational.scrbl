#lang scribble/manual

@(require racket/require
          scribble/example
          (for-label bitwise-exact-rational
                     (subtract-in typed/racket/base bitwise-exact-rational)))

@title{bitwise-exact-rational}
@author{Alex Knauth}

@defmodule[bitwise-exact-rational]

@(define ev (make-base-eval #:lang 'typed/racket '(require bitwise-exact-rational)))

Bitwise operations treating exact-rational numbers as bit fields.

@defproc[(bitwise-ior [x Exact-Rational] [y Exact-Rational]) Exact-Rational]{
Produces the bitwise "inclusive or" of @racket[x] and
@racket[y] in their fully infinite two's complement
representation.

@examples[
  #:eval ev
  (bitwise-ior 1 2)
  (bitwise-ior -32 1)
  (bitwise-ior 9 (+ 5 1/4))
  (bitwise-ior 64/127 1024/2047)
]
}

@defproc[(bitwise-and [x Exact-Rational] ...) Exact-Rational]{
Produces the bitwise "and" of the @racket[x]s in their fully
infinite two's complement representation.

@examples[
  #:eval ev
  (bitwise-and)
  (bitwise-and 1 2)
  (bitwise-and -32 1)
  (bitwise-and (+ 9 3/4) (+ 5 1/4))
  (bitwise-and 64/127 1024/2047)
]
}

@defproc[(bitwise-xor [x Exact-Rational] ...) Exact-Rational]{
Produces the bitwise "exclusive or" of the @racket[x]s in
their fully infinite two's complement representation.

@examples[
  #:eval ev
  (bitwise-xor)
  (bitwise-xor 1 2)
  (bitwise-xor 1 5)
  (bitwise-xor -32 1)
  (bitwise-xor -32 -1)
  (bitwise-xor 9 (+ 5 1/4))
  (bitwise-xor 64/127 1024/2047)
]
}
