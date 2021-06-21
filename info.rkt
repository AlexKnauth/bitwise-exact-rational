#lang info

;; --------------------------------------------------------
;; Package Info

(define collection "bitwise-exact-rational")

(define version "0.0")

(define deps
  '("base" "exact-decimal-lang" "typed-racket-lib" "math-lib"))

(define build-deps
  '("scribble-lib" "racket-doc" "typed-racket-doc" "rackunit-typed"))

;; --------------------------------------------------------
;; Collection Info

(define scribblings
  '(["scribblings/bitwise-exact-rational.scrbl" ()]))
