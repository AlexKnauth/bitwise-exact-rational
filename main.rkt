#lang exact-decimal typed/racket/base

(provide Bit
         bit-field-shift
         bit-field-ref
         bit-field-slice
         bitwise-ior
         bitwise-and
         bitwise-xor
         bitwise-not)

(require racket/match
         (only-in math/number-theory max-dividing-power divides?)
         (only-in racket/list first second)
         (prefix-in int:
                    (only-in typed/racket/base
                             bitwise-bit-set?
                             bitwise-bit-field
                             bitwise-not
                             bitwise-ior
                             bitwise-and
                             bitwise-xor)))
(module+ test
  (require typed/rackunit))

(define-type Bit (U 0 1))

(: int:bit-field-ref (-> Integer Natural Bit))
(define (int:bit-field-ref a i) (if (int:bitwise-bit-set? a i) 1 0))

(: bit-field-shift (-> Exact-Rational Integer Exact-Rational))
(module+ test
  (check-equal? (bit-field-shift 1 10) 1024)
  (check-equal? (bit-field-shift 255 -3) (+ 31 7/8))
  (check-equal? (bit-field-shift 5 -2) 1.25)
  (check-equal? (bit-field-shift -1 10) -1024))
(define (bit-field-shift a i) (* a (expt 2 i)))

(: bit-field-ref (-> Exact-Rational Integer Bit))
(module+ test
  (check-equal? (for/list ([i (in-range 0 4)]) : (Listof Bit)
                  (bit-field-ref 5 i))
                '(1 0 1 0))
  (check-equal? (for/list ([i (in-range -2 2)]) : (Listof Bit)
                  (bit-field-ref 1.25 i))
                '(1 0 1 0)))
(define (bit-field-ref a i)
  (cond
    [(<= 0 i) (int:bit-field-ref (floor a) i)]
    [else (int:bit-field-ref (floor (bit-field-shift a (- i))) 0)]))

(: bit-field-slice (-> Exact-Rational Integer Integer Integer))
(module+ test
  (check-equal? (bit-field-slice 1.25 -2 2) 5)
  (check-equal? (bit-field-slice 1/3 -2 0) 1)
  (check-equal? (bit-field-slice 2/3 -2 0) 2))
(define (bit-field-slice a start end)
  (cond
    [(<= 0 start) (int:bitwise-bit-field (floor a) start end)]
    [else (int:bitwise-bit-field (floor (bit-field-shift a (- start)))
                                 0
                                 (- end start))]))

(: find-repeating (-> Exact-Rational (List Integer Integer)))
;; A repeating binary "decimal" can be expressed as:
;;  n1             n2            n1*(2^de2 - 1) + n2   n
;; ----- + ------------------- = ------------------- = -
;; 2^de1   2^de1 * (2^de2 - 1)   2^de1 * (2^de2 - 1)   d
;; To find de1, find the number of factors of 2 in d.
;; To find de2, try bigger numbers until it's a multiple.
;; Produces (list (- (+ de1 de2)) (- de1))
(module+ test
  (check-equal? (find-repeating 1) (list -1 0))
  (check-equal? (find-repeating 1/2) (list -2 -1))
  (check-equal? (find-repeating 1/4) (list -3 -2))
  (check-equal? (find-repeating 1/3) (list -2 0))
  (check-equal? (find-repeating 5/6) (list -3 -1))
  (check-equal? (find-repeating 7/12) (list -4 -2)))
(define (find-repeating a)
  (define n (numerator a))
  (define d (denominator a))
  (define de1 (max-dividing-power 2 d))
  (define d* (assert (/ d (expt 2 de1)) exact-integer?))
  (define de2
    (assert
     (for/or ([de2 : Natural (in-naturals 1)]
              #:when (divides? d* (sub1 (expt 2 de2))))
       : (U #f Natural)
       de2)))
  (list (- (+ de1 de2)) (- de1)))

(: make-repeating (-> Integer Integer Exact-Rational))
(module+ test
  (check-equal? (make-repeating 0 1) 0)
  (check-equal? (make-repeating 1 1) 1)
  (check-equal? (make-repeating 1 2) 1/3)
  (check-equal? (make-repeating 2 2) 2/3)
  (check-equal? (make-repeating 1 3) 1/7)
  (check-equal? (make-repeating 3 3) 3/7)
  (check-equal? (make-repeating 6 3) 6/7))
(define (make-repeating n2 de2)
  (/ (bitwise-bit-field n2 0 de2) (sub1 (expt 2 de2))))

(: bitwise-unary (-> (-> Integer Integer) (-> Exact-Rational Exact-Rational)))
(define ((bitwise-unary int:op) a)
  (match-define (list a1 a2) (find-repeating a))
  (bit-field-shift
   (+ (int:op (floor (bit-field-shift a (- a2))))
      (make-repeating (int:op (bit-field-slice a a1 a2)) (- a2 a1)))
   a2))

(: bitwise-binary
   (-> (-> Integer Integer Integer)
       (-> Exact-Rational Exact-Rational Exact-Rational)))
(define ((bitwise-binary int:op) a b)
  (match-define (list a1 a2) (find-repeating a))
  (match-define (list b1 b2) (find-repeating b))
  (define r2 (min a2 b2))
  (define r1 (- r2 (lcm (- a2 a1) (- b2 b1))))
  (bit-field-shift
   (+ (int:op
       (floor (bit-field-shift a (- r2)))
       (floor (bit-field-shift b (- r2))))
      (make-repeating
       (int:op (bit-field-slice a r1 r2)
               (bit-field-slice b r1 r2))
       (- r2 r1)))
   r2))

(: bitwise-nary (-> (-> Integer * Integer) (-> Exact-Rational * Exact-Rational)))
(define ((bitwise-nary int:op) . as)
  (define a12s (map find-repeating as))
  (define a1s (map (inst first Integer) a12s))
  (define a2s (map (inst second Integer Integer) a12s))
  (define r2 (apply min 0 a2s))
  (define r1 (- r2 (apply lcm (map - a2s a1s))))
  (bit-field-shift
   (+ (apply int:op
             (for/list ([a (in-list as)]) : (Listof Integer)
               (floor (bit-field-shift a (- r2)))))
      (make-repeating
       (apply int:op
              (for/list ([a (in-list as)]) : (Listof Integer)
                (bit-field-slice a r1 r2)))
       (- r2 r1)))
   r2))

(: bitwise-not (-> Exact-Rational Exact-Rational))
;; is this just negation?
(define bitwise-not (bitwise-unary int:bitwise-not))


(: bitwise-ior (-> Exact-Rational Exact-Rational Exact-Rational))
(module+ test
  (check-equal? (bitwise-ior 5 1.25) 5.25))
(define bitwise-ior (bitwise-binary int:bitwise-ior))
(: bitwise-xor (-> Exact-Rational * Exact-Rational))
(module+ test
  (check-equal? (bitwise-xor 5 1.25) 4.25))
(define bitwise-xor (bitwise-nary int:bitwise-xor))

(: bitwise-and (-> Exact-Rational * Exact-Rational))
(module+ test
  (check-equal? (bitwise-and 5 1.25) 1))
(define bitwise-and (bitwise-nary int:bitwise-and))

;; ---------------------------------------------------------

(: int:bit-field->string (-> Integer Integer String))
(define (int:bit-field->string a n)
  (build-string n (λ (i) (if (int:bitwise-bit-set? a (- n i 1)) #\1 #\0))))

(: int:string->bit-field (-> String Natural))
(define (int:string->bit-field s)
  (define n (string-length s))
  (for/sum ([i : Natural (in-range n)] #:when (char=? (string-ref s (- n i 1)) #\1))
    : Natural
    (expt 2 i)))

(: bit-field->string (-> Exact-Rational String))
(define (bit-field->string a)
  (define sign-bit (if (negative? a) 1 0))
  (define n (integer-length (floor a)))
  (define int-part (bit-field-slice a 0 n))
  (match-define (list a1 a2) (find-repeating a))
  (define non-repeat (bit-field-slice a a2 0))
  (define repeat (bit-field-slice a a1 a2))
  (format "(~a)*~a.~a(~a)*"
          (int:bit-field->string sign-bit 1)
          (int:bit-field->string int-part n)
          (int:bit-field->string non-repeat (- a2))
          (int:bit-field->string repeat (- a2 a1))))

(: string->bit-field (-> String Exact-Rational))
(define (string->bit-field s)
  (match s
    [(regexp
      (string-append "^(([(]([01])[)]|([01]))[*])?"
                     "([(]([01]*)[)]|([01]*))"
                     "[.]?([(]([01]*)[)]|([01]*))"
                     "(([(]([01]*)[)]|([01]))[*])?$")
      (list _ _ _ sgn1 sgn2
            _ int1 int2
            _ no1 no2
            _ _ re1 re2))
     (let ([sgn (or sgn1 sgn2 "0")]
           [int (or int1 int2 "")]
           [no (or no1 no2 "")]
           [re (or re1 re2 "0")])
       (+ (match sgn
            ["1"         (bit-field-shift -1 (string-length int))]
            [(or "0" "") 0])
          (int:string->bit-field int)
          (bit-field-shift
           (int:string->bit-field no)
           (- (string-length no)))
          (bit-field-shift
           (make-repeating
            (int:string->bit-field re)
            (string-length re))
           (- (string-length no)))))]
    [_ (error 'string->exact-rational-bits "bad syntax in ~v" s)]))

(module+ test
  (define-syntax-rule (check-bit-field-string a s)
    (let ([av a] [sv s])
      (test-case (format "(check-bit-field-string ~v ~v)" av sv)
        (check-equal? (bit-field->string av) sv)
        (check-equal? (string->bit-field sv) av))))
  (check-bit-field-string 0    "(0)*.(0)*")
  (check-bit-field-string -1   "(1)*.(0)*")
  (check-bit-field-string 1    "(0)*1.(0)*")
  (check-bit-field-string 1/2  "(0)*.1(0)*")
  (check-bit-field-string -1/2 "(1)*.1(0)*")
  (check-bit-field-string 2    "(0)*10.(0)*")
  (check-bit-field-string 3    "(0)*11.(0)*")
  (check-bit-field-string 4    "(0)*100.(0)*")
  (check-bit-field-string 5    "(0)*101.(0)*")
  (check-bit-field-string 1.25 "(0)*1.01(0)*")
  (check-bit-field-string 1/3  "(0)*.(01)*")
  (check-bit-field-string 2/3  "(0)*.(10)*")
  (check-bit-field-string -1/3 "(1)*.(10)*")
  (check-bit-field-string -2/3 "(1)*.(01)*")
  (check-bit-field-string 1/7  "(0)*.(001)*")
  (check-bit-field-string 2/7  "(0)*.(010)*")
  (check-bit-field-string 3/7  "(0)*.(011)*")
  (check-bit-field-string 4/7  "(0)*.(100)*")
  (check-bit-field-string 5/7  "(0)*.(101)*")
  (check-bit-field-string 6/7  "(0)*.(110)*")
  (check-bit-field-string -1/7 "(1)*.(110)*")
  (check-bit-field-string -2/7 "(1)*.(101)*")
  (check-bit-field-string -3/7 "(1)*.(100)*")
  (check-bit-field-string -4/7 "(1)*.(011)*")
  (check-bit-field-string -5/7 "(1)*.(010)*")
  (check-bit-field-string -6/7 "(1)*.(001)*"))

;; ---------------------------------------------------------

(module+ test
  (: ?string->bit-field
     (case-> (-> String Exact-Rational)
             (-> Natural Natural)
             (-> Integer Integer)
             (-> Exact-Rational Exact-Rational)))
  (define (?string->bit-field s)
    (if (string? s) (string->bit-field s) s))
  (define-syntax-rule (check-bits (f a ...) b)
    (check-equal? (f (?string->bit-field a) ...)
                  (?string->bit-field b)))
  (check-bits (bit-field-shift "(0)*1.(0)*" 10) "(0)*10000000000.(0)*")
  (check-bits (bit-field-shift "(0)*11111111.(0)*" -3) "(0)*11111.111(0)*")
  (check-bits (bit-field-shift "(0)*101.(0)*" -2) "(0)*1.01(0)*")
  (check-bits (bit-field-shift "(1)*.(0)*" 10) "(1)*0000000000.(0)*")
  (check-bits (bit-field-slice "(0)*1.01(0)*" -2 2) "(0)*101.(0)*")
  (check-bits (bit-field-slice "(0)*.(01)*" -2 0) "(0)*1.(0)*")
  (check-bits (bit-field-slice "(0)*.(10)*" -2 0) "(0)*10.(0)*")
  (check-bits (make-repeating 0 1) "(0)*.(0)*")
  (check-bits (make-repeating 1 1) "(0)*1.(0)*")
  (check-bits (make-repeating 1 2) "(0)*.(01)*")
  (check-bits (make-repeating 2 2) "(0)*.(10)*")
  (check-bits (make-repeating 1 3) "(0)*.(001)*")
  (check-bits (make-repeating 3 3) "(0)*.(011)*")
  (check-bits (make-repeating 6 3) "(0)*.(110)*")
  (check-bits (bitwise-xor "0100110" "1001011") "1101101")
  (check-bits (bitwise-xor "1*0100110" "1001011") "1*01101")
  (check-bits (bitwise-xor "0100110" "1*001011") "1*01101")
  (check-bits (bitwise-xor "1*0100110" "1*001011") "1101101")
  (check-bits (bitwise-xor ".(10)*" ".(100)*") ".(001110)*")
  (check-bits (bitwise-xor ".(10)*" ".(1000)*") ".(0010)*")
  (check-bits (bitwise-xor ".(01)*" ".(1000)*") ".(1101)*")
  (check-bits (bitwise-xor ".(100)*" ".(1000)*") ".(000110101100)*")
  (check-bits (bitwise-xor ".(100)*" ".(1000)*") ".(000110101100)*")
  (check-bits ; the 5/7 polyrhythm
   (bitwise-xor ".(10000)*" ".(1000000)*")
   ".(00000101001000110000110001001010000)*")
  (check-bits ; the 7/11 polyrhythm
   (bitwise-xor ".(1000000)*" ".(10000000000)*")
  ".(00000001000100100000011000001000010100000010100001000001100000010010001000000)*"
  )
  (check-equal? ; the 7/11 polyrhythm
   (bitwise-xor 64/127 1024/2047)
   631855320211785786432/151115727451828646838271)

  (check-bits (bitwise-xor ".(01)*" ".(10)*") ".(1)*")
  (check-bits (bitwise-xor ".(01)*" ".(10)*") "1")
  (check-bits (bitwise-not "0*.0*") "1*.1*")
  (check-bits (bitwise-not "0*.(01)*") "1*.(10)*")
  (check-bits (bitwise-not "1*.(01)*") "0*.(10)*"))

