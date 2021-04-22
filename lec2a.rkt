#lang sicp
(define (sum-int a b)
  (if (> a b)
      0
      (+ a
         (sum-int (inc a) b))))
(define (sq n)
  (* n n))

(define (sum-sq a b)
  (if (> a b)
      0
      (+ (sq a)
         (sum-sq (inc a) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-int-2 a b)
  (sum identity a inc b))

(define (sum-sq-2 a b)
  (sum sq a inc b))

(define (pi-sum a b)
  (sum (lambda (i) (/ 1 (* i (+ i 2))))
       a
       (lambda (i) (+ i 4))
       b))

(define (sum-iter term a next b)
  (define (iter j ans)
    (if (> j b)
        ans
        (iter (next j)
              (+ (term j) ans))))
  (iter a 0))

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt x)
  (fixed-point (lambda (y) (average (/ x y) y))
               1))

(define (fixed-point f start)
  (define tolerance 0.00001)
  (define (close-enuf? u v)
    (< (abs (- u v)) tolerance))
  (define (iter old new)
    (if (close-enuf? old new)
        new
        (iter new (f new))))
  (iter start (f start)))

(define (sqrt-damp a)
  (fixed-point
   (average-damp (lambda (b) (/ a b)))
   1))

(define (average-damp f)
  (lambda (x) (average (f x) x)))

(define (sqrt-newton x)
  (newton (lambda (y) (- x (* y y)))
          1))

(define (newton f guess)
  (define df (deriv f))
  (fixed-point
   (lambda (x) (- x (/ (f x) (df x))))
   guess))

(define deriv
  (lambda (f)
    (define dx 0.0001)
    (lambda (x)
      (/ (- (f (+ x dx))
            (f x))
         dx))))