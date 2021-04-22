#lang sicp
(define (average x y) (/ (+ x y) 2))
(define (+rat x y)
  (make-rat
   (+ (* (numer x) (denom y))
      (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (*rat x y)
  (make-rat
   (* (numer x) (numer y))
   (* (denom x) (denom y))))

(define (make-rat n d)
  (let ([g (gcd n d)])
   (cons (/ n g)
         (/ d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define a (make-rat 1 2))
(define b (make-rat 1 4))
(define ans (+rat a b))
(numer ans)
(denom ans)

(define (make-vector x y) (cons x y))
(define (xcor p) (car p))
(define (ycor p) (cdr p))

(define (make-seg p q) (cons p q))
(define (seg-start s) (car s))
(define (seg-end s) (cdr s))

(define (midpoint s)
  (let [(a (seg-start s))
        (b (seg-end s))]
    (make-vector
     (average (xcor a) (xcor b))
     (average (ycor a) (ycor b)))))

(define (length s)
  (let [(dx (-
            (xcor (seg-start s))
            (xcor (seg-end s))))
        (dy (-
             (ycor (seg-start s))
             (ycor (seg-end s))))]
    (sqrt (+ (* dx dx)
             (* dy dy)))))

(define (cons-cus x y)
  (lambda (pick)
    (if (= pick 1) x
        y)))
(define (cor-cus cons-f)
  (cons-f 1))
(define (cdr-cus cons-f)
  (cons-f 2))

(define a-cons (cons-cus 6 7))
(cor-cus a-cons)
(cdr-cus a-cons)