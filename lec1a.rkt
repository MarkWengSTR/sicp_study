#lang sicp
(define a (* 5 5))
(* a a)
(define b (+ a (* 5 a)))
(+ a (/ b 5))
#(define (square x) (* x x))
(define square (lambda (x) (* x x) ))
(square 10)
(define (average x y)
  (/ (+ x y) 2))
(average 2 4)
(define (abs x)
  (cond ((< x 0) (- x))
        ((= x 0) 0)
        ((> x 0) x)))
(abs 1)
(define (abs-2 x)
  (if (< x 0)
      (- x)
      x))
(abs-2 -1)


#"find fix point"
#"make a guess G"
#"improve the guess by averaging G and x/G"
#"keep improving the guess until it is good enough"
#"use 1 as an initial guess"
(define (try guess x)
  (if (good-enough? guess x)
      guess
      (try (improve guess x) x)))

#"find square-root"
(define (sqrt x) (try 1.0 x))

#"other function"
(define (improve guess x)
  (average guess (/ x guess)))
(define (good-enough? guess x)
  (< (abs (- (square guess) x))
     .001))
(sqrt 4)