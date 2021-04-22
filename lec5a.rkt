#lang sicp
; Assignment
; <before>
; (set! <var> <value>)
; <after>

(define count 1)
(define (demo x)
  (set! count (inc count))
  (+ x count))
;=> (demo 3)
;=> 5
;=> (demo 3)
;=> 6
; express "demo" is not a function
; substitute model fail
; arguments are not represent value, are represent the location of value storage

;functional version
(define (fact n)
  (define (iter m i)
    (cond [(> i n) m]
          [else (iter (* i m) (+ i 1))]))
  (iter 1 1))

;imperative version
(define (fact-imp n)
  (let ([i 1]
        [m 1])
    (define (loop)
      (cond [(> i n) m]
            [else
             (set! m (* i m))
             (set! i (+ i 1))
             (loop)]))
    (loop)))

(fact 5)
(fact-imp 5)

; Environment model
; Bounce Variables
(lambda (y) ((lambda (x) (* x y)) 3))
(lambda (y) ((lambda (z) (* z y)) 3))
;(lambda (x) (* x y)) ; y is free variables

(define make-counter
  (lambda (n)
    (lambda ()
      (set! n (inc n))
      n)))
(define c1 (make-counter 0))
(define c2 (make-counter 10))

; cesaro's method for estimating pi:
; prob(gcd(n1, n2)=1) = 6/(pi*pi)

; monte-carlo with assignment
(define (estimate-pi n)
  (sqrt (/ 6 (monte-carlo n cesaro))))

(define (cesaro)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter remaining passed)
    (cond [(= remaining 0)
           (/ passed trials)]
          [(experiment)
           (iter (dec remaining)
                 (inc passed))]
          [else
           (iter (dec remaining)
                 passed)]))
  (iter trials 0))

(define rand
  (let [x random-init]
    (lambda ()
      (set! x (rand-update x))
      x)))

; without assignment
(define (estimate-pi n)
  (sqrt (/ 6 (random-gcd-test n))))

(define (random-gcd-test trials)
  (define (iter remaining passed x)
    (let [x1 (rand-update x)]
      (let [x2 (rand-update x1)]
        (cond [(= remaining 0)
               (/ passed trials)]
              [(= (gcd x1 x2) 1)
               (iter (dec remaining)
                     (inc passed)
                     x2)]
              [else
               (iter (inc remaining)
                     passed
                     x2)])))))