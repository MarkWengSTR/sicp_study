#lang sicp
(define (deriv exp var)
  (cond [(constant? exp var) 0]
        [(same-var? exp var) 1]
        [(sum? exp)
         (make-sum (deriv (a1 exp) var)
                   (deriv (a2 exp) var))]
        [(product? exp)
         (make-sum
          (make-product (m1 exp)
                        (deriv (m2 exp) var))
          (make-product (deriv (m1 exp) var)
                        (m2 exp))
          )]))

(define (constant? exp var)
  (and (atom? exp)
       (not (equal? exp var))))

(define (same-var? exp var)
  (and (atom? exp)
       (equal? exp var)))

(define (sum? exp)
  (and (not (atom? exp))
       (equal? (car exp) '+)))

(define (make-sum a1 a2)
  (list '+ a1 a2))
(define a1 cadr)
(define a2 caddr)

(define (product? exp)
  (and (not (atom? exp))
       (equal? (car exp) '*)))
(define (make-product m1 m2)
  (list '* m1 m2))
(define m1 cadr)
(define m2 caddr)

(define (make-sum-2)
  (cond [(and (number? a1)
              (number? a2))
         (+ a1 a2)]
        [(and (number? a1) (= a1 0)) a2]
        [(and (number? a2) (= a2 0)) a1]
        [else (list '+ a1 a2)]))