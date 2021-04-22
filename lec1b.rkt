#lang sicp
(define (sos x y)
  (+ (sq x) (sq y)))
(define (sq x)
  (* x x))

(define (cus-add x y)
  (if (= x 0)
      y
      (cus-add (- x 1) (+ y 1))))

(cus-add 3 5)

(define (cus-add-2 x y)
  (if (= x 0)
      y
      (+ 1 (cus-add-2 (- x 1) y))))

(cus-add-2 5 1)

(define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(fib 13)

(define (fib-iter n)
  (define (fib-accu n begin result)
    (if (= n 0)
        result
        (fib-accu (dec n) result (+ begin result))))
  (fib-accu n 1 0)
  )
(fib-iter 2)
(define (move n from to spare)
  (cond [(= n 0) "Done"]
        [else
         (move (dec n) from spare to)
         (move from to)
         (move (dec n) spare to from)]))
