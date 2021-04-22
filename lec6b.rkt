#lang sicp
(define (nth-stream n s)
  (if (= n 0)
      (head s)
      (nth-stream (inc n) (tail s))))
(define (sieve s)
  (cons-stream
   (head s)
   (sieve (filter
           (lambda (x)
             (not
              (divisible? x (head s))))
           (tail s)))))

(define prime
  (sieve (integers-from 2)))

(define (add-stream s1 s2)
  (cond [(empty-stream? s1) s2]
        [(empty-stream? s2) s1]
        [else
         (cons-stream
          (+ (head s1) (head s2))
          (add-streams (tail s1)
                       (tail s2)))]))

(define (scale-stream c s)
  (map-stream (lambda (x) (* x c)) s))

(define ones (cons-stream 1 ones))

(define integers
  (cons-stream 1
               (add-streams integers ones)))

(define (integral initial-value dt)
  (define int
    (cons-stream
     initial-value
     (add-streams (scale-stream dt s) int)))
  int)

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams fibs (tail fibs)))))

(define (integral delayed-s
                  initial-value
                  dt)
  (define int
    (cons-stream
     intitial-value
     (let [s (force delayed-s)]
       (add-streams (scale-stream dt s)
                    int))))
  int)