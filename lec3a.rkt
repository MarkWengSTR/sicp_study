#lang sicp
(define make-vector cons)
(define xcor car)
(define ycor cdr)

(define (+vect v1 v2)
  (make-vector
   (+ (xcor v1) (xcor v2))
   (+ (ycor v1) (ycor v2))))

(define (scale s v)
  (make-vector (* s (xcor v))
               (* s (ycor v))))

(define make-segment cons)
(define seg-start car)
(define seg-end cdr)

(define list-example (list 1 2 3 4))

(define (scale-list s l)
  (if (null? l)
      nil
      (cons (* (car l) s)
            (scale-list s (cdr l)))))

(scale-list 10 list-example)

(define (general-map p l)
  (if (null? l)
      nil
      (cons (p (car l))
            (general-map p (cdr l)))))

(define (scale-list-by-map s l)
  (general-map
   (lambda (item) (* item s)) l))

(scale-list-by-map 10 list-example)
(general-map (lambda (item) (* item item)) list-example)

(define (general-for-each p l)
  (cond [(null? l) "done"]
        [else (p (car l))
              (general-for-each p (cdr l))]))
(general-for-each (lambda (item) (* 10 item)) list-example)

(define (repeat-cus proc n)
  (lambda (item)
    (cond [(= n 0) (proc item)]
          [else (proc item)
                (repeat-cus proc (- n 1))])))

((repeat-cus write 5) "go")

