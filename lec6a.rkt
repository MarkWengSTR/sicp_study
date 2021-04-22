#lang sicp
; sum odd squares in tree
(define (sum-odd-squares tree)
  (if (leaf-node? tree)
      (if (odd? tree)
          (square tree)
          0)
      (+ (sum-odd-squares
          left-branch tree)
         (sum-odd-squares
          (right-branch tree)))))

; for integer, k, compute the k fibonacci number
; and pick the odd items, assemble them into a list
(define (odd-fibs n)
  (define (next k)
    (if (> k n)
        '()
        (let [f (fib k)]
          (if (odd? f)
              (cons f (next (inc k)))
              (next (inc k))))))
  (next 1))

; constructer of stream
(cons-stream x y)
;selector of stream
(head s)
(tail s)
; for any x, y
(head (cons-stream x y)) ; -> x
(tail (cons-stream x y)) ; -> y
; also have a emtpy-stream

(define (map-stream proc s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream
       (proc (head s))
       (map-stream proc (tail s)))))

(define (filter pred s)
  (cond
    [(empty-stram? s) the-empty-stream]
    [(pred (head s))
     (cons-stream (head s)
                  (filter pred
                          (tail s)))]
    [else (filter pred (tail s))]))

(define (accumulate combiner init-val s)
  (if (empty-stream? s)
      init-val
      (combiner (head s)
                (accumulate combiner
                            init-val
                            (tail s)))))

(define (enumerate-tree tree)
  (if (leaf-node? tree)
      (cons-stream tree
                   the-empty-stream)
      (append-streams
       (enumerate-tree
        (left-branch tree))
       (enumerate-tree
        (right-branch tree)))))

(define (append-streams s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream
       (head s1)
       (append-streams (tail s1)
                       s2))))

(define (enum-interval low high)
  (if (> low height)
      the-empty-stream
      (cons-stream
       low
       (enum-interval (inc low) high))))

; use stream for sum-odd-square
(define (sum-odd-squares-stream tree)
  (accumulate
   +
   0
   (map
    sqr
    (filter odd
            (enumerate-tree tree)))))

(define (odd-fibs-stream n)
  (accumulate
   cons
   '()
   (filter
    odd
    (map fib (enum-interval 1 n)))))

;flatten nest 1rd stream
(define (flatten st-of-st)
  (accumulate append-streams
              the-empty-stream
              st-of-st))
(define (flatmap f s)
  (flatten (map f s)))

;Given n: find all pair 0<j<i<=n, such that i+j is prime
(define (prime-sum-pairs n)
  (map
   (lambda (p)
     (list (car p)
           (cadr p)
           (+ (car p) (cadr p))))
   (filter
    (lambda (p)
      (prime? (+ (car p) (cadr p))))
    (flatmap
     (lambda (i)
       (map
        (lambda (j) (list i j))
        (enum-interval 1 (dec i))))
     (enum-interval 1 n)))))

; rewrite in collect
(define (prime-sum-pairs n)
  (collect
   (list i j (+ i j))
   ([i (enum-interval 1 n)]
   [j (enum-interval 1 (dec i))])
   (prime? (+ i j))))

;eight queens problem (backtracking search -> recursive strategy)
;support we has a safe? procedure
;(safe <row> <col> <rest-of-position>)

(define (queens size)
  (define (fill-cols k)
    (if (= k 0)
        (singleton empty-board)
        (collect
         (adjoin-postion try-row
                         k
                         rest-queens)
         ([rest-queens (fill-cols (dec k))]
          [try-row (enum-interval 1 size)])
         (safe? try-row k rest-queens))))
  (fill-cols size))

; what stream realy are?
; (cons-stream x y) -> abbreviation for (cons x (dely y))
; (head s) -> (car s)
; (tail s) -> (force (cdr s))
; (delay <exp>) -> abbrev for (lambda () <exp>)
; (force p) = (p)

; To solve (tail (tail (tail...)))
; each new tail execute, will recalculate first item
; so modify the delay to (memo-proc (lambda () <exp>))
(define (memo-proc proc)
  (let [already-run? nil]
       [result nil]
    (lambda ()
      (if (not already-run?)
          (sequence
            (set! result (proc))
            (set! already-run? (not nil))
            result)
          result))))