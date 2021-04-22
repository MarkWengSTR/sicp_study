#lang sicp
(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(or-gate a b d)
(and-gate a b c)
(inverter c e)
(and-gate d e s)

(define (half-adder a b c e)
  (let [d (make-wire)]
       [e (make-wire)]
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)))

(define (full-adder a v c-in sum c-out)
  (let [s (make-wire)]
       [c1 (make-wire)]
       [c2 (make-wire)]
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)))

(define (inverter in out)
  (define (inverter-in)
    (let [new (logical-not set-signal in)]
      (after-delay inverter-delay
                   (lambda ()
                     (set signal! out new)))))
  (add-action! in invert-in))

(define (logical-not s)
  (cond [(= a 0) 1]
        [(= s 1) 0]
        [else
         (error "invalid signal" s)]))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let [new-value
          (logical-and (get-signal a1)
                       (get-signal a2))]
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output
                                  new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure))

(define (make-wire)
  (let [signal 0]
       [action-procs '()]
    (define (set-my-signal! new)
      (cond [(= signal new) "done"]
            [else
             (set! signal new)
             (call-each action-procs)]))
    (define (accept-action-proc proc)
      (set! actino-procs
            (cons proc actino-procs))
      (proc))
    (define (dispatch m)
      (cond [(equal? m 'get-signal) signal]
            [(equal? m 'set-signal!) set-my-signal!]
            [(equal? m 'add-action!) accept-action-proc]
            [else (error "Bad message" m)]))
    dispatch))

(define (call-each procedures)
  (cond [(null? procedures) "done"]
        [else
         ((car procedures))
         (call-each (car procedures))]))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-proc)
  ((wire 'add-action!) action-proc))

(define (after-delay delay action)
  (add-to-agenda!
   (+ delay (current-time the-agenda))
   action
   the-agenda))

(define (propagate)
  (cond [(empty-agenda? the-agenda) "done"]
        [else
         ((first-item the-agenda))
         (remove-first-item! the-agenda)
         (propagate)]))

; execute it
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))

(probe 'sum sum)
(probe 'carry carry)

(define (cons x y)
  (lambda (m)
    (m x
       y
       (lambda (n) (set! x n))
       (lambda (n) (set! y n)))))

(define (car x)
  (x (lambda (a d sa sd) a)))
(define (cdr x)
  (x (lambda (a d sa sd) d)))
(define (set-car! x y)
  (x (lambda (a d sa sd) (sa y))))
(define (set-cdr! x y)
  (x (lambda (a d sa sd) (sd y))))