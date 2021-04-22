#lang sicp

(define (sqr x) (* x x))

(define (+c z1 z2)
  (make-rectangular
   (+ (real-part z1) (real-part z2))
   (+ (imag-part z1) (imag-part z2))))
(define (-c z1 z2)
  (make-rectangular
   (- (real-part z1) (real-part z2))
   (- (imag-part z1) (imag-part z2))))

(define (*c z1 z2)
  (make-polar
   (* (magnitude z1) (magnitude z2))
   (+ (angle z1) (angle z2))))

(define (/c z1 z2)
  (make-polar
   (/ (magnitude z1) (magnitude z2))
   (- (angle z1) (angle z2))))

; Representing complex numbers as
; pairs real-part, imag-part
(define (make-rectangular x y)
  (attach-type 'rectangular (cons x y)))

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))

(define (make-polar r a)
  (cons (* r (cos a)) (* r (sin a))))

(define (magnitud-rectangular z)
  (sqrt (+ (sqr (car z))
           (sqr (cdr z)))))
(define (angle-rectangular z)
  (atan (cdr z) (car z)))

; Representing complex numbers as
; pairs magnitude, angle
(define (make-polar r a) (attach-type 'polar (cons r a)))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-rectangular x y)
  (cons (sqrt (+ (sqr x) (sqr y)))
        (atan y x)))
(define (real-part-polar z)
  (* (car z) (cos (cdr z))))
(define (imag-part-polar z)
  (* (car z) (sin (cdr z))))

; Support mechanism for manifest types
(define (attach-type type contents)
  (cons type contents))
(define (type datum)
  (car datum))
(define (contents datum)
  (cdr datum))

;type predicates
(define (rectangular? z)
  (eq? (type z) 'rectangular))
(define (polar? z)
  (eq? (type z) 'polar))

(define (real-part z)
  (cond [(rectangular? z)
         (real-part-rectangular
          (contents z))]
        [(polar? z)
         (real-part-polar
          (contents z))]))
(define (imag-part z)
  (cond [(rectangular? z)
         (imag-part-rectangular
          (contents z))]
        [(polar? z)
         (imag-part-polar
          (contents z))]))
; magnitude and angle follow same rule

; data structure table
;   apply method
;(put key1 key2 value)
;(put "type" "procedure-name" "procedure")
;(get "type" "procedure-name")

;for rectangular expression
(put 'rectangular 'real-part
     real-part-rectangular)
(put 'rectangular 'imag-part
     imag-part-rectangular)
(put 'rectangular 'magnitude
     magnitude-rectangular)
(put 'rectangular 'angle
     angle-rectangular)
;for polar expression
(put 'polar 'real-part real-part-polar)
(put 'polar 'imag-part imag-part-polar)
(put 'polar 'magnitude magnitude-polar)
(put 'polar 'angle angle-polar)

(define (operate op obj)
  (let [proc (get (type obj) op)]
    (if (not (null? proc))
        (proc (contents obj))
        (error "undefined op"))))

;Data-Direct programming
;(real-part z)->
;(operate 'real-part z)->
;((get 'polar 'real-part) (contents z))
;(real-part-polar (contents z))

;general operators
;add sub mul div

;add rational calculate
(define (make-rat x y)
  (attach-type 'rational (cons x y)))

(put 'rational 'add +rat)
(put 'rational 'sub -rat)
(put 'rational 'mul *rat)
(put 'rational 'div /rat)

(define (add x y)
  (operate-2 'add x y))

(define (operate-2 op arg1 arg2)
  (if
   (eq? (type arg1) (type arg2))
   (let [proc (get (type arg1) op)]
     (if (not (null? proc))
         (proc (contents arg1)
               (contents arg2))
         (error
          "Op undefined on type")))
   (error "Args not same type")))

(define (make-complex z)
  (attach-type 'complex z))
(define (+complex z1 z2)
  (make-complex (+c z1 z2)))
(put 'complex 'add +complex)
;similarly for -complex *complex /complex

;x^15 + 2x^7 + 5
;(Polynomial x <term-list>)
;<term-list> (order coeff) ((15 1)(7 2)(0 5))

;intalling polynomials
(define (make-polynomial var term-list)
  (attach-type 'polynomial
               (cons var term-list)))

(define (*poly p1 p2)
  (if (same-var? (var p1) (var p2))
      (make-polynomial
       (var p1)
       (+term (term-list p1)
              (term-list p2)))
      (error "Polys not in same var")))

(put 'polynomial 'add +poly)

(define (+terms L1 L2)
  (cond [(empty-termlist? L1) L2]
        [(empty-termlist? L2) L1]
        [else
         (let [t1 (first-term L1)]
              [t2 (first-term L2)]
           (cond
             [(> (order t1) (order t2))
              (adjoin-term
               t1
               (+terms (rest-terms L1) L2))]
             [(< (order t1) (order t2))
              (adjoin-term
               t2
               (+terms L1 (rest-terms L2)))]
             [else
              (adjoin-term
               (make-term (order t1)
                          (add (coeff t1)
                               (coeff t2)))
               (+terms (rest-terms L1)
                       (rest-terms L2)))]))]))