#lang sicp
(define eval
  (lambda (exp env)
    (cond
      ; 3 -> 3
      [(number? exp) exp]
      ; X -> 3
      [(symbol? exp) (lookup exp env)]
      ; 'foo -> (quote foo) -> foo
      [(equal? (car exp) 'quote) (cadr exp)]
      ; (lambda (x) (+ x y)) -> (closure ((x) (+ x y)) <env>
      [(equal? (car exp) 'lambda)
           (list 'closure (cdr exp) env)]
      ; (cond (p1 e1) (p2 e2))
      [(equal? (car exp) 'cond)
           (evcond (cdr exp) env)]
          [else (apply (eval (car exp) env)
                       (evlist (cdr exp) env))])))
(define apply
  (lambda (proc args)
    (cond [(primitive? proc)
           (apply-primop proc args)]
;(lambda (x) (+ x y)) -> (closure ((x) (+ x y)) <env>)
          [(equal? (car exp) 'closure)
           (eval (cadadr proc)
                 (bind (caadr proc)
                       args
                       (caddr proc)))]
          [else "error"])))

(define evlist
  (lambda (l env)
    (cond [(equal? l '()) '()]
          [else
           (cons (eval (car l) env)
                 (evlist (cdr l) env))])))

(define evcond
  (lambda (clauses env)
    (cond [(equal? clauses '()) '()]
          [(equal? (caar clauses) 'else)
           (eval (cadar clauses) env)]
          [(false? (eval (caar clauses) env))
           (evcond (cdr clauses) env)]
          [else
           (eval (cadar clauses) env)])))

(define bind
  (lambda (vars vals env)
    (cons (pair-up vars vals)
          env)))

(define pair-up
  (lambda (vars vals)
    (cond [(equal? vars '())
           (cond [(equal? vals '()) '()]
                 [else (error "too many args")])]
          [(equal? vals '()) (error "too few args")]
          [else
           (cons (cons (car vars)
                       (car vals))
                 (pair-up (cdr vars)
                          (cdr vals)))])))

(define lookup
  (lambda (sym env)
    (cond [(equal? env '()) (error "unbound variable")]
          [else
           ((lambda (vcell)
              (cond [(equal? vcell '())
                     (lookup sym
                             (cdr env))]
                    [else (cdr vcell)]))
            (assq sym (car env)))])))

(define assq
  (lambda (sym alist)
    (cond [(equal? alist '()) '()]
          [(equal? sym (caar alist))
           (car alist)]
          [else
           (assq sym (cdr alist))])))

(eval '(((lambda (x) (lambda (y) (+ x y))) 3)4) env)
(apply (eval '((lambda (x) (lambda (y) (+ x y))) 3) e0)
       (evist '(4) e0))
(apply (eval '((lambda (x) (lambda (y) (+ x y))) 3) e0)
       (cons (eval '4 e0)))
(apply (eval '((lambda (x) (lambda (y) (+ x y))) 3) e0)
       (cons 4 '()))
(apply (eval '((lambda (x) (lambda (y) (+ x y))) 3) e0)
       '(4))
(apply (apply (eval '((lambda (x) (lambda (y) (+ x y)))
                      '(3)))
              '(4)))

(define (expt)
  (lambda (x n)
    (cond [(= n 0) 1]
          [else
           (* x (expt x (- n 1)))])))