#lang sicp
(lambda (x . y)
  (map (lambda (u) (* u x))
       y))