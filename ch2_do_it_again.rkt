#lang racket
(provide lat?)
(require "ch1_toys.rkt")
(define lat?
  (lambda(l)
    (cond
      ((null? l) #t)
      ((atom? (car l))(lat? (cdr l)))
      (else #f))))
    