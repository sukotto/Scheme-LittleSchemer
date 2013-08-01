#lang racket
(provide atom?)
;;Little Schemer 1. Toys
(define atom?
  (lambda(x)
    (and (not(pair? x))(not (null? x)))))