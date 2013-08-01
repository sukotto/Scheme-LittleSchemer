#lang racket
;; Chapter 4. Numbers Games - The Little Schemer
;; These functions assume non-negative integers
(provide add1)
(provide sub1)
(provide o+)
(provide o-)
(provide addtup)
(provide o*)
(provide tup+)
(provide o>)
(provide o<)
(provide o=)
(provide ^)
(provide %)
(provide pick)
(provide rempick)
(provide no-nums)
(provide all-nums)
(provide eqan?)
(provide occur)
(provide one?)

(define add1
  (λ (n)
    (+ n 1)))

(define sub1 
  (λ (n)
    (- n 1)))

(define o+
  (λ (n m)
    (cond((zero? m)n)
         (else (o+ (add1 n)(sub1 m))))))

(define o-
  (λ (n m)
    (cond((zero? m) n)
         (else (o- (sub1 n)(sub1 m))))))

(define addtup
  (λ (tup)
    (cond((null? tup)0)
         (else (o+ (car tup) (addtup (cdr tup)))))))

(define o* 
  (λ (n m)
    (cond((zero? m)0)
         (else (o+ n (o* n (sub1 m)))))))

(define tup+
  (λ (tup1 tup2)
    (cond((and (null? tup1)(null? tup2))'())
         ((null? tup1)tup2)
         ((null? tup2)tup1)
         (else (cons (o+ (car tup1)(car tup2)) 
                     (tup+ (cdr tup1)(cdr tup2)))))))

(define o>
  (λ (n m)
    (cond ((zero? n) #f)
          ((zero? m) #t)
          (else (o> (sub1 n)(sub1 m))))))

(define o<
  (λ (n m)
    (cond ((zero? m)#f)
          ((zero? n)#t)
          (else(o< (sub1 n)(sub1 m))))))

(define o=
  (λ (n m)
    (cond ((o> n m) #f)
          ((o< n m) #f)
          (else #t))))
    
(define ^
  (λ (n m)
    (cond((zero? m)1)
         (else (o* n (^ n (sub1 m)))))))

(define % 
  (λ (n m)
    (cond((o< n m)0)
         (else (add1 (% (o- n m) m))))))

(define pick
  (λ (n lat)
    (cond((= n 1) (car lat))
         (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (λ (n lat)
    (cond ((one? n)(cdr lat))
          (else (cons (car lat)(rempick (sub1 n)(cdr lat)))))))

(define no-nums
  (λ (lat)
    (cond((null? lat)'())
         ((number? (car lat))(no-nums (cdr lat)))
         (else (cons (car lat)(no-nums (cdr lat)))))))

(define all-nums
  (λ (lat)
    (cond ((null? lat)'())
          ((number? (car lat))(cons (car lat)(all-nums (cdr lat))))
          (else (all-nums (cdr lat))))))

(define eqan?
  (λ (a b)
   (cond((and(number? a)(number? b))(= a b))
        (else (eq? a b)))))

(define occur
  (λ (a lat)
    (cond((null? lat)0)
         ((eqan? (car lat) a)(add1 (occur a (cdr lat))))
         (else (occur a (cdr lat))))))

(define one?
  (λ (n)
    (= n 1)))
    
    
    
    
    
    
    

