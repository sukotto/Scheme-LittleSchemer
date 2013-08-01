#lang racket
(provide rember)
(provide firsts)
(provide insertR)
(provide insertL)
(provide subst)
(provide subst2)
(provide multirember)
(provide multiinsertR)
(provide multiinsertL)
(provide multisubst)

(define (rember a lat)
  (cond((null? lat)'())
       ((eq? a (car lat))(cdr lat))
       (else (cons (car lat)(rember a (cdr lat))))))

(define (firsts l)
  (cond((null? l)'())
       ((list? (car l))(cons (car(car l))(firsts (cdr l))))
       (else '())))

(define (insertR new old lat)
  (cond ((null? lat) '())
        ((eq? (car lat) old)(cons old (cons new (cdr lat))))
        (else (cons (car lat)(insertR new old (cdr lat))))))
     
(define (insertL new old lat)
  (cond ((null? lat) '())
        ((eq? (car lat) old)(cons new (cons old (cdr lat))))
        (else (cons (car lat)(insertL new old (cdr lat))))))

(define (subst new old lat)
  (cond((null? lat)'())
       ((eq? (car lat) old)(cons new (cdr lat)))
       (else (cons (car lat)(subst new old (cdr lat))))))

(define (subst2 new o1 o2 lat)
  (cond ((null? lat)'())
        ((or(eq? (car lat) o1)
            (eq? (car lat) o2))
            (cons new (cdr lat)))
        (else (cons (car lat)(subst2 new o1 o2 (cdr lat))))))

(define (multirember a lat)
  (cond((null? lat)'())
       ((eq? (car lat) a)(multirember a (cdr lat)))
       (else (cons (car lat)(multirember a (cdr lat))))))


(define (multiinsertR new old lat)
  (cond ((null? lat)'())
        ((eq? (car lat)old)
           (cons old (cons new(multiinsertR new old (cdr lat)))))
        (else (cons (car lat)(multiinsertR new old (cdr lat))))))

(define (multiinsertL new old lat)
  (cond ((null? lat)'())
        ((eq? (car lat)old)
           (cons new (cons old(multiinsertL new old (cdr lat)))))
        (else (cons (car lat)(multiinsertL new old (cdr lat))))))

(define (multisubst new old lat)
  (cond((null? lat)'())
       ((eq? (car lat) old)
           (cons new (multisubst new old(cdr lat))))
       (else (cons (car lat)(multisubst new old (cdr lat))))))
     