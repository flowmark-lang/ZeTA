#lang typed/racket/base
(provide (all-defined-out))

(: require-all (All (A) (-> (-> A Boolean) (Listof A) Boolean)))
(define (require-all f l)
  (cond ((equal? l '()) #t)
        ((not (f (car l))) #f)
        (else (require-all f (cdr l)))))

(: require-any (All (A) (-> (-> A Boolean) (Listof A) Boolean)))
(define (require-any f l)
  (cond ((equal? l '()) #f)
        ((not (f (car l))) (require-any f (cdr l)))
        (else #t)))

