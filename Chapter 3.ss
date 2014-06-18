#lang racket
(define rember
  (λ (a lat)
    (cond
      [(null? lat) '()]
      [(eq? a (car lat)) (cdr lat)]
      [else (cons (car lat) (rember a (cdr lat)))])))

(define firsts
  (λ (l)
    (cond
      [(null? l) '()]
      [else (cons (caar l) (firsts (cdr l)))])))

(define insertR
  (λ (new old lat)
    (cond 
      [(null? lat) '()]
      [(eq? old (car lat)) (cons old (cons new (cdr lat)))]
      [else (cons (car lat) (insertR new old (cdr lat)))])))

(define insertL
  (λ (new old lat)
    (cond 
      [(null? lat) '()]
      [(eq? old (car lat)) (cons new (cons old (cdr lat)))]
      [else (cons (car lat) (insertL new old (cdr lat)))])))

(define subst
  (λ (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? old (car lat)) (cons new (cdr lat))]
      [else (cons (car lat) (subst new old (cdr lat)))])))