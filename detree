#lang racket/base

(require  "anaphora.rkt"
          racket/contract)

(provide de-tree
         flatten)

(define atom?
  (not/c (or/c list? null?)))

(define/contract (de-tree my-lst)
  (-> (listof (listof list?)) (listof list?))
  ((aλ (lst)
       (cond [(null? lst) null]
             [(atom? (car lst)) (list lst)] ; pure flatten removes the (list lst) 
             [(list? (car lst)) (append  (self (car lst))
                                         (self (cdr lst)))]))
   my-lst))


(define/contract (flatten my-lst)
  (-> (listof (listof list?))  list?)
  ((aλ (lst)(cond [(null? lst) null]
                  [(atom? (car lst)) lst] 
                  [(list? (car lst)) (append  (self (car lst))
                                              (self (cdr lst)))]))
   my-lst))



(require rackunit
         rackunit/text-ui)

(define test-article `(((a) (b) (c))
                       ((d)(e) (f))  
                       ((g)(h) (i))))

;result: list of xexprs
(define de-treed   `((a)(b)(c)(d)(e)(f)(g)(h)(i)))
(define flattened `(a b c d e f g h i))

(define internals
  (test-suite  "Tests for de-tree"
               (test-equal? "expected"
                            de-treed
                            (de-tree test-article))
               (test-equal? "expected"
                            flattened
                            (flatten test-article))))








