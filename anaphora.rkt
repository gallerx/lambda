#lang racket/base

(require (for-syntax racket/base))

(provide aif
         aif!null
         xif 
         aλ
         awhen
         aunless
         awith
         acond
         astring?)

(define-syntax (aif stx)
  (syntax-case stx ()
    [(_ test-form true-form false-form)
     (with-syntax ([it (datum->syntax stx 'it)])
       #'(let ([it test-form])
           (if it
               true-form
               false-form)))]))

(define-syntax (aif!null stx)
  (syntax-case stx ()
    [(_ test-form true-form false-form)
     (with-syntax ([it (datum->syntax stx 'it)])
       #'(let ([it  test-form])
           (if (not (null? it))
               true-form
               false-form)))]))

(define-syntax (astring? stx)
  (syntax-case stx ()
    [(_ test-form true-form false-form)
     (with-syntax ([it (datum->syntax stx 'it)])
       #'(let ([it test-form])
           (if (string? it)
               true-form
               false-form)))]))

(define-syntax (awhen stx)
  (syntax-case stx ()
    [(_ test-form true-form)
     (with-syntax ([it (datum->syntax stx 'it)])
       #'(let ([it test-form])
           (when it
               true-form)))]))
               
(define-syntax (awith stx)
  (syntax-case stx ()
    [(_ bind-form body-form ...)
     (with-syntax ([it (datum->syntax stx 'it)])
       #'(let ([it bind-form])
           body-form ...))]))


(define-syntax (aunless stx)
  (syntax-case stx ()
    [(_ test-form false-form)
     (with-syntax ([it (datum->syntax stx 'it)])
       #'(let ([it test-form])
           (unless it
               false-form)))]))

(define-syntax (xif stx)
  (syntax-case stx ()
    [(_ test-form noerror-form error-form)
     (with-syntax ([it (datum->syntax stx 'it)]
                   [error? (datum->syntax stx 'error?)])
       #'(let-values ([(error? it) (with-handlers  ([exn:fail? (λ (exn)
                                                                 (values #t (format "~A\n" (exn-message exn))))])
                                     (values #f test-form))])
        (if error?
            error-form 
            noerror-form)))]))

(define-syntax (aλ stx)
   (syntax-case stx ()
    [(_ (args ...) body ...)
    (with-syntax ([self (datum->syntax stx 'self)])
      #'(letrec ((self (λ (args ...)
                    body ...)))
          self))]
    [(_ _ body ...)
    (with-syntax ([self (datum->syntax stx 'self)])
      #'(letrec ((self (λ ()
                    body ...)))
          self))]))

(define-syntax (acond stx)
  (syntax-case stx (else)
    [(_ test-form [e f] ... else final)
     (with-syntax ([it (datum->syntax stx 'it)])
       #'(let ([it test-form])
           (cond 
               [e f] 
                ...
               [else final])))]
    [(_ test-form [e f] ...)
     (with-syntax ([it (datum->syntax stx 'it)])
       #'(let ([it test-form])
           (cond 
             [e f] 
              ...)))]))
