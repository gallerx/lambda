#lang s-exp "minimal-REPL-for-DSL-inner.rkt"

(provide REPL)

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))


(define (REPL sz)
  (eval (read-all  sz) ns))


