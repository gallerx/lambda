
#|-------------------------------------------------------------------------------
finally executes a series of potentially error-generating functions, reporting the error
yet continuing until the end of the series.

Very useful for cleanup upon exiting a state

usage: (finally fn1 fn2 fn3...)
example: (finally (error "cheese") (error "whiz") (error "is") (error "fun"))
output: 
cheese
whiz
is 
fun
-------------------------------------------------------------------------------|#

(define-syntax-rule (finally fn ...)
  (let ((k-last #f))
    (with-handlers ([exn:fail? (λ (exn) (printf "~A\n" (exn-message exn)) (k-last))])
      (let/cc k (set! k-last k) fn)
      ...)))
      
