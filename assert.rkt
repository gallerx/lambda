#lang racket/base

#|-----------------------------------------------------------------------------------------
Functional Assertions raise an exception if test-condition isn't met. Otherwise equivalent
to the idendity function Ix = x 

use prefix A/ when requiring this  module. 

Example:
(prefix-in A/ "assert.rkt")

Because I like the way A/TRUE, A/XPI, A/NOT-NULL A/ONE-ROW scan

then 

(A/XPI (do-something) "I just didn't expect that.")

(A/TRUE (do-something-else) "Gee, that escalated quickly")

------------------------------------------------------------------------------------------#

(require racket/contract)

(provide TRUE
         FALSE
         ONE
         XPI
         STRING
         NOT-NULL
         ONE-ROW)
         
(define SZ-ASSERTION-FAILED "Assertion failed:")

(define/contract (XPI id msg)
    (-> any/c string? exact-positive-integer?)
    (unless (exact-positive-integer? id)
        (raise (exn:fail msg (current-continuation-marks))))
    id)

(define/contract (STRING id msg)
    (-> any/c string? string?)
    (unless (string? id)
        (raise (exn:fail msg (current-continuation-marks))))
    id)

(define/contract (NOT-NULL id msg)
    (-> any/c string? list?)
    (when (null? id)
        (raise (exn:fail (string-append SZ-ASSERTION-FAILED msg) (current-continuation-marks))))
    id)

(define/contract (TRUE id msg)
    (-> any/c string? any/c)
    (if (eq? #f id)
        (raise (exn:fail (string-append SZ-ASSERTION-FAILED msg) (current-continuation-marks)))
        id))

(define/contract (FALSE id msg)
    (-> any/c string? any/c)
    (if (not (eq? #f id))
        (raise (exn:fail (string-append SZ-ASSERTION-FAILED msg) (current-continuation-marks)))
        id))

(define/contract (ONE id msg)
  (-> any/c string? any/c)
    (if (not (eq? 1 id))
        (raise (exn:fail (string-append SZ-ASSERTION-FAILED msg) (current-continuation-marks)))
        id))

(define null/c (flat-contract null?))

#|
;raises an error if not a single row recordset. Note need null/c defined as flat contract (above) so that order of evaluation proceeds from left to right
The or/c result tests any value by applying the contracts in order, from left to right,
 with the exception that it always moves the non-flat contracts (if any) to the end, 
checking them last. Thus, a contract such as (or/c (not/c real?) positive?) is guaranteed to only invoke the positive? predicate on real numbers.
|#
(define/contract (ONE-ROW id msg)
    (-> any/c string? list?)
    (when ((or/c null/c (not/c list?) (λ (x) (not (= (length x) 1)))) id) 
        (raise (exn:fail (string-append SZ-ASSERTION-FAILED msg) (current-continuation-marks))))
      id)
    
  
    
