#lang racket/base

#|
transposition (A-transpose, A) <- a(i,j) = a-T(j,i)

|#

(require rackunit
         rackunit/text-ui
         racket/vector
         racket/contract)

(provide transpose
         transpose-with-headers)

(define/contract (transpose M)
  (-> (vectorof vector?) (vectorof vector?))
  ;vector-length M = cols(M)
  ;(vector-length (vector-ref M 0) = Rows (M)
  (for/vector [(c-idx (in-range (vector-length (vector-ref M 0))))]
    (for/vector ([r-idx (in-range (vector-length M))])
      ;(vector-ref (vector-ref M col) row)
      (vector-ref (vector-ref M r-idx) c-idx))))

(define (void->null val)
  (if (void? val)
      null
      val))


(define/contract (transpose-with-headers fields M)
  (-> (vectorof symbol?) (vectorof vector?) (listof (listof pair?)))
  ;vector-length M = cols(M)
  ;(vector-length (vector-ref M 0) = Rows (M)
  (for/list [(c-idx (in-range (vector-length (vector-ref M 0))))]
    (for/list ([r-idx (in-range (vector-length M))])
      ;(vector-ref (vector-ref M col) row)
      (cons (vector-ref fields r-idx) (void->null (vector-ref (vector-ref M r-idx) c-idx))))))



(define internals
  (test-suite
   "Tests for matrix-transpose"
   (let ((M (vector (vector 1)))
         (N (vector (vector 1 2)))
         (O (vector (vector 1 2) (vector 'a 'b)))
         (P (vector (vector 1 'a) (vector 2 'b))))
     (test-equal? "transpose single element" (vector (vector 1)) (transpose M))
     (test-equal? "transpose two element" (vector (vector 1) (vector 2)) (transpose N))
     (test-equal? "transpose two element" P  (transpose O))
     (test-equal? "transpose two element" O  (transpose P))
     (test-equal? "transpose two element" '(((pkid . 1) (code . a)) ((pkid . 2) (code . b)))  (transpose-with-headers (vector 'pkid 'code) O)))))
     
    
    
  
