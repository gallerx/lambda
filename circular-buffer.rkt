#lang racket/base

(provide circular-buffer)

#|
------------------------------------------------------------------------------------------------
implements a circular buffer as a closure
See: http://en.wikipedia.org/wiki/Circular_buffer

N.B. full? empty? this definition of full/empty results in length-1 usable slots)

usage: (circular-buffer n) 

throws error when return to full buffer or extract from empty buffer

------------------------------------------------------------------------------------------------
|#

(define (circular-buffer length)
  (let* ((lock (make-semaphore 1))
         (buffer (build-vector length values))
         (start-ptr 0)
         (end-ptr (sub1 length))
         (circ-add (lambda (ptr) (modulo (add1 ptr) length)))
         (full? (lambda () (eq? (circ-add end-ptr) start-ptr)))
         (empty? (lambda () (eq? start-ptr end-ptr))))
    (values
     (lambda () 
       (call-with-semaphore lock
                            (λ _
                              (if (empty?)
                                 (error 'empty-connection-buffer)
                                  (begin0
                                    (vector-ref buffer start-ptr)
                                    (set! start-ptr (circ-add start-ptr)))))))
     (lambda (returned)
       (call-with-semaphore lock
                            (λ _ 
                              (if (full?)
                                  (error 'full-connection-buffer)
                                  (begin
                                    (set! end-ptr (circ-add end-ptr))
                                    (vector-set! buffer end-ptr returned)))))))))
     
