#lang racket/base

(require "common-pre-reader.rkt"
         (for-syntax  syntax/parse 
                     racket/base 
                     "hex-utilities.rkt"))


(provide define  ;to allow defining an export function
         provide ;to allow exports
         parse-prompt
         test
         eval
         read-all
         define-namespace-anchor
         namespace-anchor->namespace
         #%top-interaction
         #%datum
         #%app
         #%module-begin)
         
(begin-for-syntax

(define-syntax-class verb
  #:description "verb"
  (pattern (~or (~literal scan))))

(define-syntax-class xpi
  #:description "exact-positive-integer"
  (pattern x:number
           #:fail-unless (exact-positive-integer? (syntax->datum #'x))
           "exact-positive-integer required"))
               

(define scancode-pregex (pregexp "^([X|N|O])HA([0-9a-fA-F]{8})$"))

(define-syntax-class data
  (pattern candidate:id
           #:with (_ type:str maybe-pkid:str) #`#,(regexp-match scancode-pregex
                                                               (symbol->string (syntax->datum #'candidate)))
           ;post-processing of attributes
           #:with (symbolic-type:id pkid:xpi) #`#,(list  (string->symbol (syntax->datum #'type))
                                                         (hex-string->pkid (syntax->datum #'maybe-pkid)))))

#|
Name: static-prompt-parser
GRAMMAR 
sentence

sentence <- verb

sentence <- verb data

|#

(define-splicing-syntax-class sentence
  #:description "sentence"
  #:attributes (ast) ;abstract-parse-tree
  (pattern (~seq v:verb d:data)
            #:with ast #'((verb . v) (data . (d.symbolic-type . d.pkid)))))

)
#|
Note that we quote the x.ast to get a quoted list, which won't eval

|#

(define-syntax (parse-prompt stx)
  (syntax-parse stx
                ((_ x:sentence) #''x.ast)
                ;escape used for testing, 
                [(_ (~literal esc)) #''((verb . esc))]
                [(_ (~literal retry)) #''((verb . retry))]
                [(_ (~literal reset)) #''((verb . reset))]
                (_ #'null)))

(define (test sz)
  (syntax->datum (expand (datum->syntax #f (read-all sz)))))
