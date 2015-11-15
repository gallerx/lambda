#lang racket/base

(provide make-authenticator-cookie
         kill-authenticator-cookie
         authentic-request?
         authenticate-cookies
         make-cookie-value)

(require web-server/stuffers/hmac-sha1
         web-server/http/bindings
         racket/contract
         web-server/http/request-structs
         net/base64
         racket/list
         racket/match)

;set #t for HTTPS, otherwise #f         
(define CONFIG:secure-mode? #t)

;set the number of hours until cookie expires
(define CONFIG:hours-per-session 4)

;cookie death occurs hours-per-session in the future, as measured in seconds
(define/contract (make-authenticator-cookie key)
  (-> bytes? bytes?)
  (let ((cookie-death (+ (current-seconds) (* 3600 CONFIG:hours-per-session))))
    (string->bytes/utf-8 (format (if CONFIG:secure-mode
                                     "auth=~A; Secure; Version=1;"
                                     "auth=~A;  Version=1;")
                                 (make-cookie-value cookie-death key)))))
        

#|
Replaces existing authenticator-cookie on client side

usage (make-header #Set-cookie (kill-authenticator-cookie))
|#
(define/contract (kill-authenticator-cookie)
  (-> bytes?)
  (string->bytes/utf-8 (if CONFIG:secure-mode 
                           "auth=; Secure; Version=1;"
                           "auth=; Version=1;")))
      

(define/contract (make-cookie-value cookie-death key)
  (-> exact-positive-integer? bytes? bytes?)
  (regexp-replace* #rx"\r\n" 
                   (base64-encode (make-signed-message (string->bytes/utf-8 (number->string cookie-death))
                                                       key))
                   ""))

(define (request-cookies request)
  (filter (λ (x) (eq? (car x) 'cookie)) (request-headers request)))

(define/contract (authentic-request? request  authenticator-key)
  (-> request?  bytes? boolean?)
  (authenticate-cookies (request-cookies request) authenticator-key))

;(parse-auth-cookie #"auth=20302435njnzdsgnzsdgnkz;")          
(define/contract (parse-auth-cookie cookie)
  (-> (cons/c symbol? string?) (or/c null (cons/c string? string?)))
   (match (cdr cookie)
           [(regexp #px"(auth)=(.*)" (list all-match name value))  (cons  name value)]
     [_ null] ))

(define/contract (authenticate-cookies cookies key)
  (-> list? bytes? boolean?)
  (let ((auth-cookie (findf (λ (c) (parse-auth-cookie c))
                            cookies)))
    (and auth-cookie
         (authenticate-cookie (string->bytes/utf-8 (cdr (parse-auth-cookie auth-cookie)))
                              key))))
        
(define bytes->number (compose string->number bytes->string/utf-8))
  
(define/contract (authenticate-cookie received-cookie-value key)
  (-> bytes? bytes? boolean?)
  (authenticate-signed-message (base64-decode received-cookie-value) 
                               key))

(define/contract (make-signed-message plaintext key)
  (-> bytes? bytes? bytes?)
  (bytes-append  (HMAC-SHA1 plaintext key) plaintext))

#|
Assumes that plaintext is an integer representing the UNIX-Era seconds at which the session will expire
return value of #t will indicate conjunction of (1) prepended message digest is uncorrupted
                                                (2) plaintext is a number
                                                (3) cookie is unexpired
|#
(define/contract (authenticate-signed-message signed-message key)
  (-> bytes? bytes? boolean?)
  (and (> (bytes-length signed-message) 20)
       (let ((received-MAC       (subbytes signed-message 0 20))
             (received-plaintext (subbytes signed-message 20)))
         (and (bytes=? received-MAC
                       (HMAC-SHA1 received-plaintext 
                                  key))
              (bytes->number received-plaintext)
              (< (current-seconds) (bytes->number received-plaintext))))))
  
