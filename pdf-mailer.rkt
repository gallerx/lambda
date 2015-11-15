#lang racket/base

#|
(mail-pdf ...) will send arbitrary pdf-encoded bytes as a MIME enclosure

Please make sure to take a 10-second look at pdf-demangler.rkt, also in this repository. 

If using Microsoft's SSRS, demangle your pdf-bytes before sending!

|#


(require net/base64
         racket/contract
         racket/string
         (prefix-in SMTP: net/smtp))
         
         
(define CONFIG:system-SMTP-server "stmpout.yourisp.net")
(define CONFIG:system-mail-is-from "sales@mycompany.com")
(define CONFIG:system-SMTP-server-port      80)
(define CONFIG:system-authorized-mailbox     "admin@mycompany.com")
(define CONFIG:system-authorized-mail-password "verysecret")

(provide mail-pdf)

(define email?
  (let ((email-regex #px"^[-a-zA-Z0-9~/!$%'~^&*_=+\\.\"}{\'?]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,4}$"))
  (λ (email) (and (string? email) (regexp-match-exact? email-regex email)))))

(define email/c (flat-contract email?))

(define/contract (mail-pdf subject to attachment-filename pdf-bytes)
   (-> string? (listof email/c) string? bytes? any/c)
  (unless (null? to)
        (send-mime-mail to
                        subject 
                        (format message-template
                                attachment-filename
                                (bytes->string/utf-8 (base64-encode pdf-bytes))))))

(define/contract (send-mime-mail to subj msg)
  (-> (listof email/c) string? string? any/c)
  (SMTP:smtp-send-message  CONFIG:system-SMTP-server
                           CONFIG:system-mail-is-from
                           to
                           (string-append  "Content-Transfer-Encoding: 7bit;"
                                           "\r\n"
                                           "Content-Type: multipart/mixed; boundary=_----------=_10167391557129230"
                                           "\r\n"
                                           "MIME-Version: 1.0;"
                                           "\r\n"
                                           (format "From: ~A" CONFIG:system-mail-is-from)
                                           "\r\n" 	 	 	 
                                           (string-append "To: " (string-join to ";"))
                                           "\r\n" 	 	 	 	 
                                           "Subject: " subj
                                           "\r\n" 
                                           "Mime-Version: 1.0"  
                                           "\r\n"  "\r\n" )
                         (list msg)
                         #:port-no CONFIG:system-SMTP-server-port
                         #:auth-user CONFIG:system-authorized-mailbox
                         #:auth-passwd CONFIG:system-authorized-mail-password))
        
;format-string which has two parameters, attachment-filename and the actual pdf bytes (look for ~a)
(define message-template
   (string-append "--_----------=_10167391557129230" "\r\n"
                   "Content-Disposition: inline" "\r\n"
                  "Content-Transfer-Encoding: binary" "\r\n"
                  "Content-Type: text/plain" "\r\n"
                  "\r\n\r\n"
                  "Good day,"
                  "\r\n\r\n"
                  "Attached please find your results."
                  "\r\n\r\n"
                  "We sincerely appreciate this opportunity to be of service. If you have any further questions concerning this test or its results, " 
                  "please feel free to contact us at any time."
                  "\r\n\r\n\r\n"
                  "Yours truly,"
                  "\r\n\r\n"
                  "\r\n"
                  "--_----------=_10167391557129230" "\r\n"
                  "Content-Type: application/pdf" "\r\n"
                  "Content-Transfer-Encoding: base64" "\r\n"
                  "Content-Disposition: attachment; filename=~a.pdf" "\r\n"
                   "\r\n"
                   "~A"
                   "\r\n"
                  "--_----------=_10167391557129230--" "\r\n"
                  "\r\n"
                  "."))







