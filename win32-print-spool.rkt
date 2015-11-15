#lang racket/base

#|
file->print-spool sends a file to a win32 print spool
bytes->print-spool send bytes to a win32 print spool

WARNING: if you're sending pdf-encoded bytes, the printer must support DirectPDF 1.4 or greater. Some of the less expensive
printers will not support. see: 

http://h30434.www3.hp.com/t5/Other-Printing-Questions/Direct-PDF-printing/td-p/2598885

Lessons learned:

lpstr are _string/utf-16

by ref pointers as (_ptr o _uint32)

_pointer with #f as arg works as null pointer

Use  W for Unicode vs A for ANSI

vbNullString is the null pointer, so use #f

Winspool.drv implements a print queue for a named printer. User can interact with jobs sent to queue

input pointers are probably unnecessary i.e. (_ptr i _<ctype>). _pointer seems sufficient. 

Output pointers very necessary for by-ref args, and don't show up in param list. Return as (values or as single value.

_pointer types work great with byte buffers

|#


(provide file->print-spool
         bytes->print-spool)

(require ffi/unsafe
         racket/path
         racket/port
         racket/file
         "../../../framebert/flying-web-server/total-function-layer/TLL/anaphora.rkt"
         (prefix-in fail: "../../../framebert/flying-web-server/total-function-layer/control/failure.rkt")
         (prefix-in A/ "../../../framebert/flying-web-server/total-function-layer/control/assert.rkt"))
         


(define kernel (ffi-lib "kernel32.dll"))
(define winspool (ffi-lib "winspool.drv"))

(define-cstruct _DOC_INFO_1  ([pDocName _string/utf-16 ]
                             [pOutputFile _string/utf-16] 
                             [pDatatype _string/utf-16]))


 ;just return h, if its zero, then assume an error has occured
(define open-printer (get-ffi-obj "OpenPrinterW" winspool (_fun   _string/utf-16 (h : (_ptr o _uint32)) _pointer -> (r : _uint32) -> h)));(values h r))))
(define get-last-error (get-ffi-obj "GetLastError" kernel (_fun -> _uint32)))
(define start-doc-printer (get-ffi-obj "StartDocPrinterW" winspool (_fun _uint32 _uint32 _DOC_INFO_1-pointer -> _uint32)))
(define start-page-printer (get-ffi-obj "StartPagePrinter" winspool (_fun _uint32 -> _uint32)))
;note that changing buffer from (_ptr i _pointer) to _pointer worked. May not need input pointers. Do noeed output pointers though (_ptr o ...). 
;Input pointers may convert automatically?
(define write-printer (get-ffi-obj "WritePrinter" winspool (_fun _uint32 _pointer  _uint32 (read : (_ptr o _uint32)) -> (r : _uint32) -> (values read r))))
(define end-page-printer (get-ffi-obj "EndPagePrinter" winspool (_fun _uint32 -> _uint32)))
(define end-doc-printer (get-ffi-obj "EndDocPrinter" winspool (_fun _uint32 -> _uint32)))
(define close-printer (get-ffi-obj "ClosePrinter" winspool (_fun _uint32 -> _uint32)))


;usage: (file->print-spool (build-path  "C:\\temp.ext") "ZebraUSB")
(define (file->print-spool pth szprinter)
  (let ((hprn 0))
    (dynamic-wind
     (λ _ void)
     (λ _ (define buf-size #x4000)
       (set! hprn (A/XPI  (open-printer szprinter #f) (format "print-spool; unable to open printer ~A" szprinter)))
       (let ((di (make-DOC_INFO_1  (string-append "Box-lunch:" (path->string (file-name-from-path pth))) #f "RAW")))
         (A/XPI  (start-doc-printer hprn 1 di) "print-spool: failed to start document")
         (A/XPI  (start-page-printer hprn) "print-spool: failed to start page")
         (let ([buffer (make-bytes buf-size (char->integer #\_))])
           (with-input-from-file pth
             (λ _ 
               ((aλ (bytes-read)
                    (unless (eof-object? bytes-read)
                      (let-values ([(written retval) (write-printer hprn buffer  bytes-read)])
                        (A/XPI retval "print-spool: failed to write to printer"))
                      (self (read-bytes! buffer))))
                (read-bytes! buffer)))
             #:mode 'binary))))
     (λ _ (fail:finally (end-page-printer hprn)
                        (end-doc-printer hprn)
                        (close-printer hprn))))))


(define (bytes->print-spool bstr szprinter)
  (let ((hprn 0))
    (dynamic-wind
     (λ _ void)
     (λ _ (define buf-size #x4000)
       (set! hprn (A/XPI  (open-printer szprinter #f) (format "print-spool; unable to open printer ~A" szprinter)))
       (let ((di (make-DOC_INFO_1  (format "Box-lunch-~a" (current-seconds))  #f "RAW")))
         (A/XPI  (start-doc-printer hprn 1 di) "print-spool: failed to start document")
         (A/XPI  (start-page-printer hprn) "print-spool: failed to start page")
         (let ([buffer (make-bytes buf-size (char->integer #\_))])
           (with-input-from-bytes bstr
                                  (λ _ 
                                    ((aλ (bytes-read)
                                         (unless (eof-object? bytes-read)
                                           (let-values ([(written retval) (write-printer hprn buffer  bytes-read)])
                                             (A/XPI retval "print-spool: failed to write to printer"))
                                           (self (read-bytes! buffer))))
                                     (read-bytes! buffer)))))))
     (λ _ (fail:finally (end-page-printer hprn)
                        (end-doc-printer hprn)
                        (close-printer hprn))))))


