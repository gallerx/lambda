#lang racket/base

#|
==================================================================================================================
Name: clipboard.rkt
Purpose: win32 functionality for copying to clipboard


==================================================================================================================
|#

(provide get
         set)

(require ffi/unsafe)

;win32 constants
(define GHND #x42)
(define CF_TEXT 1)
(define W32NULL 0)  
(define kernel32 (ffi-lib "kernel32.dll"))
(define user32   (ffi-lib "user32.dll"))

;Return types are defined as BOOL, which is an int under win32
;have not been able to obtain a return value other than 1 for success, 0 for failure
(define open-clipboard (get-ffi-obj "OpenClipboard" user32 (_fun _uint32 -> _uint32)))
(define close-clipboard (get-ffi-obj "CloseClipboard" user32 (_fun  -> _uint32)))
(define empty-clipboard (get-ffi-obj "EmptyClipboard" user32 (_fun  -> _uint32)))

;Memory functions

(define local-alloc   (get-ffi-obj "LocalAlloc" kernel32 (_fun _uint32 _uint32 -> _uint32)))
(define local-free    (get-ffi-obj "LocalFree" kernel32 (_fun _uint32 -> _uint32)))
(define local-lock    (get-ffi-obj "LocalLock" kernel32 (_fun _uint32 -> _uint32)))
(define local-unlock  (get-ffi-obj "LocalUnlock" kernel32 (_fun _uint32 -> _uint32)))
(define local-size    (get-ffi-obj "LocalSize" kernel32 (_fun _uint32 -> _uint32)))
(define set-clipboard-data (get-ffi-obj "SetClipboardData" user32 (_fun _uint32 _uint32 -> _uint32)))
(define get-clipboard-data (get-ffi-obj "GetClipboardData" user32 (_fun _uint32 -> _uint32)))
(define lstrcpy  (get-ffi-obj "lstrcpy" kernel32 (_fun _uint32 _string/utf-8 -> _uint32)))
(define olstrcpy  (get-ffi-obj "lstrcpy" kernel32 (_fun  _pointer _uint32 -> _uint32)))
  

;zero return code indicates success
(define (set txt)
  ;(-> string? bit/c)
  (define hMem (local-alloc GHND  (add1 (string-length txt))))
  (define pMem (local-lock hMem))
  (lstrcpy pMem txt)
  (local-unlock hMem)
  (open-clipboard W32NULL)
  (empty-clipboard)
  (set-clipboard-data CF_TEXT hMem) 
  (close-clipboard)
   (begin0 
     (local-free hMem)
  ;Empirically need to cycle the clipboard or local-free won't release
     (hack:cycle-clipboard)))
  
(define (hack:cycle-clipboard)
  (open-clipboard W32NULL)
  (close-clipboard))



#|
=======================================
Name: get 
Purpose: for testing only 

Returns value on clipboard

========================================
|#

(define (get)
  ;(-> string? )
  (open-clipboard W32NULL)
  (define hMem (get-clipboard-data CF_TEXT))
  (define pMem (local-lock hMem))
  (let ((str (make-bytes (local-size pMem) (char->integer #\space))))
    (olstrcpy str pMem)
    (local-unlock hMem)
    (close-clipboard)
    (substring (bytes->string/utf-8 str) 0 (sub1 (bytes-length str)))))
 



(require rackunit
         rackunit/text-ui)

#|
internals <- no dependencies on other modules
|#
;(run-tests internals)
(define internals
  (test-suite
   "Tests for non-exported functionality"
   (let ((counter 0))
     (define (loop)
       (when (< counter 100000)
         (test-equal? "works" 0 (set (number->string counter)))
         (test-equal? "works" (number->string counter) (get))
         (set! counter (add1 counter))
         (loop)))
     (loop))))


