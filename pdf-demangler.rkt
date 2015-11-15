#lang racket/base

(require racket/contract)

(provide demangle)


#|
==================================================================================================================
Discussion: 

Micosoft PDF Renderer 10.0.0.0,, as supplied with SSRS 2008R2, 2012, and possibly beyond

Mangles the PDF format in three ways 

1) inserts \r\n vs \n for linebreaks
2) uses special character stream + single-whitespace instead of stream and no whitespace
3) no termal #"\n"

Howerver only the "stream " issue affects printing to the print spool 

This demangler revises those deficiencies to get printable output

Silly & simple code, but it can consume a disproportional number of hours 

=================================================================================================================
|#

(define/contract (demangle bstr)
  (-> bytes? bytes?)
  (regexp-replace* #rx"stream " bstr #"stream"))
 
