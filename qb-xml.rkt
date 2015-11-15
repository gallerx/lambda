#lang racket/base

#|
--------------------------------------------------------------------------------------
module: qb-xml.rkt

Converts x-expressions of  qb-xexpr.rkt into functions of arity 1 which generate xml
when provided with a message-id (or arity 0 when no mutation of .qbw file is required)

--------------------------------------------------------------------------------------
|#

(require xml
         racket/contract
         racket/list
         (prefix-in GUID: GUID.rkt")
         "tll-common.rkt"
         "anaphora.rkt"
         "qb-common.rkt"
         (only-in "tree.rkt" de-tree)
         (prefix-in XEXPR: "qb-xexpr.rkt"))


(provide  delete-account
          delete-customer
          delete-item/sales-tax
          delete-item/inventory
          delete-item/non-inventory
          delete-item/service
          delete-item/discount
          delete-item/subtotal 
          delete-vendor
          delete-invoice
          delete-item-receipt
          delete-journal-entry
          delete-credit-memo
          query-account
          query-account-by-list-id
          query-customer
          query-item
          query-vendor
          query-vendor-type
          query-sales-tax-code
          query-currency
          query-customers-for-private-data
          query-accounts-for-private-data
          query-items-for-private-data
          query-item/sales-tax-for-private-data
          query-item/discount-for-private-data
          query-item/subtotal-for-private-data
          query-item/service-for-private-data
          query-item/non-inventory-for-private-data
          query-item/inventory-for-private-data
          query-vendors-for-private-data
          query-vendors-for-private-data-by-currency
          query-invoices-for-private-data
          query-item-receipts-for-private-data
          query-bills-for-private-data
          query-credit-memos-for-private-data
          query-invoice
          query-item-receipt
          query-credit-memo
          query-accounts
          query-item-receipts
          query-journal-entrys
          insert-account
          insert-customer
          insert-item/sales-tax
          insert-item/inventory
          insert-item/non-inventory
          insert-item/service
          insert-item/discount
          insert-item/subtotal
          insert-vendor
          insert-invoice
          insert-invoice-ex
          insert-item-receipts
          insert-item-receipts-ex
          insert-journal-entry
          make-journal-line
          insert-credit-memo
          insert-credit-memo-ex
          insert-data-ext-def
          delete-data-ext-def
          add-private-data
          modify-private-data
          status-check
          clear-recovery-data
          clear-all-recovery-data)

(define-values (delete-account
                delete-customer
                delete-item/sales-tax
                delete-item/inventory
                delete-item/non-inventory
                delete-item/service
                delete-item/discount
                delete-item/subtotal
                delete-vendor
                delete-invoice
                delete-item-receipt
                delete-journal-entry
                delete-credit-memo)
  (apply values (map (λ (fn) (contract (-> qbid/c (-> msgset-id/c xml?))
                                       (λ (object-id)
                                         (λ (msgset-id)
                                           (process-outbound-xexpr ((message-set msgset-id) 
                                                                    (make-payload (fn object-id))))))
                                       "anonymous-fn" "society"))
                     (list XEXPR:account-delete
                           XEXPR:customer-delete
                           XEXPR:item-sales-tax-delete
                           XEXPR:item-inventory-delete
                           XEXPR:item-non-inventory-delete
                           XEXPR:item-service-delete
                           XEXPR:item-discount-delete
                           XEXPR:item-subtotal-delete
                           XEXPR:vendor-delete
                           XEXPR:invoice-delete
                           XEXPR:item-receipt-delete
                           XEXPR:journal-entry-delete
                           XEXPR:credit-memo-delete))))


;pure-query message => no msgset-id => don't store unnessary recovery data in company file
(define-values (query-accounts-for-private-data
                query-accounts
                query-customers-for-private-data
                query-items-for-private-data
                query-item/sales-tax-for-private-data
                query-item/discount-for-private-data
                query-item/subtotal-for-private-data
                query-item/service-for-private-data
                query-item/non-inventory-for-private-data
                query-item/inventory-for-private-data
                query-vendors-for-private-data
                query-invoices-for-private-data
                query-item-receipts-for-private-data
                query-item-receipts
                query-bills-for-private-data
                query-journal-entrys
                query-credit-memos-for-private-data)
  (apply values (map (λ (fn) (contract (-> xml?)
                                       (λ _
                                         (process-outbound-xexpr ((message-set) 
                                                                  (make-payload (fn)))))
                                       "anonymous-fn" "society"))
                     (list XEXPR:account-query-for-private-data
                           XEXPR:accounts-query
                           XEXPR:customer-query-for-private-data
                           XEXPR:item-query-for-private-data
                           XEXPR:item/sales-tax-query-for-private-data
                           XEXPR:item/discount-query-for-private-data
                           XEXPR:item/subtotal-query-for-private-data
                           XEXPR:item/service-query-for-private-data
                           XEXPR:item/non-inventory-query-for-private-data
                           XEXPR:item/inventory-query-for-private-data
                           XEXPR:vendor-query-for-private-data
                           XEXPR:invoice-query-for-private-data
                           XEXPR:item-receipt-query-for-private-data
                           XEXPR:item-receipts-query
                           XEXPR:bill-query-for-private-data
                           XEXPR:journal-entrys-query
                           XEXPR:credit-memo-query-for-private-data))))

                    
;pure-query message => no msgset-id => don't store unnessary recovery data in company file
;will reuse for query-by-list-id's as well since its just passing string
(define-values (query-account          ;uses full-name
                query-account-by-list-id ;uses list-id
                query-customer         ;uses full-name
                query-item             ;uses full-name
                query-vendor           ;uses full-name
                query-vendor-type      ;uses full-name
                query-sales-tax-code   ;uses full-name
                query-currency         ;uses full-name
                query-vendors-for-private-data-by-currency ; uses full-name
                query-invoice          ;uses txn-id
                query-item-receipt    ;uses txn-id
                query-credit-memo)   ;uses txn-id
  (apply values (map (λ (fn) (contract (-> string? xml?)
                                       (λ (full-name)
                                         (process-outbound-xexpr ((message-set) 
                                                                  (make-payload (fn full-name)))))
                                       "anonymous-fn" "society"))
                     (list XEXPR:account-query
                           XEXPR:account-query-by-list-id
                           XEXPR:customer-query
                           XEXPR:item-query
                           XEXPR:vendor-query
                           XEXPR:vendor-type-query
                           XEXPR:sales-tax-code-query
                           XEXPR:currency-query
                           XEXPR:vendor-query-for-private-data-by-currency
                           XEXPR:invoice-query
                           XEXPR:item-receipt-query
                           XEXPR:credit-memo-query))))





#|
-----------------------------------------------------------------------------------------------
Name: message-set
Note: We use CONTINUE-ON-ERROR

In practice, we restrict file mutations (add,update, delete) to one message per group so 
can use control flow to signal that file-mutation failed.

Notes: See table 31-2 QB-SDK Programmers Guide page 410
We use following permutations of new-id old-id

NewID     OldID     Behavior
 x          null    If new, Execute request and store state for the message ID. If repeated, status check
 x          x       old = new, status check. We use this version of a status check
 null       x        clear state
 null     CLR-ALL-MSG-SETS  clears all state.
null       null    don't store recovery info. Use for queries or non-mutating messages (do they exist, besides queries?)

The CLR-ALL permutation is why we need to specify contract for old as string? instead of msgset-id/c


-----------------------------------------------------------------------------------------------
|#
(define/contract (message-set [new-id null] #:old-id [old-id null] #:on-error [on-error CONTINUE-ON-ERROR])
  (->* () ((or/c msgset-id/c null?) #:old-id (or/c string? null?) #:on-error (or/c CONTINUE-ON-ERROR STOP-ON-ERROR)) (-> (listof xexpr?) xexpr?))
  (λ (payload)
    `(QBXML ,(append `(QBXMLMsgsRq ,(cond [(and (null? old-id) (null? new-id)) `((onError ,on-error))]  ;both null-> don't want to store recovery info
                                          [(null? old-id) `((newMessageSetID  ,new-id)(onError ,on-error))] ;new only, see above table
                                          [(null? new-id) `((oldMessageSetID  ,old-id)(onError ,on-error))] ;old only, see above table
                                          [else           `((newMessageSetID  ,new-id)(oldMessageSetID ,old-id)(onError ,on-error))])) ;both non-null
                     payload))))


;each arg is a (-> integer? xexpr?) ->  (listof xexpr?)
(define-syntax-rule (make-payload args ...)
  (map (λ (arg ordinal) (arg ordinal))
       (list args ...)      
       (build-list (length (list args ...)) values)))

;used for insert-item-receipts. the de-tree coerces  return value into a list of xexpr?. (begine as  a (listof (listof xexpr?)
;assumes two sub expressions (i.e. first, second)in each pair
(define/contract (make-payload* args)
  (-> (listof (-> integer? (listof xexpr?))) (listof xexpr?))
  (de-tree  (map (λ (arg ordinal) (arg ordinal))
                 args
                 (build-list (length args) values))))


#|
----------------------------------------------------------------------------------------------------------
Returns the xml
----------------------------------------------------------------------------------------------------------
|#

(define/contract (process-outbound-xexpr xexpr)
  (-> xexpr? xml?)
  (string-append QBXML-PREAMBLE (xexpr->string xexpr)))

(define-syntax-rule (prepare-xml/recovery body ...)
  (λ (msgset-id)
    (process-outbound-xexpr ((message-set msgset-id)
                             (make-payload body
                                           ...)))))


(define/contract (prepare-xml/recovery* body)
  (-> (listof (-> integer? (listof xexpr?)))  (-> msgset-id/c xml?))
  (λ (msgset-id)
    (process-outbound-xexpr ((message-set msgset-id)
                             (make-payload* body)))))


(define/contract (insert-customer name ship-tos)
  (-> string? (listof ship-to?) (-> msgset-id/c xml?))
  (prepare-xml/recovery (XEXPR:customer-add name
                                            ship-tos)))

(define/contract (insert-account name type #:parent [parent null] #:currency [currency  null])
  (->* (string? qb-account-type/c) (#:parent qbid-or-fresh/c #:currency  qbid-or-fresh/c) (-> msgset-id/c string?))
  (prepare-xml/recovery (XEXPR:account-add name
                                           type
                                           #:parent parent
                                           #:currency currency)))

;non-neg two sigs because we have the well known OUTSIDE NEXUS rate that applies 0.0% tax
(define/contract (insert-item/sales-tax name desc tax-vendor rate)
  (-> string? string? qbid/c !negative-real/c (-> msgset-id/c xml?))
  (prepare-xml/recovery  (XEXPR:item-sales-tax-add name
                                                   desc
                                                   tax-vendor
                                                   rate)))

(define/contract (insert-item/inventory name income-acc expense-acc accrual-acc sales-tax-code charge-customer?)
  (-> string? qbid/c qbid/c qbid/c qbid/c boolean? (-> msgset-id/c xml?))
  (prepare-xml/recovery (XEXPR:item-inventory-add name
                                                  income-acc
                                                  expense-acc
                                                  accrual-acc
                                                  sales-tax-code
                                                  charge-customer?)))

(define/contract (insert-item/non-inventory name credit-acc sales-tax-code)
  (-> string? qbid/c qbid/c (-> msgset-id/c xml?))
  (prepare-xml/recovery (XEXPR:item-non-inventory-add name
                                                    credit-acc
                                                    sales-tax-code)))

(define/contract (insert-item/service name credit-acc sales-tax-code)
  (-> string? qbid/c qbid/c (-> msgset-id/c xml?))
  (prepare-xml/recovery (XEXPR:item-service-add name
                                              credit-acc
                                              sales-tax-code)))

(define/contract (insert-item/discount name credit-acc sales-tax-code pct-discount)
  (-> string? qbid/c qbid/c !neg-two-sigs/c  (-> msgset-id/c xml?))
  (prepare-xml/recovery (XEXPR:item-discount-add name
                                                 credit-acc
                                                 sales-tax-code
                                                 pct-discount)))

(define/contract (insert-item/subtotal name)
  (-> string? (-> msgset-id/c xml?))
  (prepare-xml/recovery (XEXPR:item-subtotal-add name)))

(define/contract (insert-vendor name vendor-type #:currency [currency null])
  (->* (string? qbid/c) (#:currency qbid-or-fresh/c) (-> msgset-id/c xml?))
  (prepare-xml/recovery (XEXPR:vendor-add name
                                          vendor-type
                                          #:currency currency)))




#|
-----------------------------------------------------------------------------------------------------------
insert invoice:
If this parameter is not passed to QB, QB will use the default sales tax set up in Company Preferences

For this reason, we will explicitly create and pass an "Outside Nexus" Sales Tax Item with zero rate.
(other option would be to use a null parameter). Note the Outside-Nexus item will still need to be linked
to the NY Sales Tax Payable Vendor

-----------------------------------------------------------------------------------------------------------
|#
(define/contract (insert-invoice customer date sales-tax-jurisdiction invoice-lines)
  (-> qbid/c  qb-date/c qbid/c (listof  xexpr?) (-> msgset-id/c xml?))
  (prepare-xml/recovery (XEXPR:invoice-add customer
                                           date
                                           sales-tax-jurisdiction
                                           invoice-lines)))



(define/contract (insert-invoice-ex customer date sales-tax-jurisdiction bill-to ship-to invoice-lines)
  (-> qbid/c  qb-date/c qbid/c ship-to? ship-to? (listof  xexpr?) (-> msgset-id/c xml?))
  (prepare-xml/recovery (XEXPR:invoice-add-ex customer
                                              date
                                              sales-tax-jurisdiction
                                              bill-to
                                              ship-to
                                              invoice-lines)))




;passthrough 
(provide (rename-out [XEXPR:invoice-line-add/discount      insert-invoice-line/discount]
                     [XEXPR:invoice-line-add/subtotal      insert-invoice-line/subtotal]
                     [XEXPR:invoice-line-add/inventory     insert-invoice-line/inventory]
                     [XEXPR:invoice-line-add/non-inventory insert-invoice-line/non-inventory]
                     [XEXPR:invoice-line-add/service       insert-invoice-line/service]))
                                    
#|
-----------------------------------------------------------------------------------------------
Name: insert-item-receipts
Discussion: unique in that it produced function which generates two messages (listof xexpr?)

The first message is an insert-item-receipt
The second message is 

------------------------------------------------------------------------------------------------
|#
(define-syntax-rule (insert-item-receipts plt date (vendor ref-number fx-rate items) ...)
  (prepare-xml/recovery* (list (XEXPR:item-receipt-add plt
                                                       vendor
                                                       date
                                                       ref-number
                                                       fx-rate
                                                       items)
                        ...)))

(require racket/match)

;use the special prepare-xml/recovery* to accept a list of 
(define/contract (insert-item-receipts-ex plt ir-args)
  (-> pkid/c (listof (list/c qbid/c string?  +fx/c (listof xexpr?))) (-> msgset-id/c xml?))
  (awith (map (λ (ir)
                               (match ir ;destructuring bind
                                 [(list vendor-id ref-number fx-rate items) (XEXPR:item-receipt-add plt
                                                                                                    vendor-id
                                                                                                    (qb-date)
                                                                                                    ref-number
                                                                                                    fx-rate
                                                                                                    items)]))
              ir-args)
  (prepare-xml/recovery* it)))

;Passthrough 
(provide (rename-out [XEXPR:item-receipt-line-add/subtotal      insert-item-receipt-line/subtotal]
                     [XEXPR:item-receipt-line-add/non-inventory insert-item-receipt-line/non-inventory]
                     [XEXPR:item-receipt-line-add/service       insert-item-receipt-line/service]))

;(insert-item-receipt "8399124-12521" "2015-08-21" "cheese" (list (insert-item-receipt-line/subtotal "help")))



#|
-----------------------------------------------------------------------------------------------------------
insert-journal-entry:
If this parameter is not passed to QB, QB will use the default sales tax set up in Company Preferences

For this reason, we will explicitly create and pass an "Outside Nexus" Sales Tax Item with zero rate.
(other option would be to use a null parameter). Note the Outside-Nexus item will still need to be linked
to the NY Sales Tax Payable Vendor

-----------------------------------------------------------------------------------------------------------
|#
(define/contract (insert-journal-entry date ref journal-lines)
  (-> qb-date/c string? (listof  xexpr?) (-> msgset-id/c xml?))
  (prepare-xml/recovery (XEXPR:journal-entry-add date
                                                 ref
                                                 journal-lines)))


(define/contract (make-journal-line dc)
  (-> DC/c (->* (qbid/c two-sigs/c) (#:entity qbid-or-fresh/c) xexpr?))
  (XEXPR:journal-line-helper dc))







#|
-----------------------------------------------------------------------------------------------------------
insert credit-memo:
If this parameter is not passed to QB, QB will use the default sales tax set up in Company Preferences

For this reason, we will explicitly create and pass an "Outside Nexus" Sales Tax Item with zero rate.
(other option would be to use a null parameter). Note the Outside-Nexus item will still need to be linked
to the NY Sales Tax Payable Vendor

-----------------------------------------------------------------------------------------------------------
|#
(define/contract (insert-credit-memo customer date sales-tax-jurisdiction credit-memo-lines)
  (-> qbid/c  qb-date/c qbid/c (listof  xexpr?) (-> msgset-id/c xml?))
  (prepare-xml/recovery (XEXPR:credit-memo-add customer
                                               date
                                               sales-tax-jurisdiction
                                               credit-memo-lines)))



(define/contract (insert-credit-memo-ex customer date sales-tax-jurisdiction bill-to ship-to credit-memo-lines)
  (-> qbid/c  qb-date/c qbid/c ship-to? ship-to? (listof  xexpr?) (-> msgset-id/c xml?))
  (prepare-xml/recovery (XEXPR:credit-memo-add-ex customer
                                                  date
                                                  sales-tax-jurisdiction
                                                  bill-to
                                                  ship-to
                                                  credit-memo-lines)))





;passthrough 
(provide (rename-out [XEXPR:credit-memo-line-add/discount      insert-credit-memo-line/discount]
                     [XEXPR:credit-memo-line-add/subtotal      insert-credit-memo-line/subtotal]
                     [XEXPR:credit-memo-line-add/inventory     insert-credit-memo-line/inventory]
                     [XEXPR:credit-memo-line-add/non-inventory insert-credit-memo-line/non-inventory]
                     [XEXPR:credit-memo-line-add/service       insert-credit-memo-line/service]))






#|
-------------------------------------------------------------------------
Start recovery messages. See p410, table 31-2 QB SDK Programmer Guide

clear-recovery-data: clears the recovery 
Name: clear-recovery-data
Notes: Implemented ref. table 31-2 on page 410 QB-SDK Programmer Guide
Notes: Setting #:old-id & new id to same value (msgset-id) results in
QB deleting error recovery info for that msgset-id, which is desired
-------------------------------------------------------------------------
|#
(define/contract (clear-recovery-data msgset-id)
  (-> msgset-id/c xml?)
  (process-outbound-xexpr ((message-set #:old-id msgset-id)
                           null))) 

(define/contract (clear-all-recovery-data)
  (-> xml?)
  (process-outbound-xexpr ((message-set #:old-id CLR-ALL-MSG-SETS)
                           null))) 


(define/contract (status-check msgset-id)
  (-> msgset-id/c xml?)
  (process-outbound-xexpr ((message-set msgset-id #:old-id msgset-id)
                           null))) 


#|
--------------------------------------------------------------------------------------------------------
Private Data Definition and Mutation Messages
--------------------------------------------------------------------------------------------------------
|#

(define/contract (insert-data-ext-def)
  (-> (-> msgset-id/c xml?))
  (prepare-xml/recovery (XEXPR:data-ext-def-add OWNER-GUID
                                                DATA-EXTENSION-NAME)))

(define/contract (add-private-data obj-type obj-id value)
  (-> qb-obj-type/c  qbid/c string?  (-> msgset-id/c xml?))
  (prepare-xml/recovery (XEXPR:data-ext-add obj-type
                                            obj-id
                                            value)))

(define/contract (modify-private-data obj-type obj-id value)
  (-> qb-obj-type/c qbid/c string?  (-> msgset-id/c xml?))
  (prepare-xml/recovery (XEXPR:data-ext-mod obj-type
                                            obj-id
                                            value)))

(define/contract (delete-data-ext-def)
  (-> (-> msgset-id/c xml?))
  (prepare-xml/recovery (XEXPR:data-ext-def-delete OWNER-GUID
                                                   DATA-EXTENSION-NAME)))
         
(require rackunit
         rackunit/text-ui)

(define internals
  (test-suite
   "Tests for qb-xexpr"
   (let ((m1 (GUID:GUID-23))
         (m2 (GUID:GUID-23)))
     ;permutations of msgset parameters
     (test-equal? "good with non-null new id, non-null old-id"
                  `(QBXML (QBXMLMsgsRq ((newMessageSetID ,m1)(oldMessageSetID ,m2)(onError ,CONTINUE-ON-ERROR))))
                  ((message-set m1 #:old-id m2) null))
     (test-equal? "good with non-null new id, null old-id"
                  `(QBXML (QBXMLMsgsRq ((newMessageSetID ,m1) (onError ,CONTINUE-ON-ERROR))))
                  ((message-set m1) null))
     (test-equal? "good with null new id, non-null old-id"
                  `(QBXML (QBXMLMsgsRq ((oldMessageSetID ,m2) (onError ,CONTINUE-ON-ERROR))))
                  ((message-set #:old-id m2) null))
     (test-equal? "good with null new id,null old-id"
                  `(QBXML (QBXMLMsgsRq ((onError ,CONTINUE-ON-ERROR))))
                  ((message-set) null))
     (test-equal? "expected"
                  (string-append "<?xml version=\"1.0\" ?><?qbxml version=\"13.0\" ?>"
                                 "<QBXML><QBXMLMsgsRq newMessageSetID=\"woohoo\" onError=\"continueOnError\"><xml>0</xml></QBXMLMsgsRq></QBXML>")
                  ((prepare-xml/recovery (λ (num) `(xml ,(number->string num)))) "woohoo"))
     (awith (string-append "<?xml version=\"1.0\" ?><?qbxml version=\"13.0\" ?>"
                           "<QBXML><QBXMLMsgsRq newMessageSetID=\"cheese\" onError=\"continueOnError\">"
                           "<VendorAddRq requestID=\"0\">"
                           "<VendorAdd><Name>NY</Name><VendorTypeRef><ListID>Tax agency</ListID></VendorTypeRef></VendorAdd>"
                           "</VendorAddRq><VendorAddRq requestID=\"1\">"
                           "<VendorAdd><Name>CT</Name><VendorTypeRef><ListID>Tax agency</ListID></VendorTypeRef></VendorAdd>"
                           "</VendorAddRq></QBXMLMsgsRq></QBXML>")
            (test-equal? "multi messages with prepare/xml"
                  it
                  ((prepare-xml/recovery (XEXPR:vendor-add "NY" QB/TAX-AGENCY #:currency null)
                                         (XEXPR:vendor-add "CT" QB/TAX-AGENCY #:currency null))
                   "cheese")))
     (test-equal? "multi messages with prepare-xml/recovery* only works with item-receipt-adds!!! - change date x 2 to fix error"
                  "<?xml version=\"1.0\" ?><?qbxml version=\"13.0\" ?><QBXML><QBXMLMsgsRq newMessageSetID=\"cheese\" onError=\"continueOnError\"><ItemReceiptAddRq requestID=\"0\"><ItemReceiptAdd defMacro=\"TxnID:0\"><VendorRef><ListID>a542Y</ListID></VendorRef><TxnDate>2015-11-07</TxnDate><RefNumber>wookie</RefNumber><ExchangeRate>0.500000</ExchangeRate></ItemReceiptAdd></ItemReceiptAddRq><DataExtAddRq requestID=\"10000\"><DataExtAdd><OwnerID>{4918EE0D-6342-4B07-17F4-8743D2AE268B}</OwnerID><DataExtName>BLPKID6</DataExtName><TxnDataExtType>ItemReceipt</TxnDataExtType><TxnID useMacro=\"TxnID:0\"></TxnID><DataExtValue>pl:1.0|fake</DataExtValue></DataExtAdd></DataExtAddRq><ItemReceiptAddRq requestID=\"1\"><ItemReceiptAdd defMacro=\"TxnID:1\"><VendorRef><ListID>c253F</ListID></VendorRef><TxnDate>2015-11-07</TxnDate><RefNumber>eqoker</RefNumber><ExchangeRate>0.800000</ExchangeRate></ItemReceiptAdd></ItemReceiptAddRq><DataExtAddRq requestID=\"10001\"><DataExtAdd><OwnerID>{4918EE0D-6342-4B07-17F4-8743D2AE268B}</OwnerID><DataExtName>BLPKID6</DataExtName><TxnDataExtType>ItemReceipt</TxnDataExtType><TxnID useMacro=\"TxnID:1\"></TxnID><DataExtValue>pl:2.1|fake</DataExtValue></DataExtAdd></DataExtAddRq></QBXMLMsgsRq></QBXML>"
                  ((prepare-xml/recovery* `(,(XEXPR:item-receipt-add 1 "a542Y" (qb-date) "wookie" 0.50000 null)
                                            ,(XEXPR:item-receipt-add 2 "c253F" (qb-date) "eqoker" 0.80000 null)))
                   "cheese"))
     (test-equal? "two xexpressions returned from two functions"
                  '((xml "0") (xml "1"))
                  (make-payload (λ (x) `(xml ,(number->string x)))
                                (λ (x) `(xml ,(number->string x)))))
     (test-equal? "expected"
                  '(JournalDebitLine
                    (AccountRef (ListID "mee"))
                    (Amount "12.24")
                    (EntityRef (ListID "cheese")))
                  ((make-journal-line +1) "mee" 12.24 #:entity "cheese"))
     (test-equal? "expected"
                  '(JournalCreditLine
                    (AccountRef (ListID "mee"))
                    (Amount "12.24")
                    (EntityRef (ListID "cheese")))
                  ((make-journal-line -1) "mee" 12.24 #:entity "cheese"))
     
     )))

