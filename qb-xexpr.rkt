#lang racket/base

#|
module: qb-xexpr.rkt
Purpose: This is the lowest lqevel module in the messaging stack

Sole purpose is to generate xexprs conforming to the QB-XML syntax for
a wide variety of message types

-Generates xexprs for many of Intuit's QBXML requests.

-Follows QB's xml naming convention of <object><action>, i.e. customer-add

Such xexprs are converted to xml in module messages.rkt

All exported functions in this module are (-> integer? xexpr?), where integer? is the request-id
that a wrapper function will provide just before request transmission.


|#

(require  xml
          racket/contract
          "anaphora.rkt"
          "tll-common.rkt"
         "qb-common.rkt")


(provide account-query-for-private-data
         customer-query-for-private-data
         item-query-for-private-data
         item/sales-tax-query-for-private-data
         item/discount-query-for-private-data
         item/subtotal-query-for-private-data
         item/service-query-for-private-data
         item/non-inventory-query-for-private-data
         item/inventory-query-for-private-data
         vendor-query-for-private-data
         vendor-query-for-private-data-by-currency
         invoice-query-for-private-data
         item-receipt-query-for-private-data
         bill-query-for-private-data
         credit-memo-query-for-private-data
         accounts-query
         ;queries based on full-name
         account-query
         account-query-by-list-id
         customer-query
         item-query
         vendor-query
         vendor-type-query
         sales-tax-code-query
         currency-query
         ;deletes
         account-delete
         customer-delete
         item-sales-tax-delete
         item-inventory-delete
         item-non-inventory-delete
         item-service-delete
         item-discount-delete
         item-subtotal-delete
         vendor-delete
         invoice-delete
         item-receipt-delete
         journal-entry-delete
         credit-memo-delete
         ;list-adds
         customer-add
         account-add
         item-sales-tax-add
         item-inventory-add
         item-non-inventory-add
         item-service-add
         item-discount-add
         item-subtotal-add
         vendor-add
         ;invoice
         invoice-add
         invoice-add-ex
         invoice-line-add/discount
         invoice-line-add/subtotal
         invoice-line-add/service
         invoice-line-add/non-inventory
         invoice-line-add/inventory
         invoice-query
         ;item-receipt
         item-receipt-add
         item-receipt-line-add/subtotal
         item-receipt-line-add/service
         item-receipt-line-add/non-inventory
         item-receipt-query
         item-receipts-query
         ;credit-memo
         credit-memo-add
         credit-memo-add-ex
         credit-memo-line-add/discount
         credit-memo-line-add/subtotal
         credit-memo-line-add/service
         credit-memo-line-add/non-inventory
         credit-memo-line-add/inventory
         credit-memo-query
         ;journal-enry
        journal-entrys-query
         ;private data
         data-ext-def-add
         data-ext-def-delete
         data-ext-add
         data-ext-mod
         ;journal
         journal-entry-add
         journal-line-helper)

(define/contract (generic-object-delete obj-del-type obj-id)
  (-> (or/c qb-list-del-type/c qb-txn-del-type/c)  qbid/c (-> integer? xexpr?))
  (λ (requestID)
    (cond [(qb-list-del-type/c obj-del-type) `(ListDelRq ((requestID ,(number->string requestID)))
                                                            (ListDelType ,obj-del-type)
                                                            (ListID ,obj-id))]
          [(qb-txn-del-type/c obj-del-type)  `(TxnDelRq ((requestID ,(number->string requestID)))
                                                           (TxnDelType ,obj-del-type)
                                                           (TxnID ,obj-id))]
          [else (error (format "Run of in generic-object-delete with ~a\n" obj-del-type))])))

(define-values (account-delete
                customer-delete
                item-sales-tax-delete
                item-inventory-delete
                item-non-inventory-delete
                item-service-delete
                item-discount-delete
                item-subtotal-delete
                vendor-delete
                invoice-delete
                item-receipt-delete
                journal-entry-delete
                credit-memo-delete)
  (apply values (map (λ (obj-del-type) (contract (-> qbid/c (-> integer? xexpr?))
                                                 (λ (obj-id)
                                                    (generic-object-delete obj-del-type obj-id))
                                                 "anonymous-fn" "society"))
                     (list QB/ACCOUNT
                           QB/CUSTOMER
                           QB/ITEM-SALES-TAX
                           QB/ITEM-INVENTORY
                           QB/ITEM-NON-INVENTORY
                           QB/ITEM-SERVICE
                           QB/ITEM-DISCOUNT
                           QB/ITEM-SUBTOTAL
                           QB/VENDOR
                           QB/INVOICE
                           QB/ITEM-RECEIPT
                           QB/JOURNAL-ENTRY
                           QB/CREDIT-MEMO))))


#|
---------------------------------------------------------------------------------------------
query based on full name

---------------------------------------------------------------------------------------------
|#

(define/contract (generic-full-name-query msg-sym full-name)
  (-> symbol? string? (-> integer? xexpr?))
  (λ (requestID) 
    `(,msg-sym ((requestID ,(number->string requestID)))
               (FullName ,full-name))))

(define/contract (generic-list-id-query msg-sym list-id)
  (-> symbol? string? (-> integer? xexpr?))
  (λ (requestID) 
    `(,msg-sym ((requestID ,(number->string requestID)))
               (ListID ,list-id))))



(define-values (account-query
                account-query-by-list-id
                customer-query
                item-query
                vendor-query
                vendor-type-query
                sales-tax-code-query
                currency-query)
  (apply values (map (λ (msg-sym fname?)
                       (contract (-> string? (-> integer? xexpr?))
                                 (λ (token)
                                   (if fname?
                                       (generic-full-name-query msg-sym token)
                                       (generic-list-id-query   msg-sym token)))
                                 "anonymous-fn" "society"))
                     (list 'AccountQueryRq 
                           'AccountQueryRq 
                           'CustomerQueryRq
                           'ItemQueryRq
                           'VendorQueryRq
                           'VendorTypeQueryRq
                           'SalesTaxCodeQueryRq
                           'CurrencyQueryRq)
                     (list #t
                           #f
                           #t
                           #t
                           #t
                           #t
                           #t
                           #t))))




;returns all accounts. Used to  delete all accounts (whether or not mapped) so we can have a
;re-baselined chart-of-accounts. 
(define/contract (accounts-query)
  (-> (-> integer? xexpr?))
  (λ (requestID) 
    `(AccountQueryRq ((requestID ,(number->string requestID)))
                         (IncludeRetElement "ListID"))))




(define/contract (invoice-query txn-id)
  (-> qbid/c (-> integer? xexpr?))
  (λ (requestID) 
    `(InvoiceQueryRq ((requestID ,(number->string requestID)))
                     (TxnID ,txn-id))))


(define/contract (item-receipt-query txn-id)
  (-> qbid/c (-> integer? xexpr?))
  (λ (requestID) 
    `(ItemReceiptQueryRq ((requestID ,(number->string requestID)))
                           (TxnID ,txn-id)
                           (IncludeRetElement "TxnID")
                           (IncludeRetElement "TotalAmount")
                           (IncludeRetElement "CurrencyRef")
                           (IncludeRetElement "ExchangeRate")
                           (IncludeRetElement "TotalAmountInHomeCurrency"))))
                                              
;returns all item receipts. Used for multi-message testing to delete all item-receipts when we
;don't have a private-data element (ie. any multi-message insertion)
(define/contract (item-receipts-query)
  (-> (-> integer? xexpr?))
  (λ (requestID) 
    `(ItemReceiptQueryRq ((requestID ,(number->string requestID)))
                         (IncludeRetElement "TxnID"))))

;returns all journal-entries. Used for testing to delete all journal entries b/c they don't have
;private data mapping.
(define/contract (journal-entrys-query)
  (-> (-> integer? xexpr?))
  (λ (requestID) 
    `(JournalEntryQueryRq ((requestID ,(number->string requestID)))
                         (IncludeRetElement "TxnID"))))




(define/contract (credit-memo-query txn-id)
  (-> qbid/c (-> integer? xexpr?))
  (λ (requestID) 
    `(CreditMemoQueryRq ((requestID ,(number->string requestID)))
                     (TxnID ,txn-id))))



#|
------------------------------------------------------------------------------------------------------------------------------
Not all object support private-data: Limited to the following

Account, ARRefundCreditCard, Bill, BillPaymentCheck, BillPaymentCreditCard, BuildAssembly, Charge, Check, Company, CreditCardCharge,
CreditCardCredit, CreditMemo, Customer, Deposit, Employee, Estimate, InventoryAdjustment, Invoice, Item, ItemReceipt, JournalEntry,
OtherName, PurchaseOrder, ReceivePayment, SalesOrder, SalesReceipt, SalesTaxPaymentCheck, Vendor, VendorCredit

For items which don't support private data, we will have to use full-name query.
For example, vendor-type-query and sales-tax-code-query

------------------------------------------------------------------------------------------------------------------------------
|#
(define/contract (generic-query-for-private-data msg-sym owner-id)
  (-> symbol? string? (-> integer? xexpr?))
  (λ (requestID) 
    `(,msg-sym ((requestID ,(number->string requestID)))
               (IncludeRetElement "DataExtRet")
               (OwnerID ,owner-id))))

(define-values (account-query-for-private-data
                customer-query-for-private-data
                item-query-for-private-data
                item/sales-tax-query-for-private-data
                item/discount-query-for-private-data
                item/subtotal-query-for-private-data
                item/service-query-for-private-data
                item/non-inventory-query-for-private-data
                item/inventory-query-for-private-data
                vendor-query-for-private-data
                invoice-query-for-private-data
                item-receipt-query-for-private-data
                bill-query-for-private-data
                credit-memo-query-for-private-data)
  (apply values (map (λ (msg-name) (contract (-> (-> integer? xexpr?))
                                             (λ _ (generic-query-for-private-data msg-name OWNER-GUID))
                                             "anonymous-fn" "society"))
                     (list 'AccountQueryRq
                           'CustomerQueryRq
                           'ItemQueryRq
                           'ItemSalesTaxQueryRq
                           'ItemDiscountQueryRq
                           'ItemSubtotalQueryRq
                           'ItemServiceQueryRq
                           'ItemNonInventoryQueryRq
                           'ItemInventoryQueryRq
                           'VendorQueryRq
                           'InvoiceQueryRq
                           'ItemReceiptQueryRq
                           'BillQueryRq
                           'CreditMemoQueryRq))))


(define/contract (account-add name account-type #:parent parent #:currency currency)
  (->  string? qb-account-type/c #:parent qbid-or-fresh/c #:currency qbid-or-fresh/c (-> integer? xexpr?))
  (λ (requestID) 
    `(AccountAddRq ((requestID ,(number->string requestID)))
                   ,(filter (λ (val) (not (void? val)))
                            `(AccountAdd (Name ,name)
                                        ,(unless (null? parent) `(ParentRef (ListID ,parent)))
                                        (AccountType ,account-type)
                                        ,(unless (null? currency) `(CurrencyRef (ListID ,currency))))))))



(define (address-helper sym)
  (λ (st)
    (filter (λ (val) (not (void? val)))
            `(,sym (Name ,(ship-to-company st))
                   ,(awith (ship-to-attn st)
                           (unless (null? it) `(Addr1 ,it)))
                   (Addr2 ,(ship-to-addr1 st))
                   ,(awith (ship-to-addr2 st)
                           (unless (null? it) `(Addr3 ,it)))
                   ,(awith (ship-to-addr3 st)
                           (unless (null? it) `(Addr4 ,it)))
                   (City  ,(ship-to-city st))
                   (State ,(ship-to-state st))
                   (PostalCode ,(ship-to-postal st))
                   (Country  ,(ship-to-country st))))))

(define ship-to-helper (address-helper 'ShipToAddress))

;truncate name b/c get-or-reify supported                                 
(define/contract (customer-add name ship-tos)
  (->  string? (listof ship-to?) (-> integer? xexpr?))
  (λ (requestID)
    `(CustomerAddRq ((requestID ,(number->string requestID)))
                    (CustomerAdd (Name ,(substring name 0 (min (string-length name) 41)))
                                 ,@(map (λ (st) (ship-to-helper st))
                                        ship-tos)))))

#|
----------------------------------------------------------------------------------------------------
Name: item-sales-tax-add
Notes:
-rate is stated with basis as 1/100, i.e. rate = 100 -> 100%,
-tax-vendor-id is the Relevant Tax Authority

Use non-neg two sigs for rate b/c 0.0 is acceptable for OUTSIDE NEXUS (0.0%) tax item
----------------------------------------------------------------------------------------------------
|#
(define/contract (item-sales-tax-add name desc tax-vendor rate)
  (->  string? string? qbid/c !negative-real/c  (-> integer? xexpr?))
  (λ (requestID) 
    `(ItemSalesTaxAddRq ((requestID ,(number->string requestID)))
                        (ItemSalesTaxAdd (Name ,name)
                                         (ItemDesc ,desc)
                                         (TaxRate  ,(number->string rate))
                                         (TaxVendorRef  (ListID ,tax-vendor))))))

#|
----------------------------------------------------------------------------------------------------
Name: item-inventory-add

Purpose: used for payables (NOT for inventory items, which aren't handled in QB)

Notes/Discussion:
dr AR at price * units
cr Income Account at price * units
dr Expense Account at cost * units
cr AssetAccount at cost * units. Asset account does not have to be inventory, could be a credit account such as AP

The AR account isn't specified in message, assume it

TO DO: Not sure how to set PurchaseCost and SalesPrice.

Can we specify PurchaseCost and SalesPrice when writing line item of invoiceInvoiceLine?

InvoiceLineItem allows to specify rate and price

For material, where

Winding Services
DAWN OF WISDOM: MAterials aren't an inventory item!! b/c we don't record cost
Whereas Windings-service ARE and inventory item, b/c we do need to

dr Accounts Receivable  qty * SalesPrice/Rate??  [need to check whether it uses SalesPrice or Rate, if rate present/calculated - by symmetry with non-inv, 
cr Income Account       qty * SalesPrice/Rate??
dr ExpenseAccount       qty * PurchaseCost  [fairly certain qty * purchase cost is invariant regardless of Rate or Amount sent in InvoiceLineItem]
cr InventoryAsset       qty * PurchaseCost

For service, Set Cost to 1 and set Quantity as units, so Rate = $1.00

Want following mapping:
AR->AR
Income-account -> Service Revenue
Expense Account -> COGS - Winding
Inventory Asset -> Service Payable [need to try Accounts Payable, though]

Note we hardcode Purchase Cost to 1.0, so that qty(in dollars)  * 1.0 will be credited to the accrual account

Price is contractually constrained to one  or zero. One will signify charge customer qty*1.
Zero will result to no charge to customer (zero increase in AR, Sales), and we will config QB not to print zero-amount line items
Both zero and one will result in cost @ (qty * 1.0) being accrued to desired accrual account.
--------------------------------------------------------------------------------------------------------
|#
(define/contract (item-inventory-add name
                                     income-acc
                                     expense-acc
                                     accrual-acc
                                     sales-tax-code
                                     charge-customer?)
  (->  string? qbid/c qbid/c qbid/c qbid/c boolean? (-> integer? xexpr?))
  (λ (requestID) 
    `(ItemInventoryAddRq ((requestID ,(number->string requestID)))
                         (ItemInventoryAdd (Name ,name)
                                           (SalesTaxCodeRef  (ListID ,sales-tax-code))
                                           (SalesPrice  ,(if charge-customer? "1.0" "0.0"))
                                           (IncomeAccountRef (ListID ,income-acc))
                                           (PurchaseCost "1.0")
                                           (COGSAccountRef   (ListID ,expense-acc))
                                           (AssetAccountRef  (ListID ,accrual-acc))))))

#|
--------------------------------------------------------------------------------

Discussion: Materials are, counterintuitively, a non-inventory item.
  Just dr Accounts Receivable @  qty * rate (both specified on InvoiceLineItem)
       cr Sales Revenue(for example)       @  qty * rate (both specified on InvoiceLineItem)

The account to credit is the credit-account-id
  the debit account is AccountsReceivable and does not appear to be mutable.

  Price Not required. Will send across with item.
we truncate b/c system can get-or-reify here
--------------------------------------------------------------------------------
|#
(define/contract (item-non-inventory-add name
                                         credit-acc
                                         sales-tax-code)
  (->  string? qbid/c qbid/c (-> integer? xexpr?))
  (λ (requestID) 
    `(ItemNonInventoryAddRq ((requestID ,(number->string requestID)))
                            (ItemNonInventoryAdd (Name ,(substring name 0 (min (string-length name) 31)))
                                                 (SalesTaxCodeRef  (ListID ,sales-tax-code))
                                                 (SalesOrPurchase
                                                  (AccountRef (ListID ,credit-acc)))))))


(define/contract (item-service-add name
                                   credit-acc
                                   sales-tax-code)
  (->  string? qbid/c qbid/c (-> integer? xexpr?))
  (λ (requestID) 
    `(ItemServiceAddRq ((requestID ,(number->string requestID)))
                       (ItemServiceAdd (Name ,name)
                                       (SalesTaxCodeRef  (ListID ,sales-tax-code))
                                       (SalesOrPurchase
                                        (AccountRef (ListID ,credit-acc)))))))

#|
sales-tax-code = tax -> discount applied before tax [WANT THIS]
sales-tax-code = non -> discount applied after tax  [don't want this]
|#
(define/contract (item-discount-add name
                                    credit-acc
                                    sales-tax-code
                                    discount-pct)
  (->  string? qbid/c qbid/c !neg-two-sigs/c (-> integer? xexpr?))
  (λ (requestID) 
    `(ItemDiscountAddRq ((requestID ,(number->string requestID)))
                        (ItemDiscountAdd (Name ,name)
                                         (SalesTaxCodeRef (ListID ,sales-tax-code))
                                         (DiscountRatePercent ,(number->string discount-pct))
                                         (AccountRef (ListID ,credit-acc))))))

(define/contract (item-subtotal-add name)
  (->  string?  (-> integer? xexpr?))
  (λ (requestID) 
    `(ItemSubtotalAddRq ((requestID ,(number->string requestID)))
                        (ItemSubtotalAdd (Name ,name)))))



#|
--------------------------------------------------------------------------
Name: Vendor-add

Purpose: specialization of vendor add that uses the ListId to ref. vendor-type (vs full-name, in vendor add)
will allow us to use custom vendor-types that match box lunch, if so required.



----------------------------------------------------------------------------
|#
(define/contract (vendor-add name vendor-type #:currency [currency null])
  (->* (string? qbid/c) (#:currency qbid-or-fresh/c) (-> integer? xexpr?))
  (λ (requestID) 
    `(VendorAddRq ((requestID ,(number->string requestID)))
                  ,(filter (λ (val) (not (void? val)))
                           `(VendorAdd (Name ,name)
                                       (VendorTypeRef (ListID ,vendor-type))
                                       ,(unless (null? currency) `(CurrencyRef (ListID ,currency))))))))
                                      
    
#|
---------------------------------------------------------------------------------------------------
Name: ;DataExtensionDefinition
Discussion: 

Unknown: DataExtListRequire and DataExtTxnRequire BOOLTYPE flags are documented, but undescribed.
Don't understand if we need them.

---------------------------------------------------------------------------------------------------
|#
(define/contract (data-ext-def-add  owner-guid data-ext-name)
  (-> string? string? (-> integer? xexpr?))
  (λ (requestID) 
    `(DataExtDefAddRq ((requestID ,(number->string requestID)))
                      (DataExtDefAdd
                       (OwnerID        ,owner-guid)
                       (DataExtName    ,data-ext-name)
                       (DataExtType    ,STR255TYPE)
                       (AssignToObject ,QB/CUSTOMER)
                       (AssignToObject ,QB/VENDOR)
                       (AssignToObject ,QB/ITEM)
                       (AssignToObject ,QB/ACCOUNT)
                       (AssignToObject ,QB/INVOICE)
                       (AssignToObject ,QB/ITEM-RECEIPT)
                       (AssignToObject ,QB/BILL)
                       (AssignToObject ,QB/JOURNAL-ENTRY)
                       (AssignToObject ,QB/CREDIT-MEMO)))))


(define/contract (data-ext-def-delete owner-guid data-ext-name)
  (-> string? string? (-> integer? xexpr?))
  (λ (requestID) 
    `(DataExtDefDelRq ((requestID ,(number->string requestID)))
                      (OwnerID ,owner-guid)
                      (DataExtName ,data-ext-name))))


#|
----------------------------------------------------------------------------
Name: data-ext-add|mod
Purpose: adds|modifies private data to referenced list-id object.

Existence of data denotes a map between a box lunch PKID and the QB ListID

value of the data is the map itself, stored as ns:pkid|listID

non-existence implies a mapping does not exist

We store the data as ns:pkid|listID so we can easily retrieve with XPATH library

There is a alternate form of private data: ns:pkid.step|fake
which is used only for item-receipts and part of the blocking function which
prevents re-inserting an unpublished transcation, in the event of partial success

----------------------------------------------------------------------------
|#

(define/contract (data-ext-helper msg-name)
  (-> (or/c "DataExtAdd" "DataExtMod")
      (-> qb-obj-type/c qbid/c string?  (-> integer? xexpr?)))
  (λ (object-type object-id value)
    (λ (requestID) 
      `(,(string->symbol (string-append msg-name "Rq")) ((requestID ,(number->string requestID)))
        (,(string->symbol msg-name)
         (OwnerID ,OWNER-GUID)
         (DataExtName ,DATA-EXTENSION-NAME)
         ,@(cond [(qb-list-type/c object-type) `((ListDataExtType ,object-type)
                                                 (ListObjRef
                                                  (ListID ,object-id)))]
                 [(qb-txn-type/c  object-type) `((TxnDataExtType ,object-type)
                                                 (TxnID ,object-id))]
                 [else (error (format "Run off in data-ex-add with object-type ~a\n" object-type))])
         (DataExtValue ,value))))))
  
(define/contract data-ext-add
  (-> qb-obj-type/c qbid/c string?  (-> integer? xexpr?))
  (data-ext-helper "DataExtAdd"))
                        
(define/contract data-ext-mod 
  (-> qb-obj-type/c qbid/c string? (-> integer? xexpr?))
  (data-ext-helper "DataExtMod"))


#|
--------------------------------------------------------------------------
invoice-add and invoice-line-add

Note that the OCR shown only 0-1 occurrence of invoice-line-add
per invoice-add, but the documentation seems to allow for
multiple. Check against realit

HOW TO SET qty, rate, and amount

item-type                                qty    rate['price-each']   amount
------------------------------------------------------------------------------
sales-tax                                n/a       n/a              n/a
reified.  passed  as ItemSalesTaxRef to invoice-add. Not in invoice-line-add
BUT: Will pass sales tax-code (NO/YES) to individual line items to obtain
     desired behavior wrt taxing sample sets

discount                                 null        null         null
comments: QB will calcluate discount off immediate antecedent in item-sequence
          => should usually see *subtotal-line* followed by *discount-line*
          Will NOT cacluate off all prior entrie

subtotal                                 null        null         null
comments: QB will calculate subtotal after, and excluding, prior subtotal
          does not provide a running subtotal

service(ex: freight-*-accrual)          null        null         $-amount
comments: QB will unify displayed-price with amount, and leave QTY blank


non-inventory(ex: yarn)                      lbs-sold    $price       null
comments: QB will calculate & display amount as qty*price


inventory(winding,boxing)                $value      null         null


So based on this behvaior typical sequence of line items is
material+
subtotal
discount
winding
boxing (w/zero price)
freight

Write test code to confirm this behavior is correct. Write test code
to compute your expected total and match with QB return value

So based on this, we will create specialized invoice-line-add functions
vs a generic invoice-line-add

Taxation  R(resale-certificate, item)

Resale Cert            Item        SALES-TAX-CODE
-------------------------------------
   Y                 !-sample-set  NO
   N                 !-sample-set  YES
   -                  sample-set    YES

Per-line-item logic
So R(resale-certificate?,shipping?,sample-set? destination-in-NY) -> TAX-CODE
and destination-in-NY? (cond [sample-set?  YES] ;tautology
                              [else      (not resale-certificate?)]) ;negation of resale-certificate

destination-in-NY? will be handled by the invoice-level sales tax field. If not in NY, will pass null
     
We'll always set the invoice-level tax code per the correct jurisdiction

Then set individual line item code per algorithm above

Note the invoice-line-adds are all xexpr returning
-----------------------------------------------------------------------------
|#



(define (common-invoice-line-add package)
  (-> (listof xexpr?) (-> qbid/c qbid/c string? xexpr?))
  (λ (item tax-code desc)
    `(InvoiceLineAdd (ItemRef (ListID ,item))
                     (Desc   ,desc)
                     ,@package
                     (SalesTaxCodeRef (ListID ,tax-code)))))

;WE ALWAYS WANT TAX-YES on the discount.
(define invoice-line-add/discount (λ (item desc) ((common-invoice-line-add null) item TAX-YES desc)))

;note subttotal has no tax-properties. 
(define/contract (invoice-line-add/subtotal item desc)
  (-> qbid/c string?  xexpr?)
  `(InvoiceLineAdd (ItemRef (ListID ,item))
                   (Desc   ,desc)))

;define total-$ as two-sigs to allow zero amount for boxing, and other stuff don't charge
;customer for
(define/contract (invoice-line-add/inventory item tax-code desc total-$)
  (->  qbid/c qbid/c string? !neg-two-sigs/c xexpr?)
  ((common-invoice-line-add `((Quantity ,(number->string total-$))))
   item tax-code desc))

(define/contract (invoice-line-add/service item tax-code desc amt)
  (->  qbid/c qbid/c string? two-sigs/c xexpr?)
  ((common-invoice-line-add `((Amount ,(number->string amt))))
   item tax-code desc))

(define/contract (invoice-line-add/non-inventory item tax-code desc weight price/weight)
  (->  qbid/c qbid/c string? +two-sigs/c +two-sigs/c xexpr?)
  ((common-invoice-line-add  `((Quantity ,(number->string weight))
                               (Rate     ,(number->string price/weight))))
   item tax-code desc))



;QBXML differs in structure from the customer ship-to address: Name field missing, additional address-n fields included


(define (invoice-address-helper sym)
  (λ (st)
    (filter (λ (val) (not (void? val)))
            `(,sym (Addr1 ,(ship-to-company st))
                   ,(awith (ship-to-attn st)
                           (unless (null? it) `(Addr2 ,it)))
                   (Addr3 ,(ship-to-addr1 st))
                   ,(awith (ship-to-addr2 st)
                           (unless (null? it) `(Addr4 ,it)))
                   ,(awith (ship-to-addr3 st)
                           (unless (null? it) `(Addr5 ,it)))
                   (City  ,(ship-to-city st))
                   (State ,(ship-to-state st))
                   (PostalCode ,(ship-to-postal st))
                   (Country  ,(ship-to-country st))))))

(define ship-helper (invoice-address-helper 'ShipAddress))
(define bill-helper (invoice-address-helper 'BillAddress))




#|
----------------------------------------------------------------------------------------
Name: item-receipt-add

DISCUSSION:

item-receipt-add is the only QBXML mutation-message intended to be used in a
multi-message message-set. All others are single-message per message set, which preseves atomicity
of changes and rollbacks

We take added risk of sending a second message with each item-receipt-add
persisting a private data element with the transaction number
so that we can block redundant attempts to persist the same transaction, in the event
we have a partial mutation of a message-set (i.e x of y messages succeed).

We accomplish such blocking by checking whether we can find the publication-log- transaction(plt) pkid
within the set of item-receipts' private data

IMPLEMENTATION-NOTES:

1) b/c multiple messages can have the same transaction number, which would cause failure of make-immutable hash

We Modify the key from pl:<pkid> to pl:<pkid>.<sequence-number> 

2) We set the  request id of the second message to 1E4 + the first message's request ID. This will allow
9999 messages in a single message set

3) We use defMacro, useMacro to return the TxnID for setting private data

4) We would like to write the TxnID into the appropriate portion of the private-data value
But the useMacro facility can't be used in such manner. So we leave it as fake.

If we need the txnID, we can overwrite the private data

TO DO: check behavior when item-receipt is changed to bill. Is private data preserved?


------------------------------------------------------------------------------------------
|#




(define/contract (item-receipt-add plt vendor date ref-number fx-rate line-items)
  (-> pkid/c qbid/c qb-date/c string? +fx/c (listof xexpr?) (-> integer? (listof xexpr?)))
  (λ (requestID)
    `((ItemReceiptAddRq ((requestID ,(number->string requestID)))
                        (ItemReceiptAdd ((defMacro ,(format "TxnID:~a" requestID)))
                                        (VendorRef (ListID ,vendor))
                                        (TxnDate ,date)
                                        (RefNumber ,ref-number)
                                        (ExchangeRate ,(real->decimal-string fx-rate 6))
                                        ,@line-items))
      (DataExtAddRq ((requestID ,(number->string (+ 10000 requestID))))
                    (DataExtAdd (OwnerID ,OWNER-GUID)
                                (DataExtName ,DATA-EXTENSION-NAME)
                                (TxnDataExtType ,QB/ITEM-RECEIPT)
                                (TxnID ((useMacro ,(format "TxnID:~a" requestID))))
                                (DataExtValue ,(format "pl:~a.~a|fake" plt requestID)))))))

(define (common-item-receipt-line-add package)
  (-> (listof xexpr?) (-> qbid/c  string? xexpr?))
  (λ (item desc)
    `(ItemLineAdd (ItemRef (ListID ,item))
                  (Desc   ,desc)
                  ,@package)))

(define item-receipt-line-add/subtotal (common-item-receipt-line-add null))
           

(define/contract (item-receipt-line-add/service item desc amt)
  (->  qbid/c string? two-sigs/c xexpr?)
  ((common-item-receipt-line-add `((Amount ,(real->decimal-string amt 2))))
   item desc))

(define/contract (item-receipt-line-add/non-inventory item desc weight price/weight)
  (->  qbid/c string? +two-sigs/c +two-sigs/c xexpr?)
  ((common-item-receipt-line-add  `((Quantity ,(real->decimal-string weight 2))
                                    (Rate     ,(real->decimal-string price/weight 2))))
   item desc))


#|
-----------------------------------------------------------------------------------------------
Journal-Entry-Add

Used For generation of test-balance


----------------------------------------------------------------------------------------------
|#
(define/contract (journal-entry-add date ref-number line-items)
  (-> qb-date/c string? (listof xexpr?) (-> integer? xexpr?))
  (λ (requestID)
    `(JournalEntryAddRq ((requestID ,(number->string requestID)))
                        (JournalEntryAdd (TxnDate ,date)
                                         (RefNumber ,ref-number)
                                         ,@line-items))))

(define/contract (journal-line-helper dc)
  (-> DC/c (->* (qbid/c two-sigs/c) (#:entity  qbid-or-fresh/c)  xexpr?))                                    
  (λ (account amt #:entity [entity null])
    (filter (λ (val) (not (void? val)))
            `(,(case dc
                 [(+1)  'JournalDebitLine]
                 [(-1) 'JournalCreditLine]
                 [else (format "runoff case in journal-line-helper with ~a" dc)])
              (AccountRef (ListID ,account))
              (Amount ,(real->decimal-string amt 2))
              ,(unless (null? entity) `(EntityRef (ListID ,entity)))))))

;for testing only
(define-values (journal-debit-line
                journal-credit-line)
  (values (journal-line-helper +1)
          (journal-line-helper -1)))

#|/************************************************

*******************************************************|#

#|
-------------------------------------------------------------
invoice-add and invoice-line-add

Note that the OCR shown only 0-1 occurrence of invoice-line-add
per invoice-add, but the documentation seems to allow for
multiple. Check against realit

HOW TO SET qty, rate, and amount

item-type                                qty    rate['price-each']   amount
------------------------------------------------------------------------------
sales-tax                                n/a       n/a              n/a
reified.  passed  as ItemSalesTaxRef to invoice-add. Not in invoice-line-add
BUT: Will pass sales tax-code (NO/YES) to individual line items to obtain
     desired behavior wrt taxing sample sets

discount                                 null        null         null
comments: QB will calcluate discount off immediate antecedent in item-sequence
          => should usually see *subtotal-line* followed by *discount-line*
          Will NOT cacluate off all prior entrie

subtotal                                 null        null         null
comments: QB will calculate subtotal after, and excluding, prior subtotal
          does not provide a running subtotal

service(ex: freight-*-accrual)          null        null         $-amount
comments: QB will unify displayed-price with amount, and leave QTY blank


non-inventory(ex: yarn)                      lbs-sold    $price       null
comments: QB will calculate & display amount as qty*price


inventory(winding,boxing)                $value      null         null


So based on this behvaior typical sequence of line items is
material+
subtotal
discount
winding
boxing (w/zero price)
freight

Write test code to confirm this behavior is correct. Write test code
to compute your expected total and match with QB return value

So based on this, we will create specialized invoice-line-add functions
vs a generic invoice-line-add

Taxation  R(resale-certificate, item)

Resale Cert            Item        SALES-TAX-CODE
-------------------------------------
   Y                 !-sample-set  NO
   N                 !-sample-set  YES
   -                  sample-set    YES

Per-line-item logic
So R(resale-certificate?,shipping?,sample-set? destination-in-NY) -> TAX-CODE
and destination-in-NY? (cond [sample-set?  YES] ;tautology
                              [else      (not resale-certificate?)]) ;negation of resale-certificate

destination-in-NY? will be handled by the invoice-level sales tax field. If not in NY, will pass null
     
We'll always set the invoice-level tax code per the correct jurisdiction

Then set individual line item code per algorithm above

Note the invoice-line-adds are all xexpr returning
-----------------------------------------------------------------------------
|#


(define (common-credit-memo-line-add package)
  (-> (listof xexpr?)
      (-> qbid/c qbid/c string? xexpr?))
  (λ (item tax-code desc)
    `(CreditMemoLineAdd (ItemRef (ListID ,item))
                     (Desc   ,desc)
                     ,@package
                     (SalesTaxCodeRef (ListID ,tax-code)))))



;WE ALWAYS WANT TAX-YES on the discount.
;WE ALWAYS WANT TAX-YES on the discount.
(define credit-memo-line-add/discount (λ (item desc) ((common-credit-memo-line-add null) item TAX-YES desc)))


;note subttotal has no tax-properties. 
(define/contract (credit-memo-line-add/subtotal item desc)
  (-> qbid/c string?  xexpr?)
  `(CreditMemoLineAdd (ItemRef (ListID ,item))
                      (Desc   ,desc)))

;define total-$ as two-sigs to allow zero amount for boxing, and other stuff don't charge
;customer for
(define/contract (credit-memo-line-add/inventory item tax-code desc total-$)
  (->  qbid/c qbid/c string? !neg-two-sigs/c xexpr?)
  ((common-credit-memo-line-add `((Quantity ,(number->string total-$))))
   item tax-code desc))

(define/contract (credit-memo-line-add/service item tax-code desc amt)
  (->  qbid/c qbid/c string? two-sigs/c xexpr?)
  ((common-credit-memo-line-add item `((Amount ,(number->string amt))))
   item tax-code desc))

(define/contract (credit-memo-line-add/non-inventory item tax-code desc weight price/weight)
  (->  qbid/c qbid/c string? +two-sigs/c +two-sigs/c xexpr?)
  ((common-credit-memo-line-add  `((Quantity ,(number->string weight))
                                     (Rate     ,(number->string price/weight))))
   item tax-code desc))

#|
----------------------------------------------------------------------------------------------
Name: credit-memo-add

Implementation Notes:  will only transmit pending credit-memo, so hardcode IsPending -> #t

----------------------------------------------------------------------------------------------
|#



(define/contract (common-customer-facing-add objname)
  (-> string? (-> qbid/c qb-date/c qbid/c (listof xexpr?) (-> integer? xexpr?)))
  (λ (customer date sales-tax-jurisdiction line-items)
    (λ (requestID)
      `(,(string->symbol (string-append objname "AddRq")) ((requestID ,(number->string requestID)))
                          (,(string->symbol (string-append objname "Add")) (CustomerRef (ListID ,customer))
                          (TxnDate ,date)
                          (IsPending ,(bool->xml MARK-INVOICES-AS-PENDING?))
                          (ItemSalesTaxRef (ListID ,sales-tax-jurisdiction))
                          ,@line-items)))))


(define credit-memo-add (common-customer-facing-add "CreditMemo"))
(define invoice-add     (common-customer-facing-add "Invoice"))                            



(define/contract (common-customer-facing-add-ex objname)
  (-> string?   (-> qbid/c qb-date/c qbid/c ship-to? ship-to? (listof xexpr?) (-> integer? xexpr?)))
  (λ (customer date sales-tax-jurisdiction bill-to ship-to line-items)
    (λ (requestID)
      `(,(string->symbol (string-append objname "AddRq")) ((requestID ,(number->string requestID)))
                                   (,(string->symbol (string-append objname "Add")) (CustomerRef (ListID ,customer))
                                    (TxnDate ,date)
                                    ,(bill-helper bill-to)
                                    ,(ship-helper ship-to)
                                    (IsPending ,(bool->xml MARK-INVOICES-AS-PENDING?))
                                    (ItemSalesTaxRef (ListID ,sales-tax-jurisdiction))
                                    ,@line-items)))))


(define credit-memo-add-ex (common-customer-facing-add-ex "CreditMemo"))
(define invoice-add-ex     (common-customer-facing-add-ex "Invoice"))                            



(define/contract (vendor-query-for-private-data-by-currency cur)
  (-> string? (-> integer? xexpr?))
  (λ (requestID) 
    `(VendorQueryRq ((requestID ,(number->string requestID)))
                    (CurrencyFilter (FullName ,cur))
                    (IncludeRetElement "DataExtRet")
                    (OwnerID ,OWNER-GUID))))

(require rackunit
         rackunit/text-ui)

(define internals
  (test-suite
   "Tests for internal functions"
   (test-equal? "expected"
                `(JournalDebitLine (AccountRef (ListID "cheese")) (Amount "2.43"))
                (journal-debit-line "cheese" 2.43))
   (test-equal? "expected"
                `(JournalDebitLine (AccountRef (ListID "cheese")) (Amount "2.43") (EntityRef (ListID "me")))
                (journal-debit-line "cheese" 2.43 #:entity "me"))
   (test-equal? "expected"
                `(JournalCreditLine (AccountRef (ListID "cheese")) (Amount "2.43"))
                (journal-credit-line "cheese" 2.43))
   (test-equal? "expected"
                `(JournalCreditLine (AccountRef (ListID "cheese")) (Amount "2.43") (EntityRef (ListID "me")))
                (journal-credit-line "cheese" 2.43 #:entity "me"))
   (test-equal? "expected"
                `(JournalEntryAddRq ((requestID "1"))
                                    (JournalEntryAdd (TxnDate "2015-10-01")
                                                     (RefNumber "blewie")
                                                     (JournalCreditLine (AccountRef (ListID "cheese")) (Amount "2.43") (EntityRef (ListID "me")))))
                ((journal-entry-add "2015-10-01"
                                   "blewie"
                                   (list (journal-credit-line "cheese" 2.43 #:entity "me"))) 1))
   (awith (ship-to "cheese" null "air" null null  "Homestead" "FL" "10950" "united states")
          (test-equal? "expected"
                       '(ShipAddress
                         (Addr1 "cheese")
                         (Addr3 "air")
                         (City "Homestead")
                         (State "FL")
                         (PostalCode "10950")
                         (Country "united states"))
                       (ship-helper it))
          (test-equal? "expected"
                       '(BillAddress
                         (Addr1 "cheese")
                         (Addr3 "air")
                         (City "Homestead")
                         (State "FL")
                         (PostalCode "10950")
                         (Country "united states"))
                       (bill-helper it)))
   ))

   
  

