#|
ADO-constants: provides certain enumerations necessary to integrate racket with Microsoft's ADO library.
  
Needs to be module-form vs #lang form b/c we're using it in a dynamic-place


NOTE RE adDBTimeStamp

     Use adDBTimeStamp as parameter for sending values to STORED PROCEDURE parameters 
           specified as datetime (or date, or time, etc.)

      adDBTimeStamp is the only data type that appears supported by ado and the SQL Server driver
|#

(module ado-constants racket/base

(require racket/contract
         racket/list)

(provide (struct-out ado-field)
         vbTrue
         adUseClient
         adEmpty adSmallInt adInteger
         adSingle adDouble adCurrency
         ;adDate not supported by SQL Server driver
         adBSTR adIDispatch
         adError adBoolean adVariant
         adIUnknown adDecimal adTinyInt
         adUnsignedTinyInt adUnsignedSmallInt adUnsignedInt
         adBigInt  adUnsignedBigInt adFileTime
         adGUID  adBinary    adChar
         adWChar adNumeric   adUserDefined
         ;adDBDate adDBTime not supported by SQL Server driver
         adDBTimeStamp ; this appears to be the only date type supported by SQL server driver 
         adChapter adPropVariant adVarNumeric
         adVarChar adLongVarChar adVarWChar
         adLongVarWChar adVarBinary adLongVarBinary
         adCmdText adCmdTable adCmdStoredProc adCmdUnknown adCmdFile  adCmdTableDirect
         adParamUnknown adParamInput adParamOutput adParamInputOutput adParamReturnValue
         adAsyncExecute    adAsyncFetch   adAsyncFetchNonBlocking   adExecuteNoRecords   adExecuteStream
         ;new - decimal(12,6) for persisting spot FX rates
         adFX
         BEGIN COMMIT ROLLBACK
         whole/c
         handle/c
         mangle-rkt-id>TSQL-id
         @RETURN_VALUE
         @OUTPUT
         adClipString
         serialize-ado-field
         deserialize-ado-field)

;value the driver assigns to name of 0th parameter (the return value)
(define @RETURN_VALUE "@RETURN_VALUE")

;value we assign to a special output-type parameter in the database
(define @OUTPUT "@OUTPUT")

(define adClipString 2)

;joe-dirt ->@joe_dirt
(define/contract (mangle-rkt-id>TSQL-id sym)
  (-> symbol? string?)
  (regexp-replace #rx"^"  
                (regexp-replace* #rx"-"  (symbol->string sym) "_" ) "@" ))

(define whole/c  exact-nonnegative-integer?)
(define handle/c whole/c)

(define adUseClient 3)

(define vbTrue -1)

(define-values
  (BEGIN COMMIT ROLLBACK) (values 1 2 3))

;ADO.ParameterDirectionEnum
(define-values
  (adParamUnknown adParamInput adParamOutput adParamInputOutput adParamReturnValue)
  (values 0 1 2 3 4))

;ADO.CommandTypeEnum
(define-values
  (adCmdText adCmdTable adCmdStoredProc adCmdUnknown adCmdFile  adCmdTableDirect)
  (values 1 2 4 8 256 512))

;ADO.DataTypeEnum
(define-values
  (adEmpty adSmallInt adInteger
           adSingle adDouble adCurrency
           adDate adBSTR adIDispatch
           adError adBoolean adVariant
           adIUnknown adDecimal adTinyInt
           adUnsignedTinyInt adUnsignedSmallInt adUnsignedInt
           adBigInt  adUnsignedBigInt adFileTime
           adGUID  adBinary    adChar
           adWChar adNumeric   adUserDefined
           adDBDate adDBTime adDBTimeStamp
           adChapter adPropVariant adVarNumeric
           adVarChar adLongVarChar adVarWChar
           adLongVarWChar adVarBinary adLongVarBinary
           ;new - decimal(12,6) for persisting spot rates. Not a Microsoft Constant
           adFX)
  (values 0 2 3
          4 5 6
          7 8 9 
          10 11 12
          13 14 16
          17 18 19
          20 21 64
          72 128 129
          130 131 132
          133 134 135
          136 138 139
          200 201 202
          203 204 205
          1310))

;ADO.ExecuteOptionEnum
(define-values
  (adAsyncExecute
   adAsyncFetch
   adAsyncFetchNonBlocking
   adExecuteNoRecords
   adExecuteStream)
  (values 16 32 64 128 1024))

  (define-struct/contract ado-field
  ([name string?]
   [value any/c]
   [type  (or/c symbol? integer?)] 
   [direction  integer?])
  #:transparent) ;required for equal? comparisons  

(define/contract (serialize-ado-field adof)
  (-> ado-field? (list/c string? any/c integer? integer?))
  (list (ado-field-name adof) (ado-field-value adof) (ado-field-type adof) (ado-field-direction adof)))

(define/contract (deserialize-ado-field lst)
  (-> (list/c string? any/c integer? integer?) ado-field?)
  (ado-field (first lst) (second lst) (third lst) (fourth lst)))

)
  
