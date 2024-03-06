#lang racket/base

(require (for-syntax racket/base
                     "expand.rkt")
         racket/match
         racket/stxparam
         syntax/parse/define
         html-writer)

(provide (rename-out [mb #%module-begin])
         k:content)

(define-syntax-rule (mb body)
  (#%module-begin
   (provide write-template
            render-template)
   (define (write-template [context (hash)] [out (current-output-port)])
     (void
      (write-string (xexpr->html5 (render-template context) #:wrap-at 80) out)))

   (define (render-template [context (hash)])
     (syntax-parameterize ([the-context (make-rename-transformer #'context)])
       (xml-expand-template body)))))

(define-syntax-parameter the-context #f)

(define-syntax (xml-expand-template stx)
  (expand-template stx))

;; XXX: missing value in context reports this location not the
;; location from the name
(define-syntax-parse-rule (context-lookup name:id)
  (let ([v (hash-ref the-context 'name)])
    (cond
      [(procedure? v) (v the-context)]
      [else v])))

(define-syntax k:content
  (kapro-syntax
   (syntax-parser
     [(_ (_ (({~datum value} name:string)) _ ...))
      #:with name-id
      (string->symbol (syntax-e #'name))
      #'(context-lookup name-id)])))