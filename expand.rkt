#lang racket/base

(require (for-template racket/base)
         (prefix-in x: xml)
         syntax/parse)

(provide kapro-syntax
         expand-template)

;; Some of these don't work quite right because some xexpr procedures do not
;; allow all of the possible child values
(define-syntax-class leaf
  [pattern s:string]
  [pattern i:id]
  [pattern ch:char
    #:fail-unless
    (x:valid-char? (syntax-e #'ch))
    "xml valid-character"]
  [pattern v
    #:fail-unless
    (x:cdata? (syntax-e #'v))
    "xml cdata"]
  [pattern v
    #:fail-unless
    (x:comment? (syntax-e #'v))
    "xml comment"]
  [pattern v
    #:fail-unless
    (x:p-i? (syntax-e #'v))
    "xml processing instruction"])

(define-syntax-class element-attrs
  [pattern ((names:id values:string) ...)
    #:with quoted #'(list (list 'names 'values) ...)])

(define-syntax-class element
  [pattern (name attrs:element-attrs children ...)
    #:with attrs-quoted #'attrs.quoted])

(struct kapro-syntax (proc)
  #:property prop:procedure (struct-field-index proc))

(define-syntax-class ksyntax
  [pattern (v . rest)
    #:declare v (static kapro-syntax? "kapro syntax")])

(define expand-template
  (syntax-parser
    [(expand val:leaf) #''val]
    [(expand node:ksyntax)
     ((syntax-local-value #'node.v) this-syntax)]
    [(expand node:element)
     #'(list 'node.name node.attrs-quoted (expand node.children) ...)]))