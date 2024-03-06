#lang racket/base

(require syntax/strip-context
         xml)

(provide read-syntax
         read)

(define (read-syntax src in)
  (with-syntax ([stx (syntax:read-xml in #:src src)])
    (strip-context #'(module mod kapro/lang stx))))

(define (read in)
  (syntax->datum (syntax:read-xml in)))
