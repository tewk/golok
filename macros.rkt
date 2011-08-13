#lang racket/base
(provide for/filter)
(define-syntax-rule (for/filter ([x xr]) body)
  (reverse (for/fold ([r null]) ([x xr])
  (define body-result (begin body))
  (if body-result
    (cons x r)
    r))))

