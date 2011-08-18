#lang racket/base
(provide for/filter
         for/filter-map)
(define-syntax-rule (for/filter ([x xr] rs ...) body)
  (reverse (for/fold ([r null]) ([x xr] rs ...)
  (define body-result (begin body))
  (if body-result
    (cons x r)
    r))))

(define-syntax-rule (for/filter-map ([x xr] rs ...) body)
  (reverse (for/fold ([r null]) ([x xr] rs ...)
  (define body-result (begin body))
  (if body-result
    (cons body-result r)
    r))))
