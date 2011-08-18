#lang racket/base
; front-end for calling find-k from the command line
; David Samuelson
(require (for-syntax racket/base))

;; version header -- should be in form vx.y.z(-tag)
(define version "v1.2.1")
(provide version)

; parsed values of command line arguments
; currently size 11

(define-syntax (define-global stx)
  (syntax-case stx ()
    [(_ name default)
     (with-syntax ([set!name (string->symbol (string-append "set!-" (symbol->string (syntax->datum #'name))))])
     #'(begin
         (define name default)
         (define (set!name x) (set! name x))
         (provide name set!name)))]))

(define-global debug 0)             ; information level
(define-global max-t #f)            ; number of seconds to halt after (only checked between building models)
(define-global dump-1E #f)          ; write out oneE models
(define-global dump-sys #f)         ; write out system models
(define-global dfs #f)              ; use dfs instead of bfs
(define-global ring #f)             ; ring partial order reduction
(define-global star (list))         ; star partial order reduction
(define-global npp (list))          ; non-parameterized processes ; TODO: should be able to discover this from addition rules
(define-global dump #f)             ; just dump the model at a particular depth
(define-global output-directory #f) ; output directory
(define-global start-depth #f)      ; fast forward (skip) searching n levels of bfs tree ; TODO: not implemented
(define-global stop-depth #f)       ; stop at depth ; TODO: not implemented
(define-global process-type #f)     ; name of process type to check
(define-global pruning #t)          ; branch pruning
(define-global isplace #f)          ; is this a worker place
