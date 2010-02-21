#!/usr/bin/env mzscheme
#lang scheme

;
; dump-1e.sh
;
; Dump the 1e models from an amf description to GraphViz source files.
;
; Usage:
;       ./dump-1e.sh <amf>
;
; (places dot source files in the "output" directory)
;
; Example:
;       ./dump-1e.sh examples/dme.amf
;
; creates the 1e model for the distributed mutual exclusion protocol
;

(require scheme/system)

(require "datatypes.scm")
(require "parser.scm")
(require "topo-datatypes.scm")
(require "model-builder.scm")

(define output-directory "output/")

(define args (current-command-line-arguments))

(if (not (eq? (vector-length args) 1))
  (error "need to specify .amf file on command line")
  (void))

(define prot_name (vector-ref (current-command-line-arguments) 0))

; protocol and topo to dump
(define prot (parse-amf-file prot_name))

(define-values (tt mb2) (build-oneEmodel-builder prot))

(for-each (lambda (x)
          (model2dot (mb2 x) (string-append output-directory "dump-1E-" 
                            (symbol->string x) ".dot") #:show-buf #f))
                             (protocol-process-names prot))
