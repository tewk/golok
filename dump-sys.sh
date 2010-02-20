#!/usr/bin/env mzscheme

#lang scheme

;
; dump-sys.sh
;
; Dump a system instance to file.
;
; Usage:
;       ./dump-1e.sh <amf> <topo>
;
; (places dot source files in the "output" directory)
;
; Example:
;        ./dump-1e.sh examples/lr-dpp.amf examples/lr-dpp-33.topo
;
; builds the lr-dpp model with 3 left and 3 right philosoophers
;


(require scheme/system)

(require errortrace)

(require "datatypes.scm")
(require "parser.scm")
(require "topo-datatypes.scm")
(require "model-builder.scm")
(require "lookup-table.scm")

(define args (current-command-line-arguments))

(define input-directory "examples/")

(define output-directory "output/")

(define output-name "sys-dump.dot")

(define amf-name (void))
(define topo-name (void))

(if 
  (or (eq? (vector-length args) 3) (eq? (vector-length args) 2))
    (begin
      (set! amf-name (vector-ref (current-command-line-arguments) 0))
      (set! topo-name (vector-ref (current-command-line-arguments) 1)))
  (error "need to specify protocol name and topology file on command line"))

(if (eq? (vector-length args) 3)
  (set! output-name  (vector-ref (current-command-line-arguments) 2)) (void))

; protocol and topo to dump
(define prot (parse-amf-file amf-name))
(define topo (file->topology topo-name))

(define-values (lt mb) (build-oneEmodel-builder prot))

(debug-lookup-table lt)

(define smb (build-sysNmodel-builder prot))

(define mdl (smb topo #t))
(model2dot mdl (string-append output-directory output-name))
