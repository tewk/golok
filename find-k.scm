;;
;; find-k.scm
;;
;; Top level algorithm for finding the system configuration which
;; simulates 1e models for all process types.
;;
;; David Samuelson
;; May 28, 2009
;;

#lang racket/base
  
  ;; find-k: (amf-filename options-list) -> () : <dumps solution to file>
  (provide find-k
           find-k-places)
  
  ;; for various utility functions
  (require "datatypes.scm")
  
  ;; provides
  ;;
  ;; parse-amf-file: (filename) -> (prot)
  ;;
  (require "parser.scm")

  ;; provides
  ;;
  ;; file->topology: (filename) -> (topo-struct)
  ;;
  ;; topology->file: (topo-struct filename) -> () : <dump topo-struct to filename>
  ;;
  (require "topo-datatypes.scm")
  
  ;; provides
  ;;
  ;; model2dot: (model filename) -> () <dump model to graphviz "dot" output>
  ;;
  (require "model-builder.scm")
  
  ;; provides
  ;;
  ;; search: (protocol topology proc-type ...) -> (values #t vector-model) | (values #f max-trace-integer)
  (require "search.scm")
(require "macros.rkt"
         "globals.rkt"
         "place-channel-param.rkt"
         racket/place)


;;; internals ;;;

;(initialized by init-internals)
;; output directory

(define (init-internals filename)
 ; sanity check on file (mostly to prevent pathlist-closure from being called on a directory)
 (when (not (file-exists? filename)) 
   (raise-user-error filename " not found!"))

 ; save the path of filename
 (define cl (path->complete-path (string->path filename)))
 (define-values (amf-dir amf-file dir?) (split-path cl))

 (unless output-directory (set!-output-directory amf-dir))
 (define prot (parse-amf-file filename))
 ; base name for output files
 (define base-name (parse-filename filename))
            
 (values prot base-name))
 


(define (parse-filename fn)
  (define len (string-length fn))
  (define first_dot (ormap
                      (lambda (x) (if (equal? (string-ref fn x) #\.) x #f))
                      (build-list len (lambda (x) (- (sub1 len) x)))))
  (if first_dot (substring fn 0 first_dot) fn))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
(define (find-k filename)
  (define bar "---------------------------\n")

  (when (>= debug 4)
      (display-ln bar "find-k call\n\tfilename: " filename "\n"))

  (define-values (prot base-name) (init-internals filename))
  ; k is current system instance
  (define k (protocol-kernel prot))

  (define eps-auts (for/filter ([z (protocol-behavior-automata prot)]) (equal? eps (automaton-in-msg z))))
  (define starts
    (cond
      [process-type
        (define names (protocol-process-names prot))
        (define index (item-index process-type names))
        (cond  
          [(< index 0) (raise-user-error "Invalid process index ~s" process-type)]
          [else 
            (display-ln "INFO: only checking " process-type " for simulation\n")
            (list index)])]
      ; otherwise, check all process indices
      [else
        (build-list (length (protocol-process-names prot)) values)]))
    
  ;; TODO: clean this up... should 
  (let ([k (for/fold ([k k])([x eps-auts])
    (define processes (protocol-process-names prot))
    (define proc-type (automaton-proc-type x))
    (define proc-mask (remove proc-type processes))
    (cond
      ; if this is a non-parameterized process type, skip
      [(member (automaton-proc-type x) npp) k]
      ; if this is an epsilon initiated process and not starting from its start, skip
      ; ((and (member (list-ref processes x) (map automaton-proc-type eps-auts)) (not (equal? (automaton-proc-type start-aut) (list-ref processes x))))
      ; (void))
      [else
        (define-values (new-k data) (find-solution k (item-index (automaton-proc-type x) processes) prot x))
        (when dump-sys
          (model2dot data (build-path output-directory (string-append (symbol->string proc-type) "-sys.dot")) npp))
        new-k]))])

    (dump-solution-to-file k base-name)))

(define (dump-solution-to-file k base-name)
  (define output-name (simplify-path (build-path output-directory (string-append (strip-folder base-name) "-cutoff.topo"))))
  (topology->file k output-name)
  (display-ln "The cut-off system has " (topology->string k) " processes.")
  (display-ln "see " output-name " for system configuration")
  (when dump-sys  
    (display-ln "and <process names>-sys.dot for simulating paths.")))

  ;; TODO: clean up find-solution and find-solution-rec
(define (find-solution k id prot start-aut)
  (define names (protocol-process-names prot))
  (define name (list-ref names id))
#|
  (pretty-print k)
  (pretty-print id)
  (pretty-print prot)
  (pretty-print start-aut)
  (pretty-print names)
|#
  (when dump-1E 
        (define-values (tt oneE-builder) (build-oneEmodel-builder prot))
        (model2dot (oneE-builder name start-aut (map (lambda (x) (item-index x names)) npp))
                   (build-path output-directory (string-append (symbol->string name) "-1e.dot")) 
                   #:show-buf #f))

  (define-values (soln data) (search 
                               prot 
                               k 
                               name
                               dfs 
                               start-aut
                               #:pruning pruning
                               #:npp npp
                               #:ring ring
                               #:dump dump
                               #:star star
                               #:start start-depth
                               #:stop stop-depth))
  (if soln  
      ; if soln == #t, then data is a model containing the simulating subset of the system
      (values k data)
      (find-solution-rec k id prot start-aut names 0 data #f 0)))


;; if we try all addition rules and none work, which rule should be applied before looping over them again
  ;
  ; TODO: clearly define an ordering for increasing processes
  ;
  ; id: index of process type we are checking
  ; names: list of process names
  ; mask : list of process names minus the pt we are checking
  ; prev_ch: index of the last applied rule
  ; cur_ch: index of the rule to apply
  ; best-trace: integer measure of how close model has come to simulating 1e
(define (find-solution-rec k id prot start-aut names cur_ch best-trace made-a-change next-id-to-inc)
  (define rules (protocol-addition-rules prot))
  (define rule (list-ref (protocol-addition-rules prot) cur_ch))
  (define size (length rules))

  ; if this rule cannot be applied to this model, skip and move on
  (cond
    [(zero? (rule 'query k))
      (when (>= debug 4) (display-ln "-------\n" "rule " rule " cannot be applied... skipping"))
      (find-solution-rec k id prot names (modulo (add1 cur_ch) size) best-trace made-a-change next-id-to-inc)]
    [else 
      (define new-k (rule 'apply k))
      (define-values (soln data) (search prot
                                         new-k
                                         (list-ref names id)
                                         dfs
                                         start-aut
                                         #:pruning pruning
                                         #:npp npp 
                                         #:ring ring
                                         #:star star
                                         #:start start-depth
                                         #:stop stop-depth))
      (cond 
        ; new configuration simulates
        [soln
          (values new-k data)]

        ; new configuration provides longer trace of oneE behavior 
        [(> data best-trace)
          (find-solution-rec new-k id prot start-aut names (modulo (+ 1 cur_ch) size) data #t next-id-to-inc)]

        ; is the the last rule before trying them all again?
        [(= 0 (modulo (+ 1 cur_ch) size))
          ;; TODO: fix this... it could fail if the current next-id-to-inc rule cannot be applied
          (cond
            [made-a-change
              (find-solution-rec k id prot start-aut names (modulo (+ 1 cur_ch) size) best-trace #f next-id-to-inc)]
            [else
              ; otherwise apply the next rule
              (define new-k ((list-ref (protocol-addition-rules prot) next-id-to-inc) 'apply k))
              (define new-next-id-to-inc (modulo (+ 1 cur_ch) size))
              (find-solution-rec new-k id prot start-aut names (modulo (+ 1 cur_ch) size) best-trace #f new-next-id-to-inc)])]

        [else
           ; otherwise just continue to the next rule
           (find-solution-rec new-k id prot start-aut names (modulo (+ 1 cur_ch) size) best-trace made-a-change next-id-to-inc)])]))


; depreciated
(define (give-up k max-k secs max-secs)
  (if (and max-k (> k max-k))
      (display-ln "Checked maximum instance size " max-k " and did not find a simulation. Quitting...")
      (display-ln "Hit time limit of " max-secs" seconds without finding a simulation. (Actual time was " secs" seconds.) Quitting..."))
  (exit))

; Remove all parts before and including the last '/'
;
; (string?) -> (string?)
(define (strip-folder x)
  (define-values (dir-path file dir?) (split-path x))
  (path->string file))

(define (find-k-places ch)
  (parameterize ([pch ch])
    (find-k (place-channel-get ch))))
