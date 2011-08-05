;;
;; find-k.scm
;;
;; Top level algorithm for finding the system configuration which
;; simulates 1e models for all process types.
;;
;; David Samuelson
;; May 28, 2009
;;

#lang scheme
  
  ;; find-k: (amf-filename options-list) -> () : <dumps solution to file>
  (provide find-k)
  
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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; gobal variables ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; command line options ;;;

;(initialized by init-clo)

; info level
(define debug 0)

; model size to halt after
(define max-secs #f)

;; write out all 1E models
(define dump-1E #f)
(define dump-sys #f)
(define dfs #f)
(define ring #f)
(define star (list))
(define start-depth #f)
(define process-type #f)
(define stop-depth #f)
(define pruning #t)

; dump depth
(define dump #f)

; list of non-paramterized process names
(define npp (list))

;; TODO: figure out if there is any better way for 
;;        passing arguments across modules
;;        (while avoiding circular "requires")
;;        because this is ugly as sin
;;
;; set globals passed via command line arguments
(define (init-clo arg-list)
  (set! debug (car arg-list))
  (set! max-secs (cadr arg-list))
  (set! dump-1E (caddr arg-list))
  (set! dump-sys (cadddr arg-list))
  (set! output-directory (list-ref arg-list 4))
  (set! dfs (list-ref arg-list 5))
  (set! ring (list-ref arg-list 6))
  (set! start-depth (list-ref arg-list 7))
  (set! stop-depth (list-ref arg-list 8))
  (set! process-type (list-ref arg-list 9))
  (set! star (list-ref arg-list 10))
  (set! dump (list-ref arg-list 11))
  (set! npp (list-ref arg-list 12))
  (set! pruning (list-ref arg-list 13)))

;;; internals ;;;

;(initialized by init-internals)
  
; protocol
(define prot #f)

; base name for output files
(define base-name #f)

; current topology-struct
(define k #f)

;; time at find-k call
(define start-time #f)

;; output directory
(define output-directory #f)

(define amf-directory #f)

(define (init-internals filename)
 ; sanity check on file (mostly to prevent pathlist-closure from being called on a directory)
 (when (not (file-exists? filename)) 
   (raise-user-error filename " not found!"))

 ; save the path of filename
 (define cl (path->complete-path (string->path filename)))
 (define-values (amf-dir amf-file dir?) (split-path cl))

 (set! amf-directory amf-dir)
 (unless output-directory (set! output-directory amf-directory))
 (set! prot (parse-amf-file filename))
 (set! base-name (parse-filename filename))
 (set! start-time (current-seconds))
            
 ; k is current system instance
 (set! k (protocol-kernel prot)))
 


(define (parse-filename fn)
  (define len (string-length fn))
  (define first_dot (ormap
                      (lambda (x) (if (equal? (string-ref fn x) #\.) x #f))
                      (build-list len (lambda (x) (- (sub1 len) x)))))
  (if first_dot (substring fn 0 first_dot) fn))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
(define (find-k filename arg-list)
  (define bar "---------------------------\n")

  (init-clo arg-list)
  (when (>= debug 4)
      (display-ln bar "find-k call\n\tfilename: " filename "\n\targ-list: " arg-list))

  (init-internals filename)

  (define eps-auts (filter (lambda (z) (equal? eps (automaton-in-msg z))) (protocol-ba prot)))
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
  (for ([x eps-auts])
    (define processes (protocol-process-names prot))
    (define proc-type (automaton-proc-type x))
    (define proc-mask (remove proc-type processes))
    (cond
      ; if this is a non-parameterized process type, skip
      [(member (automaton-proc-type x) npp) (void)]
      ; if this is an epsilon initiated process and not starting from its start, skip
      ; ((and (member (list-ref processes x) (map automaton-proc-type eps-auts)) (not (equal? (automaton-proc-type start-aut) (list-ref processes x))))
      ; (void))
      [dump-sys
        (model2dot (find-solution (item-index (automaton-proc-type x) processes) x)
                   (build-path output-directory (string-append (symbol->string proc-type) "-sys.dot")) npp)]
      [else
        (find-solution (item-index (automaton-proc-type x) processes) x)]))

  (dump-solution))

(define (dump-solution)
  (define output-name (simplify-path (build-path output-directory (string-append (strip-folder base-name) "-cutoff.topo"))))
  (topology->file k output-name)
  (display-ln "The cut-off system has " (topology->string k) " processes.")
  (display-ln "see " output-name " for system configuration")
  (when dump-sys  
    (display-ln "and <process names>-sys.dot for simulating paths.")))

  ;; TODO: clean up find-solution and find-solution-rec
(define (find-solution id start-aut)
  (define names (protocol-process-names prot))
  (define name (list-ref names id))
  (define mask (remove name names))
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
      data
      (find-solution-rec id start-aut names 0 data)))


;; if we try all addition rules and none work, which rule should be applied before looping over them again
(define next-id-to-inc 0)

;; did we increment the model in this loop?
(define made-a-change #f)

  ;
  ; TODO: clearly define an ordering for increasing processes
  ;
  ; id: index of process type we are checking
  ; names: list of process names
  ; mask : list of process names minus the pt we are checking
  ; prev_ch: index of the last applied rule
  ; cur_ch: index of the rule to apply
  ; best-trace: integer measure of how close model has come to simulating 1e
(define (find-solution-rec id start-aut names cur_ch best-trace)
  (define rules (protocol-addition-rules prot))
  (define rule (list-ref (protocol-addition-rules prot) cur_ch))
  (define size (length rules))
  (define mask (remove (list-ref names id) names))

  ; if this rule cannot be applied to this model, skip and move on
  (cond
    [(zero? (rule 'query k))
      (when (>= debug 4) (display-ln "-------\n" "rule " rule " cannot be applied... skipping"))
      (find-solution-rec id names (modulo (add1 cur_ch) size) best-trace)]
    [else 
      (define new-k (rule 'apply k))
      (define-values (soln data) (search prot new-k (list-ref names id) dfs start-aut
                                                        #:pruning pruning
                                                        #:npp npp 
                                                        #:ring ring
                                                        #:star star
                                                        #:start start-depth
                                                        #:stop stop-depth))
      (cond 
        ; new configuration simulates
        [soln
          (set! k new-k)
          data]

        ; new configuration provides longer trace of oneE behavior 
        [(> data best-trace)
          (set! k new-k)
          (set! made-a-change #t)
          (find-solution-rec id start-aut names (modulo (+ 1 cur_ch) size) data)]

        ; is the the last rule before trying them all again?
        [(= 0 (modulo (+ 1 cur_ch) size))
          ;; TODO: fix this... it could fail if the current next-id-to-inc rule cannot be applied
          (cond
            [made-a-change
              ; clear the flag and continue
              (set! made-a-change #f)
              (find-solution-rec id start-aut names (modulo (+ 1 cur_ch) size) best-trace)]
            [else
              ; otherwise apply the next rule
              (set! made-a-change #f)
              (set! k ((list-ref (protocol-addition-rules prot) next-id-to-inc) 'apply k))
              (set! next-id-to-inc (modulo (+ 1 cur_ch) size))
              (find-solution-rec id start-aut names (modulo (+ 1 cur_ch) size) best-trace)])]

        [else
           ; otherwise just continue to the next rule
           (find-solution-rec id start-aut names (modulo (+ 1 cur_ch) size) best-trace)])]))


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
