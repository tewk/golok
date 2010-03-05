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

  ; dump depth
  (define dump #f)

;; set globals passed via command line arguments
  (define init-clo 
    (lambda (arg-list)
      (begin
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
           (set! dump (list-ref arg-list 11)))))

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

  (define init-internals
    (lambda (filename)
      ; sanity check on file (mostly to prevent pathlist-closure from being called on a directory)
      (if (not (file-exists? filename)) (raise-user-error filename " not found!")
            ; save the path of filename
         (let ([cl (pathlist-closure (list filename))])
            (begin
              (set! amf-directory (path->string (list-ref cl (- (length cl) 2))))
              (if (not output-directory) (set! output-directory amf-directory) (void)))
              (set! prot (parse-amf-file filename))
              (set! base-name (parse-filename filename))
              (set! start-time (current-seconds))
              
              ; k is current system instance
              (set! k (file->topology (string-append amf-directory (protocol-kernel prot))))))))

  (define parse-filename
    (lambda (fn)
      (let* ([len (string-length fn)]
             [first_dot (ormap
                          (lambda (x) (if (equal? (string-ref fn x) #\.) x #f))
                          (build-list len (lambda (x) (- (sub1 len) x))))])
      (if first_dot (substring fn 0 first_dot) fn))))

; helper for debug statements
(define bar "---------------------------\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
  (define find-k
    (lambda (filename arg-list)
      (begin
        (init-clo arg-list)
        (if (>= debug 4)
            (display-ln bar "find-k call\n\tfilename: " filename "\n\targ-list: " arg-list ) (void))

        (init-internals filename)

      (let*
        ([eps-auts (filter (lambda (z) (equal? eps (automaton-in-msg z))) (protocol-ba prot))]
         [starts (if process-type
              (let* ([names (protocol-process-names prot)]
                     [index (item-index process-type names)])
                  (if (< index 0) (raise-user-error "Invalid process index ~s" process-type)
                          (begin
                                (display-ln "INFO: only checking " process-type " for simulation\n")
                                (list index))))
              ; otherwise, check all process indices
              (build-list (length (protocol-process-names prot)) values))])
        
       (for-each
          (lambda (start-aut)
            (for-each 
              (lambda (x)
                (let* ([processes (protocol-process-names prot)]
                        [proc-type (list-ref processes x)] 
                      [proc-mask (remove proc-type processes)])
                    (cond
                      ; if this is an epsilon initiated process and not starting from its start, skip
                      ((and (member (list-ref processes x) (map automaton-proc-type eps-auts)) (not (equal? (automaton-proc-type start-aut) (list-ref processes x))))
                                (void))
                      (dump-sys
                          (model2dot (find-solution x start-aut)
				                    (string-append output-directory "/" (symbol->string proc-type) "-from-" 
                                          (symbol->string (automaton-proc-type start-aut)) "-sys.dot") proc-mask))
                      (#t
                        (find-solution x start-aut)))))

          ; if only checking a single process type, find its index number and return a singleton list
          starts))
          eps-auts))

        (dump-solution))))

  (define dump-solution
    (lambda ()
      (let ([output-name (string-append output-directory "/" (strip-folder base-name) "-cutoff.topo")])
        (begin
          (topology->file k output-name)
          (display-ln "The cut-off system has " (topology->string k) " processes.")
          (display-ln "see " output-name " for system configuration")
          (if dump-sys  
            (display-ln "and <process names>-sys.dot for simulating paths.")
            (void))))))

  ;; TODO: clean up find-solution and find-solution-rec
  (define find-solution
    (lambda (id start-aut)
      (let* ([names (protocol-process-names prot)]
             [name (list-ref names id)]
             [mask (remove name names)])
        (let-values ([(tt oneE-builder) (build-oneEmodel-builder prot)])
          (let ([dummy0 (if dump-1E (model2dot (oneE-builder name start-aut)
                      (string-append output-directory "/" (symbol->string name) "-from-" 
                                                    (symbol->string (automaton-proc-type start-aut)) "-1e.dot") #:show-buf #f) (void))])
        (let-values ([(soln data) (search prot k name dfs start-aut
                                                         #:ring ring
                                                         #:dump dump
                                                         #:star star
                                                         #:start start-depth
                                                         #:stop stop-depth)])
        (if soln  
          ; if soln == #t, then data is a model containing the simulating subset of the system
           data
    (find-solution-rec id start-aut names 0 data))))))))


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
(define find-solution-rec
  (lambda (id start-aut names cur_ch best-trace)
   (let* ([rules (protocol-addition-rules prot)]
          [rule (list-ref (protocol-addition-rules prot) cur_ch)]
          [size (length rules)]
          [mask (remove (list-ref names id) names)])

      ; if this rule cannot be applied to this model, skip and move on
      (if (zero? (rule 'query k))
          (begin
            (if (>= debug 4) (display-ln "-------\n" "rule " rule " cannot be applied... skipping") (void))
            (find-solution-rec id names (modulo (add1 cur_ch) size) best-trace))
          (let* ([new-k (rule 'apply k)])
            (let-values ([(soln data) (search prot new-k (list-ref names id) dfs start-aut
                                                        #:ring ring
                                                        #:star star
                                                        #:start start-depth
                                                        #:stop stop-depth)])
        (cond 
          ; new configuration simulates
          (soln (begin
                    (set! k new-k)
                    data))

          ; new configuration provides longer trace of oneE behavior 
          ((> data best-trace)
                (begin
                    (set! k new-k)
                    (set! made-a-change #t)
                    (find-solution-rec id start-aut names (modulo (+ 1 cur_ch) size) data)))

          ; is the the last rule before trying them all again?
          ((= 0 (modulo (+ 1 cur_ch) size))
           ;; TODO: fix this... it could fail if the current next-id-to-inc rule cannot be applied
           (if made-a-change
               ; clear the flag and continue
               (begin
                  (set! made-a-change #f)
                  (find-solution-rec id start-aut names (modulo (+ 1 cur_ch) size) best-trace))
               ; otherwise apply the next rule
               (begin
                  (set! made-a-change #f)
                  (set! k ((list-ref (protocol-addition-rules prot) next-id-to-inc) 'apply k))
                  (set! next-id-to-inc (modulo (+ 1 cur_ch) size))
                  (find-solution-rec id start-aut names (modulo (+ 1 cur_ch) size) best-trace))))
            

          ; otherwise just continue to the next rule
          (#t (find-solution-rec id start-aut names (modulo (+ 1 cur_ch) size) best-trace)))))))))


; depreciated
  (define give-up
    (lambda (k max-k secs max-secs)
      (begin
        (if (if max-k (> k max-k) #f)
            (display-ln "Checked maximum instance size " max-k " and did not find a simulation. Quitting...")
            (display-ln "Hit time limit of " max-secs" seconds without finding a simulation. (Actual time was " secs" seconds.) Quitting..."))
        (exit))))

; Remove all parts before and including the last '/'
;
; (string?) -> (string?)
(define strip-folder
  (lambda (x)
    (let ([rev-chars (reverse (string->list x))])
      (list->string (reverse (strip-folder-rec rev-chars))))))

; (list-of char?) -> (list-of char?)
(define strip-folder-rec
  (lambda (x)
     (cond
        ((null? x) '())
        ((or (eq? (car x) #\/) (eq? (car x) #\\)) '())
        (#t (cons (car x) (strip-folder-rec (cdr x)))))))
