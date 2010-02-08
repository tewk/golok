;
; front-end for calling find-k from the command line
; David Samuelson
;
#lang scheme

  (require "find-k.scm")
  (require scheme/cmdline)
  
  ;; version header -- should be in form vx.y.z(-tag)
  (define version "v0.1.3.0")
  
  ; parsed values of command line arguments
  ; currently size 11

  ; information level
  (define debug 0)

  ; number of seconds to halt after (only checked between building models)
  (define max-t #f)

  ; write out oneE models
  (define dump-1E #t)

  ; when searching through system state space 
  ; for first simulating state, use dfs 
  ; instead of bfs
  (define dfs #f)

  ; ring partial order reduction
  (define ring #f)

  ; start partial order reduction
  (define star (list))

  ; just dump the model at a particular depth
  (define dump #f)

  ; output directory -- if not defined by command-line, defaults to amf directory
  (define output-directory #f)

  ; fast forward (skip) searching n levels of bfs tree
  ; TODO: not implemented
  (define start-depth #f)

  ; stop at depth
  ; TODO: not implemented
  (define stop-depth #f)

  ; index of process type to check
  (define process-type #f)
  
  ; command line parser which sets up environment
  (define amf-file
    (command-line
     #:program (string-append "golok" "-" version)

     #:multi
     [("--star-point") x "partial order reduction for star" (cons (string->symbol x) star)]
  
     #:once-each
     [("-t" "--max-time") num "Specify a maximum execution time in seconds before stopping"
                          (let ([x (string->number num)])	
                           (if (< 0 x)
                               (set! max-t x) (raise-user-error "maximum execution time must
be a positive number, given" num)))]
     [("-d" "--debug") num "set debugging level (default 0), up to 5" (let ([x (string->number num)])	
                           (if (or (<= 0 x) (<= x 5))
                               (set! debug x) (raise-user-error "debug level must be between 0 and 5 inclusive, given " num)))]

     [("--no-1e") "suppress output of 1e models" (set! dump-1E #f)]
     [("-o" "--output-dir") dir "Set output directory (default is input directory)." (if (directory-exists? dir) (set! output-directory dir) (raise-user-error "The specified output directory does not exist!"))]
    [("--dfs") "Use depth first search for enumerating system states (BFS is default)" (set! dfs #t)]
    [("--dump") x "Dump the model up to passed depth if no solution found" (set! dump (string->number x))]
    [("--ring") "assume system states are equivalent under rotation" (set! ring #t)]
    [("-f" "--start-depth") x "Depth to begin searching for simulation" (set! start-depth (string->number x))]
    [("-s" "--stop-depth") x "Depth to halt simulation search" (set! stop-depth (string->number x))]
    [("-p" "--process-type") x "Search only for a specific process" (set! process-type (string->number x))]

     #:args (amf-file)  amf-file ))
 
(find-k amf-file (list debug max-t dump-1E output-directory dfs ring start-depth stop-depth process-type star dump))
