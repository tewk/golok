#lang scheme
;
; front-end for calling find-k from the command line
; David Samuelson
;
(require "globals.rkt"
         "find-k.scm")

; command line parser which sets up environment
(define amf-file
  (command-line
   #:program (string-append "golok" "-" version)

   #:multi
   [("--star-point") x "partial order reduction for star" (set!-star (cons (string->symbol x) star))]
   [("--npp" "--non-parameterized-process") x "do not check process type for simulation" (set!-npp (cons (string->symbol x) npp))]

   #:once-each
   [("-t" "--max-time") num "Specify a maximum execution time in seconds before stopping"
                        (let ([x (string->number num)])	
                         (if (< 0 x)
                             (set!-max-t x) (raise-user-error "maximum execution time must
be a positive number, given" num)))]
   [("-d" "--debug") num "set debugging level (default 0), up to 5" (let ([x (string->number num)])	
                         (if (or (<= 0 x) (<= x 5))
                             (set!-debug x) (raise-user-error "debug level must be between 0 and 5 inclusive, given " num)))]

   [("--1e") "dump 1e models as dot files" (set!-dump-1E #t)]
   [("--sys") "dump simulating subsystem models as dot files" (set!-dump-sys #t)]
   [("-o" "--output-dir") dir "Set output directory (default is input directory)." (if (directory-exists? dir) (set!-output-directory dir) (raise-user-error "The specified output directory does not exist!"))]
  [("--dfs") "Use depth first search for enumerating system states (BFS is default)" (set!-dfs #t)]
  [("--dump") x "Dump the model up to passed depth if no solution found" (set!-dump (string->number x))]
  [("--ring") "assume system states are equivalent under rotation" (set!-ring #t)]
  [("-f" "--start-depth") x "Depth to begin searching for simulation" (set!-start-depth (string->number x))]
  [("-s" "--stop-depth") x "Depth to halt simulation search" (set!-stop-depth (string->number x))]
  [("-p" "--process-type") x "Search only for a specific process" (set!-process-type (string->symbol x))]
  [("--disable-pruning") "for testing only" (set!-pruning #f)]

   #:args (amf-file)  amf-file ))

(find-k amf-file)
