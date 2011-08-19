;;
;; datatypes.scm
;;
;; Data types and environment for model generation.
;;
;; David Samuelson
;; May 19, 2009
;;

#lang scheme

;; for docs
(require scribble/base scribble/srcdoc)

(provide 
        ; behavioral automaton
        (struct-out automaton)

        ; system processes
        (struct-out mprocess)

        ;
        (struct-out model)

        ; from list of process names and a list of process counts
        make-offset-table

        ; total count
        ot_size
        ; process and index to raw index
        ot_pi2ri
        ; raw index to process type
        ot_ri2pt
        ; process type to count
        ot_pt2pc
        ; process type to zero index
        ot_pt2zi
         
         ;; utility functions
	display-ln
         list-of-lists->list 
         los->string 
         lostrings->string
         cons-to-vec
         append-to-vec
         cons-to-hash
         symbol<?      

        state->process

         item-index
         
         ;; global variables
         eps ; keyword for no input/output msg
         tau ; keyword for 1e transitions between automata
          )
         

;;!!! GLOBAL MAGIC VARIABLES !!!

; no input transition
(define eps 'epsilon)

; fake linking message
(define tau 'tau)

;;;;;;;;;;;;;;;;;;;;;;;;; datatypes ;;;;;;;;;;;;;;;;;;;;;;;;

; state: process state
; buff ; list of msgs in output buffer 
(define-struct mprocess (state buff) #:prefab)

;
; mdl: vector representation of model
;
;    each element of mdl contains
;    (vector state-id (list-of (vector in-msg-id out-msg-id new-process-state-id next-state-index)))
; 
; table: link-table
;
(define-struct model (mdl table) #:transparent)

(define-struct automaton (proc-type name state1 in-msg state2 out-msg) #:prefab)

;; create a process wrapper around an automaton
(define (state->process state) (make-mprocess state '()))

;; 
;; similar to "flatten", but only unlist the top level lists
;;
(define list-of-lists->list
  (lambda (x)
    (if (null? x) '()
        (append (car x) (list-of-lists->list (cdr x))))))

;; transform a list of symbols into a space separated string                                                                           
(define los->string
  (lambda (ls)
    (cond
      ((null? ls) "")
      ((equal? (length ls) 1) (symbol->string (car ls)))
      (#t (string-append (symbol->string (car ls)) " " (los->string (cdr ls)))))))

;; transform a list of strings into a string separated by sep
(define lostrings->string
  (lambda (ls sep)
    (cond
      ((null? ls) "")
      ((equal? (length ls) 1) (car ls))
      (#t (string-append (car ls) sep (lostrings->string (cdr ls) sep))))))

;; display symbols on a single line
(define display-ln
  (lambda lst
    (if (null? lst)
        (newline)
        (begin (display (car lst))
               (apply display-ln (cdr lst))))))

; cons item to the value stored at x of vector
(define cons-to-vec
  (lambda (vect x item)
    (vector-set! vect x (cons item (vector-ref vect x)))))

(define append-to-vec
  (lambda (vect x item)
    (vector-set! vect x (append (vector-ref vect x) item))))

;;; offset table ;;;;;

; name-count is a list of pairs of symbols: 
;     (node-name number-of-nodes-in-model) 
(define make-offset-table
  (lambda (name-count)
    (ot-rec name-count 0)))

; helper for make-offset-table
(define ot-rec 
  (lambda (nc index)
    (if (null? nc) '()
        (let ([name (caar nc)]
              [count (cadar nc)])
        (cons (list name count index) (ot-rec (cdr nc) (+ count index)))))))

; size of offset table
(define ot_size
  (lambda (x)
    (let ([end (last x)])
      (+ (cadr end) (caddr end)))))


; process index to raw index
(define ot_pi2ri
  (lambda (proc index offset-table)
    (let ([set (assoc proc offset-table)])
      (if set (if (<= (cadr set) index) (error 'PARSE "Bad process index on " proc index)
                              (+ (caddr set) index))
          (error 'PARSE "Bad process name on " proc index)))))

; raw index to process type
(define ot_ri2pt
  (lambda (index offset-table)
      (if (null? offset-table) (error 'LOOKUP "Bad index " index)
          (let ([fr (car offset-table)])
            (if (<= (+ (cadr fr) (caddr fr)) index)
                  (ot_ri2pt index (cdr offset-table))
                  (car fr))))))

; process type to process count
(define ot_pt2pc
  (lambda (name ot)
    (cadr (assoc name ot))))

; process type to zero index
(define ot_pt2zi
  (lambda (name ot)
    (caddr (assoc name ot))))

; update a hashmap with key and append x to value of key (must be a list)
(define cons-to-hash
  (lambda (hm key x)
    (let ([current-value (hash-ref! hm key #f)])
      (cond
        ; if not in hash, add
        ((not current-value) (hash-set! hm key (list x)))
        ; if already in, ignore
        ((member x current-value) (void))
        ; otherwise, cons to current value
        (#t (hash-set! hm key (cons x current-value)))))))

;;
;; add item to set if not already a member
;;
;; (any/c list) -> (list)
;;
(define add-item-to-set
  (lambda (item set)
    (if (member item set) set
        (cons item set))))


;;
;; return the index of item in ls
;;
;; or -1 if not a member
;;
(define (item-index item ls [index 0])
  (cond
    [(null? ls) -1]
    [(equal? item (car ls)) index]
    [else (item-index item (cdr ls) (add1 index))]))


;;
;; wrapper for string<? on symbols
;;
(define (symbol<? x y)
  (string<? (symbol->string x) (symbol->string y)))

