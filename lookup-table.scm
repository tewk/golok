;;
;; lookup-table.scm 
;;
;;  A lookup-table struct is a container for all mappings
;;  from symbols (automaton names, states, and messages)
;;  to their internal representation. 
;;
;;  It is also used to generate the "next state" hash-table
;;  used in model construction.
;;
;; David Samuelson
;; May 20, 2009
;;

#lang racket/base
  
(provide

  ;; (list-of automaton?) -> (lookup-table?)
  create-lookup-table

  ;; (lookup-table?) -> () : dump terse description to display
  debug-lookup-table

  ;; return a list of possible produced state, msg pairs
  ;; (state msg) -> (list-of (state msg))
  get-trans

  topo-description&initial-auts->topo-hash&start

  ;; ========= representation conversions ======= ;;
  state->process 
  
  ;; all id types are integers,
  ;; all others are symbols
  state-id->proc-id

  ; proc is process type (as in left-diner or right-diner in lr-dpp)
  proc->proc-id
  proc-id->proc

  state->state-id
  state-id->state

  msg->msg-id
  msg-id->msg

  ;; return all msg-ids
  ;; (lookup-table?) -> (list-of integer)
  lookup-table->all-msgs)


; for automaton datatype (and various utilities)
;  and mprocess
(require "datatypes.scm")

;;
;; magic internal number
;;
;; (process-state / state-mod) = process-type 
(define state-mod 128)

; automata: vector of automata
; refs: corresponding vector of next nodes (by index)
(define-struct lookup-table (trans  
                             state->state-id 
                             state-id->state
                             msg->msg-id
                             msg-id->msg
                             proc->proc-id 
                             proc-id->proc) #:transparent) 

(define (debug-lookup-table z)
  (define (displayit x y) (display-ln x " \t\t<--> " y))
  (display-ln "state->state-id map")
  (display-ln "-------------------")
  (hash-for-each (lookup-table-state->state-id z) displayit)
  (display-ln "\nstate->msg-id map")
  (display-ln "-------------------")
  (hash-for-each (lookup-table-msg->msg-id z) displayit)
  (display-ln "\ntrans map")
  (display-ln "-------------------")
  (hash-for-each (lookup-table-trans z) displayit))

;;
;; topology is what is returned from instantiate-protocol
;;
;; ((list-of automaton)) -> (lookup-table)
;;
(define (create-lookup-table automata [for-1e? #f])
  (if for-1e?
      (create-1e-lookup-table automata)
      (create-system-lookup-table automata)))

(define-syntax-rule (make-lookup-hash lst func)
  (begin 
    (define table (make-hash))
    (for ([x lst]
          [i (in-naturals)])
      (hash-set! table (func x) i))
    table))
  

;;
;; create a lookup table (linking for 1e model)
;;
;; ((list-of automaton)) -> (lookup-table)
;; automaton: List-of (prot-type name state1 in-msg state2 out-msg)
;;
(define (create-1e-lookup-table automata)
  ;; Assign each automaton to an integer.
  (define (get-simple-auts auts) (make-lookup-hash auts automaton-name))

  ;;
  ;; create a hashmap with keys:
  ;;   (vector symbol? symbol? boolean?)
  ;;   where the first symbol is process name, second is automaton name, and third is got message?
  ;;                                                                                    (equivalently, state 1 or 2)
  ;;values are:
  ;;  integer?
  ;;  (a unique id for each automaton+state)
  ;; 
  ;;
  (define (get-auts-map automata proc-map simple-auts-map)
    (define table (make-hash))
    (for ([x automata])
      (define proc-type (automaton-proc-type x))
      (define name (automaton-name x))
      (define proc-id (hash-ref proc-map proc-type))
      (define aut-id (hash-ref simple-auts-map name))
      (add-to! table 
               (vector proc-type name #f) 
               (+ (* state-mod proc-id) (* 2 aut-id)))
      (add-to! table 
               (vector proc-type name #t) 
               (+ (* state-mod proc-id) (add1 (* 2 aut-id)))))
    table)
                    
  (define proc-map (get-processes automata))
  (define auts-map (get-auts-map automata proc-map (get-simple-auts automata)))
  (define msgs-map (get-msgs automata))
  (define links (create-1e-linking automata auts-map msgs-map))
  (make-lookup-table links 
                     auts-map 
                     (reverse-map auts-map) 
                     msgs-map 
                     (reverse-map msgs-map)
                     proc-map
                     (reverse-map proc-map)))
;;
;; create a lookup table (linking for system states)
;;
;; ((list-of automaton)) -> (lookup-table)
;;
(define (create-system-lookup-table automata)
  (define (get-simple-states auts)
    (define table (make-hash))
    (for ([x auts])
      (add-to! table (automaton-state1 x))
      (add-to! table (automaton-state2 x)))
    table)

  (define proc-map (get-processes automata))
  (define msgs-map (get-msgs automata))
  ; create a mapping from process-name and state to create a unique state ids
  (define states-map (get-states-map automata proc-map (get-simple-states automata)))
  (define links (create-sys-linking automata states-map msgs-map))
  (make-lookup-table links 
                     states-map 
                     (reverse-map states-map) 
                     msgs-map 
                     (reverse-map msgs-map) 
                     proc-map 
                     (reverse-map proc-map)))

;; returns a hash-map with
;;
;; keys: (vector process-type state)
;; values: unique integers with the property that (quotient state-id state-mod) = proc-id
(define (get-states-map automata proc-map simple-states-map)
  (define table (make-hash))
  (for ([x automata])
    (define proc-type (automaton-proc-type x))
    (define proc-id (hash-ref proc-map proc-type))
    (define (add-to-map! field-selector)
      (define state (field-selector x))
      (define state-id (hash-ref simple-states-map state))
      (add-to! table (vector proc-type state) (+ (* state-mod proc-id) state-id)))

    (add-to-map! automaton-state1)
    (add-to-map! automaton-state2))
  table)


;;
;; returns the inverse map of passed hash-map (this assumes the map is bijective)
;; 
;; (hash-map) -> (hash-map)
;;
(define (reverse-map hm)
  (define hm2 (make-hash))
  (hash-for-each hm (lambda (x y) (hash-set! hm2 y x)))
  hm2)

(define (create-sys-linking auts states-map msgs-map)
  (define store (make-hash))
  (for ([x auts])
    (let ([s1 (hash-ref states-map (vector (automaton-proc-type x) (automaton-state1 x)))]
          [s2 (hash-ref states-map (vector (automaton-proc-type x) (automaton-state2 x)))]
          [in (hash-ref msgs-map (automaton-in-msg x))]
          [out (hash-ref msgs-map (automaton-out-msg x))])
      (cons-to-hash store (vector s1 in) (vector s2 out))))
  store)

(define (create-1e-linking auts auts-map msgs-map)
  (define store (make-hash))
  (define tau-id (hash-ref msgs-map tau))
  (for ([x auts])
    (let ([s1 (hash-ref auts-map (vector (automaton-proc-type x) (automaton-name x) #f))]
          [s2 (hash-ref auts-map (vector (automaton-proc-type x) (automaton-name x) #t))]
          [in (hash-ref msgs-map (automaton-in-msg x))]
          [out (hash-ref msgs-map (automaton-out-msg x))])
      ;; add the labeled transition between the two states of the automaton
      (cons-to-hash store (vector s1 in) (vector s2 out))
      ;; add all the tau links
      (for ([y auts])
        (let ([os1 (hash-ref auts-map (vector (automaton-proc-type y) (automaton-name y) #f))]
              [oim (hash-ref msgs-map (automaton-in-msg y))])
          ;; if this automaton (x)'s output message equals the others (y)'s
          ;; input message, add a tau link
          (when (= out oim)
            (cons-to-hash store (vector s2 tau-id) (vector os1 tau-id)))))))
  store)
                              

(define (get-msgs auts)
  (define table (make-hash))
  ; add a dummy message for taus with fixed msg-id
  (add-to! table tau -2)
  (for ([x auts])
    (add-to! table (automaton-in-msg x))
    (add-to! table (automaton-out-msg x)))
  table)
                  
;;
;; Add an integer label to all process types.
;;
;; ((list-of automaton?) -> (hash-map?)
;;
(define (get-processes auts)
  (define table (make-hash))
  (for ([x auts]) (add-to! table (automaton-proc-type x)))
  table)
                      

(define (state->proc-type x tt)
  (vector-ref (hash-ref (lookup-table-state-id->state tt) x) 0))

;; add state to hash-table (and set its value to value)
(define (add-to! ht state [value #f])
  (when (not (hash-has-key? ht state))
    (define size (hash-count ht))
    (hash-set! ht state (if value value size))))

(define (state->state-id s tt) (hash-ref (lookup-table-state->state-id tt) s)) 
(define (state-id->state s tt) (hash-ref (lookup-table-state-id->state tt) s))
(define (msg->msg-id s tt)     (hash-ref (lookup-table-msg->msg-id tt) s))
(define (msg-id->msg s tt)     (hash-ref (lookup-table-msg-id->msg tt) s))
(define (proc->proc-id s tt)   (hash-ref (lookup-table-proc->proc-id tt) s))
(define (proc-id->proc s tt)   (hash-ref (lookup-table-proc-id->proc tt) s))

(define (state-id->proc-id s) (quotient s state-mod))

; TODO: clean this up
;       This is the nasty interface between
;       symbolic generation of system instances
;       and the representations in the lookup table
(define (topo-description&initial-auts->topo-hash&start raw-topo initial-auts tt)
  ; map all initial-automatons to state-id
  (define initial-system-state 
    (for/list ([y initial-auts]) 
              (state->process (state->state-id (vector (automaton-proc-type y) (automaton-state1 y)) tt))))

  ; first convert all messages to message ids
  ;
  ; msg-to-id-pairs is a list of lists, where each element is
  ;
  ; (msg-id to-index)
  ;
  ; which is read as: the process in this index of the list can 
  ; send msg-id to the process at to-index
;;;  (define msg-to-id-pairs (map (lambda (x) (map (lambda (y) (list (msg->msg-id (car y) tt) (cadr y)))
;;;                                                   x))
;;;                                  (vector->list raw-topo)))
  (define msg-to-id-pairs 
    (for/list ([x (in-vector raw-topo)])
      (for/list ([y x])
        (list (msg->msg-id (car y) tt) (cadr y)))))

  (define msg-from-id-pairs (reverse-refs msg-to-id-pairs))
  (define ht (make-hash))

  (for ([msg-pair-list msg-from-id-pairs]
        [recv-index (in-naturals)])
    ; always include an epsilon message from the invisible sender
    (for ([msg-pair (cons (list (msg->msg-id eps tt) -1) msg-pair-list)])
      (add-msg-pair msg-pair recv-index (state-id->proc-id (mprocess-state (list-ref initial-system-state recv-index))) ht tt)))
  (values ht initial-system-state))

;change from at every index: (msg send-to-id)*
; to at every index: (msg from-id)*
(define (reverse-refs refs)
  (define nw (make-vector (length refs) (list)))
  (for ([ls  refs]
        [index (in-naturals)])
    (for ([pair (list-ref refs index)])
      (cons-to-vec nw (cadr pair) (list (car pair) index))))
  (vector->list nw))
      
    
(define (add-msg-pair msg-pair recv-index recv-proc-id ht tt)
          ; msg-pair is list (msg-id send-index)
  (for* ([recv-process-state (process-id->all-state-ids recv-proc-id tt)]
         [out-trans (get-trans recv-process-state (car msg-pair) tt)])
    (cons-to-hash ht (vector recv-index recv-process-state (car msg-pair) (cadr msg-pair)) out-trans)))

(define (process-id->all-state-ids proc-id tt)
    (filter (lambda (x) (= proc-id (state-id->proc-id x))) (lookup-table->all-states tt)))

;; return a list of all messages generated by all automata
(define (lookup-table->all-msgs tt)   (hash-map (lookup-table-msg->msg-id tt)     (lambda (x y) y)))
(define (lookup-table->all-states tt) (hash-map (lookup-table-state->state-id tt) (lambda (x y) y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add an item to a list if it is not already in it
;;;
(define (get-trans state msg tt)
    (hash-ref! (lookup-table-trans tt) (vector state msg)  (list)))
