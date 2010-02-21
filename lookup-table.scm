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

#lang scheme
  
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
                                   state->state-id state-id->state
                                   msg->msg-id  msg-id->msg
                                   proc->proc-id proc-id->proc) #:transparent) 

(define debug-lookup-table
  (lambda (z)
    (begin
      (display-ln "state->state-id map")
      (display-ln "-------------------")
      (hash-for-each (lookup-table-state->state-id z) (lambda (x y) (display-ln x " \t\t<--> " y)))
      (display-ln "\nstate->msg-id map")
      (display-ln "-------------------")
      (hash-for-each (lookup-table-msg->msg-id z) (lambda (x y) (display-ln x " \t\t<--> " y)))
      (display-ln "\ntrans map")
      (display-ln "-------------------")
      (hash-for-each (lookup-table-trans z) (lambda (x y) (display-ln x " \t\t<--> " y))))))

;;
;; topology is what is returned from instantiate-protocol
;;
;; ((list-of automaton)) -> (lookup-table)
;;
(define create-lookup-table
  (lambda (automata [for-1e? #f])
    (if for-1e? (create-1e-lookup-table automata)
                (create-system-lookup-table automata))))

;;
;; create a lookup table (linking for 1e model)
;;
;; ((list-of automaton)) -> (lookup-table)
;;
(define create-1e-lookup-table
  (lambda (automata)
    (let* ([proc-map (get-processes automata)]
           [auts-map (get-auts-map automata proc-map (get-simple-auts automata))]
           [msgs-map (get-msgs automata)]
           [links (create-1e-linking automata auts-map msgs-map)])
      (make-lookup-table links auts-map (reverse-map auts-map) msgs-map (reverse-map msgs-map)
                                                    proc-map (reverse-map proc-map)))))
;;
;; create a lookup table (linking for system states)
;;
;; ((list-of automaton)) -> (lookup-table)
;;
(define create-system-lookup-table
  (lambda (automata)
    (let* ([proc-map (get-processes automata)]
            ; create a mapping from process-name and state to create a unique state ids
           [states-map (get-states-map automata proc-map (get-simple-states automata))]
           [msgs-map (get-msgs automata)]
           [links (create-sys-linking automata states-map msgs-map)])
      (make-lookup-table links states-map (reverse-map states-map) msgs-map (reverse-map msgs-map) proc-map (reverse-map proc-map)))))

;; returns a hash-map with
;;
;; keys: (vector process-type state)
;; values: unique integers with the property that (quotient state-id state-mod) = proc-id
(define (get-states-map automata proc-map simple-states-map)
  (let ([table (make-hash)])
    (begin
      (for-each (lambda (x)
                  (let ([proc-id (hash-ref proc-map (automaton-proc-type x))]
                        [state1-id (hash-ref simple-states-map (automaton-state1 x))]
                        [state2-id (hash-ref simple-states-map (automaton-state2 x))])
                    (begin
                      (add-to! table (vector (automaton-proc-type x) (automaton-state1 x))
                                              (+ (* state-mod proc-id) state1-id))
                      (add-to! table (vector (automaton-proc-type x) (automaton-state2 x))
                                              (+ (* state-mod proc-id) state2-id)))))
               automata)
      table)))

;;
;; create a hashmap with keys:
;;                        (vector symbol? symbol? boolean?)
;;                    where the first symbol is process name, second is automaton name, and third is got message?
;;                                                                                    (equivalently, state 1 or 2)
;;
;;                    values are:
;;                          integer?
;;                      (a unique id for each automaton+state)
;; 
;;
(define (get-auts-map automata proc-map simple-auts-map)
  (let ([table (make-hash)])
    (begin
      (for-each (lambda (x)
                  (let ([proc-id (hash-ref proc-map (automaton-proc-type x))]
                        [aut-id (hash-ref simple-auts-map (automaton-name x))])
                    (begin
                      (add-to! table (vector (automaton-proc-type x) (automaton-name x) #f)
                                              (+ (* state-mod proc-id) (* 2 aut-id)))
                      (add-to! table (vector (automaton-proc-type x) (automaton-name x) #t)
                                              (+ (* state-mod proc-id) (add1 (* 2 aut-id)))))))
                automata)
      table)))
                  

;;
;; returns the inverse map of passed hash-map (this assumes the map is bijective)
;; 
;; (hash-map) -> (hash-map)
;;
(define reverse-map
  (lambda (hm)
    (let ([hm2 (make-hash)])
      (begin
        (hash-for-each hm (lambda (x y) (hash-set! hm2 y x)))
        hm2))))

(define create-sys-linking
   (lambda (auts states-map msgs-map)
     (let* ([store (make-hash)])
        (begin
          (for-each (lambda (x)
                       (let ([s1 (hash-ref states-map (vector (automaton-proc-type x) (automaton-state1 x)))]
                             [s2 (hash-ref states-map (vector (automaton-proc-type x) (automaton-state2 x)))]
                             [in (hash-ref msgs-map (automaton-in-msg x))]
                             [out (hash-ref msgs-map (automaton-out-msg x))])
                        (cons-to-hash store (vector s1 in) (vector s2 out))))
                    auts)
           store))))

(define create-1e-linking
  (lambda (auts auts-map msgs-map)
    (let* ([store (make-hash)]
           [tau-id (hash-ref msgs-map tau)])
      (begin
          (for-each (lambda (x)
                       (let ([s1 (hash-ref auts-map (vector (automaton-proc-type x) (automaton-name x) #f))]
                             [s2 (hash-ref auts-map (vector (automaton-proc-type x) (automaton-name x) #t))]
                             [in (hash-ref msgs-map (automaton-in-msg x))]
                             [out (hash-ref msgs-map (automaton-out-msg x))])
                         (begin
                            ;; add the labeled transition between the two states of the automaton
                            (cons-to-hash store (vector s1 in) (vector s2 out))
                            ;; all all the tau links
                            (for-each (lambda (y)
                                (let ([os1 (hash-ref auts-map (vector (automaton-proc-type y)
                                                                      (automaton-name y) #f))]
                                      [oim (hash-ref msgs-map (automaton-in-msg y))])
                                    ;; if this automaton (x)'s output message equals the others (y)'s
                                    ;; input message, add a tau link
                                  (if (= out oim)
                                      (cons-to-hash store (vector s2 tau-id) (vector os1 tau-id))
                                      ; otherwise, ignore
                                      (void))))
                                      auts))))
                    auts)
          store))))
         
              
                              

(define get-msgs
  (lambda (auts)
    (let ([table (make-hash)])
      (begin
        ; add a dummy message for taus with fixed msg-id
        (add-to! table tau -2)
        (for-each (lambda (x)
                  (begin
                    (add-to! table (automaton-in-msg x))
                    (add-to! table (automaton-out-msg x))))
              auts)
        table))))
                  
(define get-simple-states
  (lambda (auts)
    (let ([table (make-hash)])
      (begin
        (for-each (lambda (x)
                      (begin
                        (add-to! table (automaton-state1 x))
                        (add-to! table (automaton-state2 x))))
                 auts)
        table))))

;;
;; Assign each automaton to an integer.
;;
(define get-simple-auts
  (lambda (auts)
    (let ([table (make-hash)])
      (begin
        (for-each (lambda (x)
                        (add-to! table (automaton-name x)))
                 auts)
        table))))
;;
;; Add an integer label to all process types.
;;
;; ((list-of automaton?) -> (hash-map?)
;;
(define get-processes
  (lambda (auts)
    (let ([table (make-hash)])
      (begin
        (for-each (lambda (x)
                      (add-to! table (automaton-proc-type x)))
                  auts)
        table))))
                      

(define state->proc-type
  (lambda (x tt)
    (let ([aut (hash-ref (lookup-table-state-id->state tt) x)])
      (vector-ref aut 0))))

;; add state to hash-table (and set its value to value)
(define add-to!
  (lambda (ht state [value #f])
    (let ([size (hash-count ht)])
      (if (not (hash-has-key? ht state))
            (hash-set! ht state (if value value size)) (void)))))

(define state->state-id
  (lambda (s tt)
    (hash-ref (lookup-table-state->state-id tt) s)))

(define msg->msg-id
  (lambda (s tt)
    (hash-ref (lookup-table-msg->msg-id tt) s)))

(define state-id->state
  (lambda (s tt)
    (hash-ref (lookup-table-state-id->state tt) s)))

(define msg-id->msg
  (lambda (s tt)
    (hash-ref (lookup-table-msg-id->msg tt) s)))

(define proc->proc-id
  (lambda (s tt)
    (hash-ref (lookup-table-proc->proc-id tt) s)))

(define proc-id->proc
  (lambda (s tt)
    (hash-ref (lookup-table-proc-id->proc tt) s)))

(define state-id->proc-id
  (lambda (s)
    (quotient s state-mod)))

; TODO: clean this up
;       This is the nasty interface between
;       symbolic generation of system instances
;       and the representations in the lookup table
(define topo-description&initial-auts->topo-hash&start
  (lambda (raw-topo initial-auts tt)
          ; map all initial-automatons to state-id
    (let* ([initial-system-state (map (lambda (x) (state->process x)) 
                                        (map (lambda (y) (state->state-id
                                                (vector (automaton-proc-type y)
                                                        (automaton-state1 y)) tt))
                                        initial-auts))]
          ; first convert all messages to message ids
          ;
          ; msg-to-id-pairs is a list of lists, where each element is
          ;
          ; (msg-id to-index)
          ;
          ; which is read as: the process in this index of the list can 
          ; send msg-id to the process at to-index
          [msg-to-id-pairs (map (lambda (x) 
                                            (map (lambda (y) (list (msg->msg-id (car y) tt) (cadr y)))
                                                   x))
                                  (vector->list raw-topo))]
          [msg-from-id-pairs (reverse-refs msg-to-id-pairs)]
          [ht (make-hash)])
      (begin
        (for-each (lambda (msg-pair-list recv-index)
                     (for-each (lambda (msg-pair)
                                    (add-msg-pair msg-pair recv-index (state-id->proc-id 
                (mprocess-state (list-ref initial-system-state recv-index))) ht tt))
                                  ; always include an epsilon message from the invisible sender
                                (cons (list (msg->msg-id eps tt) -1) msg-pair-list)))
                  msg-from-id-pairs
                  (build-list (length msg-from-id-pairs) values))
        (values ht initial-system-state)))))

;change from at every index: (msg send-to-id)*
; to at every index: (msg from-id)*
(define reverse-refs
  (lambda (refs)
    (let ([nw (make-vector (length refs) (list))])
      (begin
        (for-each (lambda (ls index)
                        (for-each (lambda (pair)
                                  (cons-to-vec nw (cadr pair) (list (car pair) index)))
                        (list-ref refs index)))
                 refs
                (build-list (length refs) values))
        (vector->list nw)))))
      
    
(define add-msg-pair
          ; msg-pair is list (msg-id send-index)
  (lambda (msg-pair recv-index recv-proc-id ht tt)
    (for-each
      (lambda (recv-process-state)
        (for-each
          (lambda (out-trans)
            (cons-to-hash ht (vector recv-index recv-process-state (car msg-pair) (cadr msg-pair)) out-trans))
          (get-trans recv-process-state (car msg-pair) tt)))
       (process-id->all-state-ids recv-proc-id tt))))

(define process-id->all-state-ids
  (lambda (proc-id tt)
    (filter (lambda (x) (= proc-id (state-id->proc-id x))) (lookup-table->all-states tt))))

;; return a list of all messages generated by all automata
(define lookup-table->all-msgs
  (lambda (tt)
    (hash-map (lookup-table-msg->msg-id tt) (lambda (x y) y))))

(define lookup-table->all-states
  (lambda (tt)
    (hash-map (lookup-table-state->state-id tt) (lambda (x y) y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add an item to a list if it is not already in it
;;;
(define get-trans
  (lambda (state msg tt)
    (hash-ref! (lookup-table-trans tt) (vector state msg)  (list))))
