;;
;; model-builder.scm
;;
;; machinery for generation of oneE and sysN models
;;

#lang scheme 

;;
;; build-oneEmodel (list-of-automata) -> model
;;
;; build-sysNmodel (list-of-automata )
;;
;; model2directed-graph: (model process-mask) -> (vector-of (list-of generic-transition?)) 
;;                                  "flat" representation of transitions without state info
;;
;; model2dot: (model filename process-mask) -> () <dump model to graphviz "dot" output>
;;
(provide model build-sysNmodel-builder model2dot 
         
         build-oneEmodel-builder
         
         ; (model) -> (list-of symbol?) : input messages for the first state's transitions
         ;               (used in search.scm to create heuristic)
         
         ;;; alternative interface for interactive model construction
         
         ; init-stepper: (transition-model topology) -> generation-function
         ;
         ; generation-function: (state [proc-mask (list)] [to-todo #f]) -> (list-of next-states)
         init-stepper
         
         ; todo->label: (todo) -> (label)
         todo->label
         
         ; todo->next-state: (todo) -> (next-state)
         todo->next-state
         
         ; shared for check-specific.scm
         make-mprocess
         
         ; for db update
         get-id
         
         ; (list-of frame?) -> (model?)
         make-model
         
         ; convert a hash-map of states to the first part of a struct-model 
         hash->model
         
         ; (state trans db) -> ()
         add-trans-to-set!
         
         (struct-out todo)
         
         ; shared for check-specific.scm and search.scm
         (struct-out mprocess))

(require "datatypes.scm")

(require "lookup-table.scm")

(require "parser.scm")


;; !!! LOCAL MAGIC VARIABLES !!!
(define oneE-flag -317) ; used for topology and sender-id for oneEmodel generation
(define sys-prefix "a") ; prefix for node names in dot output
; (so state 0 is "a0" ...)


; msg id of 'epsilon according to the trans-table
(define eps-id (void))

; debug level 
; (level >=3 gets raw topology instances)
; (level >=4 dumps trans tables)
; (level >=5 dumps *everything*)
(define debug 0)

;;;;;;;;;;;;;;;;;;
;;; datatypes
;;;;;;;;;;;;;;;;;;

;;
;; collection of information to uniquely identify any state+transition
;;
;; next-state and transition are both generated from a todo 
;;
; state: the state from which this "todo" describes a transition
; sent-msg: message consumed off an out-buffer of a process
; sent-id: index (in the system state list) of process whose message is consumed
; recv-id: index of receiving process
; cons-state: the new state of the consuming process
; msg2: the message produced by the transition
(define-struct todo (state msg send-id recv-id cons-state msg2 new-state) #:transparent)

;; (for backwards compatibility)
(define todo->next-state todo-new-state)

;; global function
(define init-stepper
  (lambda (prot topo-obj [verbose #f])
    (let ([lookup-table (create-lookup-table (protocol-ba prot))])
      (let-values ([(initial-auts topo-raw) (instantiate-protocol prot topo-obj)])
        (let-values ([(topo start-state) (topo-description&initial-auts->topo-hash&start topo-raw initial-auts lookup-table)])
          (values
           (lambda (state [proc-mask (list)] [to-todos? #f])
             (let ([todos (state->todos state topo lookup-table proc-mask)])
               (if to-todos? todos
                   (map todo->next-state todos)))) start-state lookup-table topo))))))

;;;;;;;;;;;;;;;;;;;
;;;; global function to create a model based on an initial state, full list of automata, and topo
;;;;;;;;;;;;;;;;;;;
(define build-sysNmodel-builder
  (lambda (prot) 
    (let ([lookup-table (create-lookup-table (protocol-ba prot))])
      (lambda (topo-obj [verbose #f])
        (let-values ([(initial-auts topo-raw) (instantiate-protocol prot topo-obj)])
          (let-values ([(topo start-state) (topo-description&initial-auts->topo-hash&start topo-raw initial-auts lookup-table)])
            (let* ([md (build-model start-state lookup-table topo)]
                   [model (hash->model md lookup-table topo start-state)])
              (make-model model lookup-table)))))))) 


;returns a model builder (which returns compact representations of 1e
; and the trans-table
(define build-oneEmodel-builder
  (lambda (prot)
    (let ([tt (create-lookup-table (protocol-ba prot) #t)])
      (values
       tt
       (lambda (proc-type initial-aut id-mask [verbose #f])
         (let* ([all-aut (protocol-ba prot)]
                ; set the global eps-id from the lookup table
                [dmy (set! eps-id (msg->msg-id eps tt))]
              ;  [initial-aut
              ;   (let* ([record (filter (lambda (x) (and
              ;                                       (equal? (car x) proc-type)
              ;                                       (= (cadr x) 0))) (protocol-start-conf prot))])
              ;     (if (not (null? record)) (caddar record) (car (filter (lambda (z) (equal? proc-type (automaton-proc-type z))) all-aut))))]
                [state-id (state->state-id (vector (automaton-proc-type initial-aut) (automaton-name initial-aut) #f) tt)]
                [initial-state (list (state->process state-id))]
                                   
            ;    [initial-state (if (eq? (automaton-in-msg initial-aut) eps) 
            ;                       (collect-all-eps initial-aut all-aut tt) 
                [dummy2 (if verbose (display-ln "Building oneEmodel...") (void))]
                [md (build-model initial-state tt)]
                [model (hash->model md tt oneE-flag initial-state)]
                ;; now filter out the taus and object transition types
                [stripped (strip-taus model id-mask tt)]
                ;; remove all duplicate paths
                [cleaned-model (reduce-model stripped)]
                ;; remove lonely nodes (without transitions to or from)
                [finished (compactify cleaned-model)])
           (make-model finished tt)))))))


;; simple front-end to compact-model 
(define (compactify model)
  (let ([mapper (make-hash)])
    (begin
      ; mark all reachable elements
      (mark-elements-rec model 0 mapper)
      (compact-model model mapper))))



(define (mark-elements-rec model index mapper)
  (let ([size (hash-count mapper)])
   (if (hash-has-key? mapper index)
      (void)
      (begin
        (hash-set! mapper index size)
        ;; add all children
        (for-each (lambda (x) (mark-elements-rec model x mapper))
              (map (lambda (z) (vector-ref z 3)) (vector-ref (vector-ref model index) 1)))))))

      
;;
;; strip-taus
;;
;; eliminates all tau transitions and transitions from another type from a given model
;;
;; (vector? symbol? lookup-table?) -> (vector?)
;;
(define (strip-taus-short raw-model proc-id lt)
  (let* ([model (remove-tau-linking raw-model lt)])
          model))

(define (strip-taus raw-model id-mask lt)
  (let* ([model (remove-tau-linking raw-model lt)]
         [new-model (make-vector (vector-length model))]
        ; lookups to compacted model (without "extra" start states)
         [mapper (make-hash)])
    (begin
      ; rebuild the model (leaving out transitions)
      (for-each (lambda (x)
                  (let ([entry (vector-ref model x)]
                        [new-entry (make-vector 2)])
                    (begin
                      ;; copy state
                      (vector-set! new-entry 0 (vector-ref entry 0))
                      ;; make blank trans list
                      (vector-set! new-entry 1 (list))
                      (vector-set! new-model x new-entry))))
                (build-list (vector-length new-model) values))

      ; for each state reachable via non-tau links, add the map
      (add-elements-rec 0 id-mask model new-model mapper)

         ;; return the compacted new model
         (compact-model new-model mapper))))


;; shrink a model (remove states and correct indexing) given a model and a map of old indices to new indices
(define (compact-model model mapper)
  (let ([new-model (make-vector (hash-count mapper))])
    (begin
      (hash-for-each mapper 
        (lambda (x y)
          (let ([state (vector-ref (vector-ref model x) 0)]
                [trans (vector-ref (vector-ref model x) 1)]
                [new-state (make-vector 2)])
          (begin
            ;; copy the old state into the new
            (vector-set! new-state 0 state)
            ;; change all the next-indices of the transitions
            ;; add the modified transitions to the state
            (vector-set! new-state 1 (map (lambda (z) 
                          (let ([new-tran (vector-copy z)])
                            (begin
                             (vector-set! new-tran 3 (hash-ref mapper (vector-ref z 3)))
                              new-tran))) trans))
            ;; write the whole thing into the new model
            (vector-set! new-model y new-state)))))
        ; return the new model
        new-model)))
          
            


(define (add-elements-rec index id-mask model new-model mapper)
   (if (hash-has-key? mapper index) (void)
     (let* ([trans (remove-duplicates (collect-endpoints index id-mask model))]
            [new-entry (vector-ref new-model index)]
            [size (hash-count mapper)])
          (begin
                (vector-set! new-entry 1 trans)
                (vector-set! new-model index new-entry)
                (hash-set! mapper index size)
                (for-each 
                  (lambda (x) (add-elements-rec (vector-ref x 3) id-mask model new-model mapper)) 
                  trans)))))

(define (collect-endpoints state-index id-mask model)
  (let ([ends (make-vector 1 (list))]
        [visit-list (make-hash)])
    (begin
      (collect-endpoints-rec state-index id-mask model ends visit-list)
      (vector-ref ends 0))))
    

(define (collect-endpoints-rec state-index id-mask model ends visited-list)
  (if (hash-has-key? visited-list state-index)
          ; if we've been here before, die
          (void)
      ;; count the transitions
     (let* ([non-pid-tran (filter (lambda (y) (member (state-id->proc-id (vector-ref y 2)) id-mask))
                                (vector-ref (vector-ref model state-index) 1))]
            [pid-tran (filter (lambda (y) (not (member y non-pid-tran)))
                                (vector-ref (vector-ref model state-index) 1))])
        (begin
          ; mark this index
          (hash-set! visited-list state-index #t)
        (cond
          ; if there are no taus from here, just add all these transitions
          ((null? non-pid-tran)
                      (vector-set! ends 0 (append pid-tran (vector-ref ends 0))))
          ; if everything is a tau transition, don't add these transitions, just their children
          ((null? pid-tran)
                     (for-each (lambda (z) (collect-endpoints-rec z id-mask model ends visited-list)) 
                                  (map (lambda (a) (vector-ref a 3)) non-pid-tran)))
          ; otherwise, add these transitions and their children
          (#t
              (begin
                  (vector-set! ends 0 (append pid-tran (vector-ref ends 0)))
                     (for-each (lambda (z) (collect-endpoints-rec z id-mask model ends visited-list)) 
                                  (map (lambda (a) (vector-ref a 3)) non-pid-tran)))))))))

;;
;; (vector?) -> (vector?)
;;
(define (remove-tau-linking model lt)
  (let ([new-model (make-vector (vector-length model))]
        ; lookup from index in old model to index in new model
        [old->new-map (make-hash)]
        [tau-id (msg->msg-id tau lt)])
    (begin
      ;; copy needed states into new model and generate lookup table
      (for-each 
        (lambda (x)
          (let ([element (vector-ref model x)]
                [size (hash-count old->new-map)])
            ; if this is a first state (for all system processes), remember it
            (if (all-process-states-even? (vector-ref element 0))
                (let ([new-element (make-vector 2)])
                (begin
                   ;; add element to lookup
                   (hash-set! old->new-map x size)
                   ;; copy state to new model
                   (vector-set! new-element 0 (vector-ref element 0))
                   ;; make blank link table
                   (vector-set! new-element 1 (list))
                   ;; copy into new model
                   (vector-set! new-model size new-element)))

                ;;otherwise, ignore
                (void))))
          (build-list (vector-length model) values))

      ;; now add links pointing in the correct places (skipping intermediates)
      (for-each
        (lambda (x)
          (let ([element (vector-ref model x)])
            ; if the state-id is even, then this is the first of the two automaton states
            ; (and so we care about correcting where it points)

            (if (not (all-process-states-even? (vector-ref element 0)))
                ; ignore this state
                (void)
                ; otherwise, clean up its references
                (for-each
                  (lambda (y)
                    (if (= tau-id (vector-ref y 0))
                        ;; skip taus here
                        (void)
                        ;; otherwise, clean up all the transitions that come from the state this
                        ;; transition leads to
                        (for-each
                          (lambda (z)
                            (if (or (not (= tau-id (vector-ref z 0)))
                                    (not (hash-has-key? old->new-map (vector-ref z 3))))
                                ;; skip non-taus and transitions which go to non-start states
                                (void)
                                (let* ([new-tran (make-vector 4)]
                                       [new-id (hash-ref old->new-map x)]
                                       [original-entry (vector-ref new-model new-id)])
                                 (begin
                                   ;; copy the in and out messages and process-id
                                   (vector-set! new-tran 0 (vector-ref y 0))
                                   (vector-set! new-tran 1 (vector-ref y 1))
                                   (vector-set! new-tran 2 (vector-ref y 2))
                                   ;; copy the next-id from this transision 
                                   (vector-set! new-tran 3 (hash-ref old->new-map (vector-ref z 3)))
                                   (vector-set! original-entry 1 (cons new-tran (vector-ref original-entry 1)))
                                   (vector-set! new-model new-id original-entry)))))
                                                              
                        (vector-ref (vector-ref model (vector-ref y 3)) 1))))
                  (vector-ref element 1)))))
           (build-list (vector-length model) values))
          ;; and return the new model (chopped down to size)
          (vector-copy new-model 0 (hash-count old->new-map)))))

;;
;; create the start state for the 1e model in the case where
;; this process starts (with first-aut) starts with epsilon 
;;
;; (automaton? (list-of automaton?) lookup-table?) -> (list-of process?)
;;
(define (collect-all-eps first-aut all-aut lt)
  (map state->process
       (map 
        (lambda (x)
          (state->state-id (vector (automaton-proc-type x)
                                   (automaton-name x) #f) lt))
        (cons first-aut (filter (lambda (y)
                                  (and 
                                   (not 
                                    (equal? 
                                     (automaton-proc-type y)
                                     (automaton-proc-type first-aut)))
                                   (equal? (automaton-in-msg y) eps)))
                                all-aut)))))


(define (all-process-states-even? system-state)
  (= 0 (foldl + 0 (map (lambda (x) (modulo (mprocess-state x) 2)) system-state))))

;;
;; construct a hash-map filled with all states reachable from 
;; ss (the start state) with a given transition table and topology
;;
;; (system-state transition-table [system-topology]) -> (hash-map)
;;
(define (build-model ss tt [topo oneE-flag])
  (let* ([state-space (make-hash)]
         [fringe (make-hash)]
         [dmy0 (hash-set! state-space ss #t)]
         [dmy (hash-set! fringe ss #t)])
    (build-model-rec tt fringe state-space topo)))

(define (build-model-rec tt fr ss topo)
  (if (zero? (hash-count fr)) ss
      (let ([expanded (list-of-lists->list (hash-map fr (lambda (x y) 
                                                          (map (lambda (z)
                                                                 (todo->next-state z))
                                                               (state->todos x topo tt)))))]
            [new-fr (make-hash)])
        (begin   
          (for-each
           (lambda (x) 
             (if (not (hash-has-key? ss x))
                 (begin (hash-set! ss x #t)
                        (hash-set! new-fr x #t))
                 (void)))
           expanded)
          (build-model-rec tt new-fr ss topo)))))

;; dump a model to graphviz source
;;
;;
(define model2dot
  (lambda (md filename [process-mask (list)] #:show-buf [sb? #t])
    (let* ([save (current-output-port)]
           [lookup-table (model-table md)]
           [mdl (model-mdl md)]
           [proc-mask-ids (map (lambda (x) (proc->proc-id x lookup-table)) process-mask)])
      (begin
        (if (file-exists? filename) (raise-user-error "output file already exists:" filename) (void))
        (current-output-port (open-output-file filename))
        (display-ln "/* model generated by model2dot */\n\ndigraph {\n\trankdir=LR\n\n")
        (for-each (lambda (x) (entry2dot x (vector-ref mdl x) lookup-table proc-mask-ids sb?))
                  (build-list (vector-length mdl) values))
        (display-ln "}")
        (close-output-port (current-output-port))
        (current-output-port save)))))

(define entry2dot
  (lambda (index entry lt pmi sb?)
    (begin
      (display-ln "\t" sys-prefix (number->string index) " [ label  = \"" 
                  (build-name (vector-ref entry 0) sb? lt)"\" ];")
      (for-each (lambda (x) (display-ln 
                             "\t" sys-prefix (number->string index) " -> " (trans2dot x lt pmi))) (vector-ref entry 1)))))

(define build-name
  (lambda (state show-buf? tt)
    (if (equal? (length state) 1) (process2dot (car state) show-buf? tt)
        (string-append (process2dot (car state) show-buf? tt) "\\n----------------\\n" (build-name (cdr state) show-buf? tt)))))


(define trans2dot
  (lambda (x tt pmi)
    (let ([in (msg-id->msg (vector-ref x 0) tt)]
          [out (msg-id->msg (vector-ref x 1) tt)]
          [proc-id (state-id->proc-id (vector-ref x 2))]
          [to-id (vector-ref x 3)])
      (if (member proc-id pmi)
          
          (string-append sys-prefix (number->string to-id) " [ label = tau ]")
          
          (string-append sys-prefix 
                         (number->string to-id) " [ label = \"" (symbol->string in) "/\\n" 
                         (symbol->string out) "\" ]")))))


(define process2dot
  (lambda (pr with-buf? tt)
    (let* ([aut (state-id->state (mprocess-state pr) tt)]
           [buff (mprocess-buff pr)])
      (string-append (symbol->string (vector-ref aut 0)) "\\n"
                     (symbol->string (vector-ref aut 1))
                     (if with-buf? (string-append "\\n" 
                                                  " {" (los->string (map (lambda (x) (msg-id->msg x tt)) buff)) "}") 
                         (if (vector-ref aut 2) "\\nstate2" "\\nstate1"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; next state (todo) generation  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-todos: (state link-table topology)->(list-of todo?)
;;
;;
;; state: list-of mprocess-struct
;; topo: hash-map mapping (vector recv-index recv-process-state in-msg send-index) ->  
;;                                                                    (list-of (vector new-state out-msg))
;;       or oneE-flag
;;
;; tt: lookup-table-struct
;; proc-mask: list of proc-ids not to expand
(define state->todos
  (lambda (state topo tt [proc-mask (list)])
    (let ([size (length state)])
      (flatten (map (lambda (x) (sender&msg->todos (car x) (cadr x) state tt topo proc-mask))
                    ;; if building a oneE model, allow all possible inputs
                    (let ([raw-input (if (equal? topo oneE-flag) 

                                        ;; making a 1e model... so if not in a "start state" for all automatons,
                                        (if (all-process-states-even? state)
                                          ;; allow anything but taus
                                          (remove (list (msg->msg-id tau tt) oneE-flag) 
                                            (map (lambda (x) (list x oneE-flag)) (lookup-table->all-msgs tt)))
                                          ;; allow only taus
                                          (list (list (msg->msg-id tau tt) oneE-flag)))
                                         ;; always include an epsilon message with a bogus sender in available message list
                                         (cons (list (msg->msg-id eps tt) -1)
                                               (list-of-lists->list (map (lambda (x) (collect-messages state x))
                                                                         (build-list size values)))))])
                      raw-input))))))

;;; state: list of processes
;;  id: index of process in passed state from which to collect messages
;;; return a list of (msg proc-id) where 
(define collect-messages
  (lambda (state x)
    (let ([buff (mprocess-buff (list-ref state x))])
      (map (lambda (msg) (list msg x)) buff))))

;; collect all todos for a single sender and message
;; by trying every possible transition permitted by the topo
(define sender&msg->todos
  (lambda (msg id state lt topo proc-mask)
    ;       initially try to send the message to everyone
    (let* ([raw_targets (build-list (length state) values)]
           ; remove targets in proc-mask
           [targets (filter (lambda (x)
                              (not (member (state-id->proc-id (mprocess-state (list-ref state x))) proc-mask))) raw_targets)])
      (let ([result 
             (list-of-lists->list
              (map 
               (lambda (x) 
                 (sender&msg&receiver->todos msg id state x topo lt)) targets))])
        result))))

;;
;; sender&msg&receiver->todos: (msg id oneEmodel) -> (list-of todo?)
;; 
;; given the output and id of a particular process, find other processes that can receive
;; the message, and return a list of todos representing this transition
;;
;; msg = message to receive
;; state = state creating from
;; recv-id = id of receiving automaton
;;
;; output: ( a list of <a-todo>)
(define sender&msg&receiver->todos
  (lambda (msg send-id state recv-id topo lt)
    (let* ([proc (list-ref state recv-id)]
           [st (mprocess-state proc)]
           [buff (mprocess-buff proc)])
      (make-todos state msg send-id recv-id 
                  (if (number? topo)
                      ; if oneE, give all possible transitions
                      (get-trans (mprocess-state (list-ref state recv-id)) msg lt)

                      (hash-ref! topo (vector recv-id (mprocess-state (list-ref state recv-id)) msg send-id) 
                          (list)))))))

(define make-todos
  (lambda (state msg send-id recv-id final-recv-pairs)
    (if (null? final-recv-pairs) '()
        (let* ([tp (car final-recv-pairs)]
               [new-state (vector-ref tp 0)]
               [out-msg (vector-ref tp 1)])
          (cons 
           (make-todo state msg send-id recv-id new-state out-msg 
                      (make-next-state state msg send-id recv-id out-msg new-state)) 
           (make-todos state msg send-id recv-id (cdr final-recv-pairs)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end todo generation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; generate the next state from
; state, in-msg, send-id, recv-id, out-msg, and new process state
(define make-next-state
  (lambda (state in-msg send-id recv-id out-msg new-pstate)
    (let* ([updated-state (clear-buffer state send-id in-msg)] ;; remove the message from the sender's out buffer
           [old-recv-buff (mprocess-buff (list-ref updated-state recv-id))]
           ;; if the message is already in the buffer, is an epsilon, or generating oneE model, don't add to the buffer
           [new-buffer (if (or (member out-msg old-recv-buff) (equal? in-msg eps) (equal? send-id oneE-flag)) old-recv-buff 
                           
                           ;; otherwise add the message to the buffer and sort it (so states with equal buffers are "equal?")
                           (sort (cons out-msg old-recv-buff) <))])
      (state-swap updated-state recv-id (make-mprocess new-pstate new-buffer)))))

;; clear-buffer (state id msg)->(state)   ;; remove an element msg from process "id" in state
(define clear-buffer
  (lambda (state id msg)
    (if (< id 0) ;; if the message had a bogus sender, don't clear anything (used for initial epsilon messages and oneE generation)
        state
        (let* ([proc (list-ref state id)] 
               [new-buff (remove msg (mprocess-buff proc ))]
               [new-proc (struct-copy mprocess proc [buff new-buff])])
          (state-swap state id new-proc)))))

;; create a new state by swapping process at id out for new-proc
(define state-swap
  (lambda (state id new-proc)
    (if (= 0 id) (cons new-proc (cdr state))
        (append (take state id) (list new-proc) (list-tail state (+ id 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  functions for managing database ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define state->id
  (lambda (state database)
    (let ([new-id (hash-count database)])
      (if (hash-has-key? database state)
          (values (car (hash-ref database state)) #t)
          (begin
            (hash-set! database state (list new-id '()))
            (values new-id #f))))))

(define state->trans
  (lambda (state database)
    (cdar (hash-ref database state))))

;;;;;;;;;;;;;;;;
; general utilities

(define add-trans-to-set!
  (lambda (state trans db)
    (let* ([entry (hash-ref db state)]
           [id (car entry)]
           [old-list (cadr entry)])
      (if (member trans old-list) (void)
          (hash-set! db state (list id (cons trans old-list)))))))


;;
;; hash-map has 
;;   keys: system states
;;   values: #t
;;
;; result vector elements are system states
;;
;; the values of those elements are in the form:
;;    (vector state-id (list-of (vector in-msg out-msg next-id)))
;;
;; where in-msg, out-msg are integer message ids (from trans-table mapping)
;;  and next-id is the index in the vector of next state
;;
;; (if state is passed, it will be the zeroth element in the vector)
;;
;; (hash-map [state]) -> (vector)
;;
(define hash->model
  (lambda (sp tt topo [ss #f] [state->representative (lambda (x . y) x)])
    (let ([db (make-hash)])
      (begin
        ; ensure the start state gets id 0
        (if ss (get-id ss db) (void))
        ; dump all states into db
        (hash-for-each sp (lambda (x y) (get-id x db)))
        ; dump all transitions into db
        (hash-for-each sp (lambda (x y)
                            (let ([todos (state->todos x topo tt)])
                              (for-each 
                               (lambda (z) 
                                 (let* ([ss-id (get-id (state->representative (todo-state z) sp) db)]
                                        [to-id (get-id (state->representative (todo->next-state z) sp) db)])
                                   (add-trans-to-set! (todo-state z) (todo->label to-id z) db)))
                               (filter (lambda (g) (and (hash-has-key? db (state->representative (todo->next-state g) sp)) (hash-has-key? db (state->representative (todo-state g) sp)))) todos)))))

        (let ([vect (make-vector (hash-count db))])
          (begin
            (hash-for-each db (lambda (x y)
                                (let ([id (car y)]
                                      [list-of-trans (cadr y)])
                                  (vector-set! vect id (vector x list-of-trans)))))
            vect))))))

(define (todo->label to-id td)
  (let ([in (todo-msg td)]
        [out (todo-msg2 td)]
        ; saved for filtering (on process type)
        [new-state (todo-cons-state td)])
    (vector in out new-state to-id)))

(define get-id
  (lambda (state db)
    (if (hash-has-key? db state)
        (car (hash-ref db state))
        (let ([new-id (hash-count db)])
          (begin
            (hash-set! db state (list new-id (list)))
            new-id)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; reduce model ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove duplicate transitions from given model and returns new model
(define reduce-model
  (lambda (md)
          (let* ([new-md (vector-copy md)]
                 [dummy (for-each (lambda (x) (strip-dups x new-md)) (build-list (vector-length new-md) values))]
                 [original-ends (mark-singletons md)]
                 [dummy1 (remove-dead-nodes new-md original-ends)])
      new-md)))

;;
;; helper for rec-strip
;;
;; returns true iff the in and out messages of the two transitions is equal
;;      
(define (labels-eq? t1 t2)
  (and (= (vector-ref t1 0) (vector-ref t2 0)) (= (vector-ref t1 1) (vector-ref t2 1))))

;; top level caller for "trace"... calls trace on all duplicates
(define strip-dups
  (lambda (index md)
    (rec-strip-dups 0 index md)))

(define rec-strip-dups
  (lambda (trans-index index md)
    (let ([full-list (vector-ref (vector-ref md index) 1)])
      (if
        (>= trans-index (length full-list)) (void) ; done
        (let* ([trans-list (list-tail full-list (+ 1 trans-index))]
               [trans-to-match (list-ref (vector-ref (vector-ref md index) 1) trans-index)]
               [matching (filter (lambda (x) (labels-eq? trans-to-match x)) trans-list)])
          (if (not (null? matching))
              (if (trace index trans-to-match (car matching) md)
                  (rec-strip-dups trans-index index md) ; we made a change, so run against the same trans again
                  (rec-strip-dups (+ 1 trans-index) index md)) ; otherwise, move on
              (rec-strip-dups (+ 1 trans-index) index md))))))) ; ditto here
          
;;
;; Returns a vector of #t/#f values tagging all the non-looped nodes in the model.
;; This is necessary to prevent them from being cleared in the "remove-dead-nodes"
;; function.
;;
;; (Checking for duplicate paths works by deleting the final parallel link and 
;; then cleaning up the dead ends back to the branch point. But we need to 
;; differentiate between non-loops in the original model and dead ends introduced
;; by the duplicate path identification algorithm.)
;;
;; (vector?) -> (vector?)
(define mark-singletons
  (lambda (mdl)
    (let ([mark-list (make-vector (vector-length mdl) #f)]
          [visited (make-vector (vector-length mdl) #f)])
      (begin
        (mark-rec 0 mdl mark-list visited)
        mark-list))))

(define mark-rec
  (lambda (index mdl mark-list visited)
        ; we've been here before, return whether or not we are part of a deadend path
    (if (vector-ref visited index) (vector-ref mark-list index)
    (begin
      ; mark this node as visited
      (vector-set! visited index #t)
      ; if this is a deadend, mark it and return true
    (cond 
      ((zero? (length (vector-ref (vector-ref mdl index) 1)))
          (begin
                (vector-set! mark-list index #t) #t))
         ; otherwise, if all children are dead ends, do the same 
      ((andmap (lambda (x) (mark-rec x mdl mark-list visited))
                              (map (lambda (z) (vector-ref z 3)) (vector-ref (vector-ref mdl index) 1)))
          (begin
                (vector-set! mark-list index #t) #t))
      (#t
          ; otherwise, we are not part of a dead end
          #f))))))

;;
;; strips non-loop transitions from md (except for nodes in blacklist)
;;
(define remove-dead-nodes
  (lambda (md blacklist)
    (let ([out-count (build-vector (vector-length md) (lambda (x) (list (length (vector-ref (vector-ref md x) 1)) x)))]
          [rev-g (reverse-graph md)])
      (remove-dead-rec out-count md rev-g blacklist))))

(define remove-dead-rec
  (lambda (out-count md rev-g blacklist)
  (let ([first (assoc 0 (vector->list out-count))])
    (if first
         (let ([index (cadr first)])
           (begin
              ; if this is on the blacklist, do not remove the link
             (if (vector-ref blacklist index)
                 (void)
                  ; otherwise, this was part of a duplicate path, so remove it
                 (remove-node index md rev-g out-count))
             (vector-set! out-count index (list -1 index)) ; mark this end node as checked
             (remove-dead-rec out-count md rev-g blacklist)))
           
           (void))))) ; otherwise, finished

;; remove all transitions to <index> in <md>
(define remove-node
  (lambda (index md rev-g count-g)
    (let ([nodes-with-trans-to (map (lambda (x) (vector-ref x 3)) (vector-ref (vector-ref rev-g index) 1))])
      (for-each (lambda (x) (for-each 
                             (lambda (y)
                               (begin
                                 (del-trans md x y)
                                      ; update reference count
                                 (vector-set! count-g x (list (- (car (vector-ref count-g x)) 1) x))))
                               (filter 
                                (lambda (z) (eq? (vector-ref z 3) index)) (vector-ref (vector-ref md x) 1))))
                nodes-with-trans-to))))
      

(define reverse-graph
  (lambda (md)
    (let* ([size (vector-length md)]
           [new-md (build-vector size (lambda (x) (make-vector 2)))])
      (begin
        ;; set up all elements of the new model with state information and blank transitions
        (for-each (lambda (x)
               (let ([element (make-vector 2)])
                (begin
                  ; copy state
                  (vector-set! element 0 (vector-ref (vector-ref md x) 0))
                  ; create empty list of transitions
                  (vector-set! element 1 (list))
                  (vector-set! new-md x element))))
              (build-list size values))

        ;; add all reversed transitions
        (for-each (lambda (x) (reverse-element x (vector-ref (vector-ref md x) 1) new-md)) (build-list size values))

        ;; return the created reverse map
        new-md))))

(define trace
  ; id: index of state with eq? labeled transitions
  ; t1, t2: eq? labeled transitions
  ; md: model referred to
  (lambda (id t1 t2 md) 
    (let ([res (trace-rec (list (list id t1)) (list (list id t2)) md)])
      (if res
          (begin
            (for-each (lambda (x) (del-trans md (car x) (cadr x))) res)
            #t)
          #f))))

; search for a continuation of equivalent transitions
(define trace-rec
  (lambda (l1 l2 md)
    (let ([id1 (vector-ref (cadar l1) 3)]
          [id2 (vector-ref (cadar l2) 3)])
      (if (= id1 id2) ; hit simulation
          l2
      (let ([trans1 (vector-ref (vector-ref md id1) 1)]
            [trans2 (vector-ref (vector-ref md id2) 1)])
             (if 
              (and (= (length trans1) 1) (= (length trans2) 1) (labels-eq? (car trans1) (car trans2)))
              (trace-rec (cons (list id1 (car trans1)) l1) (cons (list id2 (car trans2)) l2) md)
                 #f))))))

(define reverse-element
  (lambda (new-end lot mdl)
    (for-each (lambda (x) 
       (add-trans mdl (vector-ref x 3) new-end (vector-copy x))) lot)))

;;
;; lbl : a vector of length 4 < in-msg-id, out-msg-id, new-state-id, to-id (replaced with end) >
;; end : new to-id (index in mdl that this transition leads to)
;;
(define add-trans
  (lambda (mdl start end lbl)
    (let* ([cur (vector-ref mdl start)]
           [cur-trans (vector-ref cur 1)])
      (begin
        ; replace endpoint with new end index
        (vector-set! lbl 3 end)
        ; add this transition to the transition list for the new start element
        (vector-set! cur 1 (cons lbl cur-trans))
        (vector-set! mdl start cur)))))

(define del-trans
  (lambda (mdl index tr)
    (let* ([cur (vector-ref mdl index)]
           [cur-trans (vector-ref cur 1)]
           [new-trans (remove tr cur-trans)]
           [dmy (vector-set! cur 1 new-trans)])
      (vector-set! mdl index cur))))
