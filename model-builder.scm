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

         build-oneEmodel-builder-new

          ; (model) -> (list-of symbol?) : input messages for the first state's transitions
          ;               (used in search.scm to create heuristic)
        
          ;;; alternative interface for interactive model construction
          
          ; init-stepper: (transition-model topology) -> generation-function
          ;
          ; generation-function: (state [proc-mask (list)] [to-todo #f]) -> (list-of next-states)
          init-stepper

          ; struct
          todo-state

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
(define build-oneEmodel-builder-new
  (lambda (prot)
    (let ([tt (create-lookup-table (protocol-ba prot))])
  (values
    tt
    (lambda (proc-type [verbose #f] #:with-heuristics [heur? #f])
      (let* ([all-aut (protocol-ba prot)]
             [dmyyy (set! eps-id (msg->msg-id eps tt))]
             [initial-todos '()]
             [initial-aut
       (let* ([record (filter (lambda (x) (and
                                      (equal? (car x) proc-type)
                                      (= (cadr x) 0))) (protocol-start-conf prot))])
                                      (if (not (null? record)) (caddar record) (car (filter (lambda (z) (equal? proc-type (automaton-proc-type z))) all-aut))))]
             [state-id (state->state-id (vector (automaton-proc-type initial-aut) (automaton-state1 initial-aut)) tt)]
             [initial-state (list (state->process state-id))]
             [dummy2 (if verbose (display-ln "Building oneEmodel...") (void))]
             [md (build-model initial-state tt)]
             [model (hash->model md tt oneE-flag initial-state)])
           (make-model model tt)))))))

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
                   " {" (los->string (map (lambda (x) (msg-id->msg x tt)) buff)) "}") "")))))
  
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
     (let ([raw-input (if (equal? topo oneE-flag) (map (lambda (x) (list x oneE-flag)) (lookup-table->all-msgs tt))
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
                                                    (get-trans (mprocess-state (car state)) msg lt)
                                                    (hash-ref! topo 
                                                      (vector recv-id (mprocess-state (list-ref state recv-id)) msg send-id) (list)))))))
  
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
                                (filter (lambda (g) (hash-has-key? db (state->representative (todo->next-state g) sp))) todos)))))
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
