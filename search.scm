; search.scm
;
; Algorithm for checking a system instance for  
; simulation of 1e of a particular process type.
;
; Sept. 5, 2009
; David Samuelson

#lang scheme

; trans struct type
(require "datatypes.scm")

; for process-protocol-names
(require "parser.scm")

; for topology->string
(require "topo-datatypes.scm")

; for init-stepper
;
; init-stepper: (prot topology) -> (step-function first-state)
;
; step-function: (state [proc-mask (list)]) -> (list-of next-states)
(require "model-builder.scm")

; for generate-permutations
(require "perm.scm")

; for state->state-id
(require "lookup-table.scm")

(provide
  ; search: (prot topology proc-type) -> (#t simulating-model) | (#f max-count)
  search)

;;;;;;;;;;;;;;;;;;;;
; global variables ;

;; 1E model as labeled-transition directed graph
(define oneE (void))

;; the expand function 
;;
;; returns all possible new states
;; (state) -> (list-of state)
(define expand (void))

;; maximum trace
(define max-trace 0)

;; process-type we are checking
(define proc-type (void))
(define proc-type-id (void))

;; default start automaton for process we are checking
(define start-aut (void))

;; mask of all other processes
(define other-mask (void))
(define other-mask-ids (void))

;; global unsatisfiability table (specific to search calls)
(define unsat (void))

;; function which maps a system state to a specific representative (~ is partial order reduction)
;;
;; (state? hash-map?) -> (state?)
(define state->representative (void))

;; hash-map of states already checked for simulation  (keys: system-states, values: #t)
(define state-space #f)

;; filter for checking a state for simulation (this is a heuristic)
;; TODO: implement this (since we changed to state-based instead of 
;;            automaton-based representation, this is harder to do)
(define sim-filter (void))

;; TODO: use this
(define start-depth #f)

;; TODO: use this
(define stop-depth #f)

; lookup table
(define lt #f)

; topology
(define topo-ht #f)

;; debug value

; 5 dumps 1E models, shows raw search results

(define debug 2)

; filter for branch pruning
(define start-state-filter (void))

; print optimizations debugging output
(define opt-dbg 0)

; ids of non-parameterized process names as integers
(define npp-ids (list))

; ids of parameterized process names as integers
(define pp-ids (list))

;; start state
(define start-state (void))
;;;;;;;;;;;;;;;;;;;;

;;; search function and small helper functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define search
  ; prot - protocol-struct
  ; topo - lookup-table-struct
  ; pt - process-index
  ; dfs - boolean
  ; dump - #f | integer-depth
  ; star - (list-of process-types of which system states are equivalent when swapped around)
  ; start - integer-depth | #f
  ; stop - integer-depth | #f
  (lambda (prot topo pt dfs oneE-start-aut
                            #:pruning [pr #t]
                            #:npp [npp (list)]
                            #:dump [dump #f] 
                            #:ring [r #f]
                            #:star [s (list)]
                            #:start [start-d #f]
                            #:stop [stop-d #f])
    (let-values ([(stepper ss lookup-table topo-hash) (init-stepper prot topo)])
      (begin

        ; save the ids of all parameterized system types 
        (set! npp-ids (map (lambda (x) (item-index x (protocol-process-names prot))) npp))
        (set! pp-ids (filter (lambda (x) (not (member x npp-ids))) (build-list (length (protocol-process-names prot)) values)))

        ; enable/disable pruning
        (set! start-state-filter (if pr start-aut-filter (lambda (x) #t)))
       
        ; TODO: generalize these reductions (and maybe figure them out from the topology)
        (cond 
          ((and r (not (null? s)))
                (begin
                (display-ln "WARNING: both ring and star partial order reductions enabled!")
                (display-ln "\t\t\tYou probably don't want that... disabling both.")
                (set! state->representative (lambda (x . y) x))))
           (r
                (begin
                  (display-ln "INFO: enabling ring partial order reduction")
                  (set! state->representative (ring-reducer-init (length (protocol-process-names prot))))))
           ((not (null? s))
                 (begin
                    (display-ln "INFO: enabling star partial order reduction on " (los->string s))
                    (set! state->representative (star-reducer-init ss s lookup-table))))
          (#t
                  (set! state->representative (lambda (x . y) x))))


        ; set a bunch of global (to this module) variables
        (set! start-state (state->representative ss))

        ; XXX: cleanup
        (set! lt lookup-table)
        (set! topo-ht topo-hash)
         
        ; TODO: use these
        (set! start-depth start-d)
        (set! stop-depth stop-d)

        ; TODO: set up branch pruning

        ;;; add wrapper around the expand function 
        ;; whicch handles state reduction
        (set! expand 
               (lambda (state [proc-mask (list)] [to-todos? #f])
            (if to-todos?
                ;; repackage all the todos to have reduces representation
                (map (lambda (td)
                        (let ([s1 (state->representative (todo-state td))]
                              [s2 (state->representative (todo->next-state td))])
                        (make-todo s1 (todo-msg td)
                                      (todo-send-id td)
                                      (todo-recv-id td)
                                      (todo-cons-state td)
                                      (todo-msg2 td)
                                        s2)))
                        (stepper state proc-mask #t))
                ;; otherwise, just map all the returned states
                (map state->representative (stepper state proc-mask)))))

        (set! proc-type pt)
        (set! start-aut (state->state-id (vector proc-type (automaton-state1 (process-default-aut (car 
                              (filter (lambda (x) (equal? proc-type (process-name x)))
                                                (protocol-processes prot)))))) lt))
        
        (set! other-mask (remove proc-type (protocol-process-names prot)))

        (set! proc-type-id (proc->proc-id proc-type lt))
        (set! other-mask-ids (map (lambda (x) (proc->proc-id x lt)) other-mask))

        (let-values ([(fresh-tt builder) (build-oneEmodel-builder prot)])
          (set! oneE (model-mdl (builder proc-type oneE-start-aut npp-ids))))
        
        ; debugging messages
        (if (>= debug 2) (display-ln "checking " (topology->string topo) " for simulation of "
                                     proc-type) (void))

        (if (and dump dfs) (display-ln "WARNING: using dfs, so ignoring #:dump option " dump) (void))

         ;; create a new unsatisfiability table
        (set! unsat (make-hash))

        ; logic starts here
        
        ; result is either a simulating subset model or false
        (let ([result (if dfs (search-dfs start-state) (search-bfs 0 (if dump dump #f)))])
          (cond 
            ((and (not (equal? #f dump)) (model? result))
              (begin
                 (display-ln "dumping (did NOT really find simulation)")
                  (values #t result)))
            (result
                (values #t (search->model result)))
              ; otherwise just return failure
              (#t
                (values #f max-trace))))))))

(define search-dfs
  (lambda (state)
     (search-dfs-rec state (make-hash))))
          
(define search-dfs-rec
  (lambda (state store)
      (begin 
          (if (zero? (modulo (hash-count store) 100)) (display-ln "\t" (hash-count store)
                          " states checked and " (hash-count unsat) " in unsat" ) (void))
      (if
         (hash-has-key? store state) #f
          (let ([ans (search-node state)])
            (if ans
              ; if we found an answer, just return it
                ans
            ; otherwise, check children
            (begin  
              (let* ([new-states (filter (lambda (y) (not (hash-has-key? store y)))
                          (remove-duplicates (expand state)))]
                                   

                ; remove all states that do not have a process of proc-type in the 
                ; initial state
                ; (because they cannot possibly simulate)
                    [possible-starts (filter start-state-filter new-states)])
                  (begin
                      (hash-set! store state #t)
                      (if (null? possible-starts) #f
                         (ormap (lambda (x) (search-dfs-rec x store)) possible-starts)))))))))))
        

;
; search state space with BFS
;
(define search-bfs
  (lambda (depth [dump #f])
    (begin
      ; reset the global space
      (set! state-space (make-hash))
      (search-bfs-rec depth (make-hash) dump))))

(define search-bfs-rec
  (lambda (depth fringe dump)
            ; new-fringe is a hash-map with new states as keys
      (let* ([new-fringe (get-fringe depth state-space fringe)]
            ; list of states in new fringe
             [gr (if new-fringe (hash-map new-fringe (lambda (x y) x)) #f)]
             [dbg-dummy (if (>= debug 2) (display-ln "\tdepth " depth " fringe has " (if gr (length gr) 0)) (void))])

        (cond ((and (not gr) (not dump)) #f) ; there are no more states to search
              ((and (not gr) dump) (make-model (hash->model state-space lt topo-ht start-state state->representative) lt))

        (#t
          ; check if some fringe node is the start of 1e simulating chunk
        (let ([sim (ormap search-node gr)])
          (cond 
            ; if we found a solution, return it
            (sim sim)
                    
            ; are we just dumping output?
            ((and dump (= 0 dump)) 
                      (make-model (hash->model state-space lt topo-ht start-state state->representative) lt))
            ; otherwise, keep going
            (#t
              (search-bfs-rec (add1 depth) new-fringe (if dump (sub1 dump) #f))))))))))

; create a state space and fringe
(define create-sp-and-fr
  (lambda ()
    (let ([sp (make-hash)]
          [sf (make-hash)])
      (begin
        (hash-set! sp start-state #t)
        (hash-set! sf start-state #t)
        (values sp sf)))))

; filter returns true only for systems states which may contain a proc-type process
; in its initial state
(define start-aut-filter
  (lambda (a) 
      (ormap (lambda (x) (= start-aut (mprocess-state x))) a)))

(define get-fringe
  (lambda (depth state-space fringe)
    ; if this is the first time called (and state space has one element), just return the first guy
    (if (eq? 0 (hash-count state-space)) 
            (begin
                (hash-set! state-space start-state #t)
                (hash-set! fringe start-state #t)
                fringe)
        ; expand once and return results
       (get-fringe-rec state-space fringe 1))))

(define get-fringe-rec
  (lambda (state-space fringe depth)
    (let ([fr (hash-map fringe (lambda (x y) x))])
      (cond
        ; if fringe empty, return fail
        ((= 0 (length fr)) #f)

        ; return fringe
        ((= 0 depth) fringe) 

        ; otherwise expand next
        (#t
          (let* ([new-fr (make-hash)]
                [new-states (filter (lambda (y) (not (hash-has-key? state-space y)))
                  (remove-duplicates 
                    (list-of-lists->list (map expand fr))))]
                ; remove all states that do not have a process of proc-type in the 
                ; initial state
                ; (because they cannot possibly simulate)
                [possible-starts (filter start-state-filter new-states)])
            (begin
              ; create new fringe
              (map (lambda (x) 
                (hash-set! new-fr x #t)) possible-starts)

              ; add all states to state-space
              (map (lambda (x)
                (hash-set! state-space x #t)) possible-starts)
              (get-fringe-rec state-space new-fr (sub1 depth)))))))))
;;;;;;;;;;;;;;;;; end search ;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; search-node function ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; eq-map is a vector corresponding to the oneE vector
; each element is a list of the following entry format
;
; (list entry-state (list todo next-index)*)*
;
; the entry-state is the state some todo in the eq-map points to.
; the list of todos corresponds to each of the labeled transitions of the oneE vector
; (each todo has info to represent state, labeled transition, and new state)
; (note the the state may not match the entry-state, but the state is reachable through 
; tau transitions)
(define search-node
  (lambda (state)
    (cond 
    ; fail fast if already in unsat map
    ((member 0 (hash-ref! unsat state (list))) #f)
    (#t (let ([eq-map (make-vector (vector-length oneE) (list))])
      (try-fit state 0 eq-map))))))

; try to fit state as 1e position index
(define try-fit
  (lambda (state index eq-map)
    (cond
      ; if sim-filter rejects, remove
    ;((not ((vector-ref sim-filter index) state)) #f)

    ; if already in the map, done
    ((assoc state (vector-ref eq-map index))  eq-map)
    
    ; if in unsatisfiability map, fail
    ((member index (hash-ref! unsat state (list))) #f)

    (#t
    ; otherwise, expand all possible branches
                ; first collect all states reachable via tau transitions
      (let* ([starts (explode state pp-ids)]
              ; then collect all the proc-type transitions possible from the start set
             [possibles 
                  ;(list-of-lists->list (map (lambda (x) (expand x other-mask-ids #t)) starts))]
                  (list-of-lists->list (map (lambda (x) (expand x npp-ids #t)) starts))]

              ; finally sort the transitions according to the needed 1e transitions
             [check-list (condense possibles index)])

        ; if condensing found unsimulated transitions (returned #f), update trace and leave
        (if (not check-list)
          (begin (update-trace eq-map) 
                ;add to unsatisfiability map
                 (for-each (lambda (x) (cons-to-hash unsat x index)) starts)
                  ; and fail
                  #f)

      ; otherwise, lets assume this state simulates
      (let ([new-em (make-vector (vector-length eq-map))])
        (begin
            ; first make a copy of the equivalence map
          (vector-copy! new-em 0 eq-map)

                ; current is a list of entries of (state (list-of (todo index)))
         (let* ([current (vector-ref new-em index)]
                [new-entry (list state (list))])
          ; add this state to the equivalence map
          (vector-set! new-em index (cons new-entry current)))

          ; make sure all transitions are simulated recursively
          (fit-children state check-list (vector-ref (vector-ref oneE index) 1) new-em index)))))))))

  ; condense: (list-of todo, oneE transition index to match) -> #f | (list-of (list-of todo))
(define condense
  (lambda (todos index)
    (let ([possible-list (map (lambda (x) 
                                (filter (lambda (y) 
                                      (and 
                                          (= (vector-ref x 0) (todo-msg y))
                                          (= (vector-ref x 1) (todo-msg2 y))))
                                todos))
                              (vector-ref (vector-ref oneE index) 1))])
      
      ; if there are any empties (1e transitions which are not simulated), fail
      (if (member (list) possible-list) #f possible-list))))

 ; return a list of *all* states reachable without using a transition in proc-mask
 ;  (including the passed state)
(define explode
  (lambda (state proc-mask)
    (let* ([collection (make-hash)]
           [fringe (make-hash)])
      (begin
        ; add start state to collection and fringe
        (hash-set! fringe state (void))
        (hash-set! collection state (void))
        (explode-rec collection fringe proc-mask)))))

(define explode-rec
  (lambda (collection fringe proc-mask)
    (let* ([fringe-step (remove-duplicates 
                            (list-of-lists->list 
                                  (hash-map fringe 
                                       (lambda (x y) (expand x proc-mask)))))]
           [new-fringe (make-hash)])
       (begin
          ;; add all new states in fringe-step to new-fringe and collection
         (for-each (lambda (x) (if (hash-has-key? collection x) (void)
                                          (begin
                                            ;; add to new fringe and collection
                                            (hash-set! collection x #t)
                                            (hash-set! new-fringe x #t)))) fringe-step)
         (if (= 0 (hash-count new-fringe)) (hash-map collection (lambda (x y) x))
            (explode-rec collection new-fringe proc-mask))))))

; update max-trace
;
; max-trace is the longest (incomplete) "trace" of the 1e model
(define update-trace
  (lambda (eq-map)
    (let ([ln (length (list-of-lists->list (vector->list eq-map)))])
      (if (> ln max-trace) (set! max-trace ln) (void)))))

; ensure "state"'s children (in check-list) recursively complete the oneE-list of transitions
;
; eq-map is the current equivalence map, and index is the index of the 1e model to simulate
;
; oneE-list is list of (vector in-msg-id out-msg-id ignore-id next-index)
(define fit-children
  (lambda (state check-list oneE-list eq-map index)
    (cond
      ((null? oneE-list) eq-map) ; all transitions found
      ((null? check-list) (error "fit-children: logic error: check-list and oneE-list different sizes?"))
      ((null? (car check-list)) 
              ; didn't find a match
              ;     add to unsat and fail
          (begin (cons-to-hash unsat state index) 
                  #f))
      (#t
          ; test the first element in the first slot
          (let* ([next-state (todo->next-state (caar check-list))]
                 [next-index (vector-ref (car oneE-list) 3)]
                 [res (try-fit next-state next-index eq-map)])
            (if res 
                ; the next state works... now add the link between its parent and it
              (let* ([entry (vector-ref res index)]
                     [to-change (if (assoc state entry) (assoc state entry) 
                                              (error "fit-children: logic error: original state entry not found"))]
                     [new-entry-element (list state 
                                      (cons (list (caar check-list) next-index) (cadr to-change)))]
                     [new-entry (cons new-entry-element (remove to-change entry))]
                     [dummy0 (vector-set! res index new-entry)])

              ; continue the call
              (fit-children state (cdr check-list) (cdr oneE-list) res index))

              ; if failed, recurse on this sublist of the check-list
              (fit-children state (cons (cdar check-list) (cdr check-list)) oneE-list eq-map index)))))))

; ================================== solution visualization ===================================== ;

; converts search results (for proc-type)  to standard model output
(define search->model
  (lambda (results)
    (let* ([db (make-hash)])
      (begin
        (add-all-states results db)
                                    ; pass the first state of simulation
        (let ([mdl (hash->model db lt topo-ht (caar (vector-ref results 0)) state->representative)])
            (make-model mdl lt))))))

; helper for search->model
;
; lookup id in db, add if does not exist

; helper for search->mode
;
; puts all states mentioned in the search result into the db
(define add-all-states
  (lambda (res db)
    (for-each (lambda (x) 
                  (for-each (lambda (y) (process-entry! y db)) x)) (vector->list res)))) 

; helper for add-all-states
;
; entry: a single entry in search result (start-state (list-of todo))
;
; from the start state, there is a link to the first state of every todo 
; using only tau transitions
;
; each todo encapsulates the "start state" process-type transition
;
; (so if start-state is not equal to todo-state, there is a tau link between them,
; and the transition from todo-state to todo->next-state is of this process type)
;
(define process-entry!
  (lambda (entry db)
    (let* ([start-state (car entry)]
            ; ensure start-state is in the db
           [ss-id (get-id start-state db)])
      ; all all transitions
      (for-each (lambda (x) 
                  (let ([td (car x)])
                ; edge-state is the initial state of the transition described by 
                ; each todo x
                ;
                ; if start-state does not equal the initial state of the proc-type transition,
                ; then at least one tau transition needed to happen, first
                    (begin
                     (if (not (equal? start-state (todo-state td)))
                         (connect-taus! start-state (todo-state td) db)
                         (void))
                      (add-td-to-db! td db)))) (cadr entry)))))

(define add-td-to-db!
  (lambda (td db)
    (let ([first-id (get-id (todo-state td) db)]
          [second-id (get-id (todo->next-state td) db)])
      (add-trans-to-set! (todo-state td) (todo->trans second-id td) db))))

;;
;; number: index of todo->next-state in db (the value returned by (get-id (todo->next-state todo)))
;; td: todo
;;
;; returns a vector in the form 
;;   ( in-msg out-msg process-type-of-reciever to-index)
;;
;; where in-msg, out-msg are msg ids
;;      process-type-of-reciever is procoss-type index (used for filtering in output...
;;                                                      so certain transition types can be tau'ed)
;;    to-id is the index of the state this transition points to
;;
;; (number todo) -> (vector)
;;
(define todo->trans
  (lambda (to-id td)
    (vector (todo-msg td) (todo-msg2 td) (todo-cons-state td) to-id)))

;; adds all intermediate states from start-state to end-state 
;; (using tau transitions)
(define connect-taus!
  (lambda (start-state end-state db) 
    ; dummy check
    (if  (equal? start-state end-state) (void)
      (let* ([end-db (make-hash)]
	     [dmy (hash-set! end-db (state->representative end-state) #t)]
             [possibles (remove-duplicates (expand start-state pp-ids #t))]
                ; since there *is* a path to the end-state, this ormap always gives a non #f result
                ;
                ; if it *does* return false, there is an error in our search algorithm
             [success? (ormap (lambda (x) (connect-taus-rec x end-db db)) possibles)])
          (if (not success?) (error "there is a bug in search!") (void))))))

; returns a list or #f: #f on failure or (list todo final-state) 
;     the todo which started this level and the final edge state
; (we need to return the first state of final-todo)
(define connect-taus-rec
  (lambda (init-td final-db db)
    (let* ([start-state (todo-state init-td)]
          [end-state (todo->next-state init-td)]
	  [dmy0 (if (>= debug 4) (display-ln "ctr call:\n\tss: " start-state "\n\tes: " end-state) (void))])
      (if (hash-has-key? final-db end-state)
        (begin
	        (if (>= debug 4) (display-ln "found connection in connect-taus-rec!") (void))
          ; add final transition
          (add-td-to-db! init-td db)
          #t)
          ;; add all generated todos to done-db and filter out todos that were previously there
        (let* ([possibles ;(filter (lambda (x) (if (hash-has-key? done-db x) #f #t))
                              (expand end-state pp-ids #t)]
	             [dbg0 (if (>= debug 4) (display-ln "possibles for connect-taus-rec: " possibles) (void))])
          (if (null? possibles) #f
            (let ([res (ormap (lambda (x) (connect-taus-rec x final-db db)) possibles)])
              ; if one of our children found the solution, add its transition and return
              (if res
                (begin
                  (add-td-to-db! init-td db) #t)
                #f))))))))

; =============================== partial order reduction =================================== ;
(define ring-reducer-init-big
  (lambda ()
    (let ([red-hash (make-hash)])
     (lambda (x)
        (let ([lookup (hash-ref! red-hash x #f)])
          ; if in table, return what we got
          (if lookup lookup
          ; otherwise add all types and point to this one
          (begin
            (for-each (lambda (y) (hash-set! red-hash y x)) (all-rotations x))
            x)))))))


(define all-permutations
  (lambda (x conf)
    (map (lambda (y) (mangle x y)) conf)))

; return a copy of the state with positions switched around
(define mangle
  (lambda (state mangle)
    (map (lambda (x) (list-ref state x)) mangle)))

;
; maps all states to their rotation with the lowest process ids first
;
; sz: number of separate process types
;
; x: system state to check
;
(define ring-reducer-init
  (lambda (sz)
      ; x: state
      ; y: ignored
     (lambda (x . y)
        (car (sort (all-rotations x sz) order-by-number)))))

;
;
;
;
(define star-reducer-init
  (lambda (example-state reducible-elements lt)
    (let* ([reducible-ids (map (lambda (x) (proc->proc-id x lt)) reducible-elements)]
           ; chunks is a list of pairs (integer? integer?)
           ; where (a b)
           ; means starting at a, b elements can be sorted
           ; (in other words, the sublist of length b starting at a 
           ; can be chopped out of the full list, sorted, and put back in)
           [chunks (map (lambda (x) (list (find-first-rec x example-state 0) (count-number x example-state)))
                                reducible-ids)])
          (lambda (z . y) 
            (reduce-chunks z chunks)))))

(define reduce-chunks
  (lambda (x chunks)
    (if (null? chunks) x
      (let ([start-index (caar chunks)]
            [len (cadar chunks)])
      (let-values ([(front mid) (split-at x start-index)])
        (let-values ([(to-sort back) (split-at mid len)])
          (reduce-chunks (append front (sort to-sort sort-mprocess) back) (cdr chunks))))))))

;
; return the first index of the list in which the process-type (parameter integer?) matches (car mprocess)
; (integer? (list-of mprocess?)) -> (integer?)
;
(define find-first-rec
  (lambda (id ls index)
    (if (= id (state-id->proc-id (mprocess-state (car ls))))
        index
        (find-first-rec id (cdr ls) (add1 index)))))

;
; return the number of processes in the state of process type id 
;
(define count-number
  (lambda (id st)
    (length (filter (lambda (x) (= id (state-id->proc-id (mprocess-state x)))) st))))

(define (sort-mprocess x y)
  (if (> (sort-mprocess-raw x y) 0) #f #t))
;
; returns -1, 0, or 1
; based on ordering of xs1 and ys1
; (-1 means xs1 < ys1, 0 : xs1 = ys1, 1 :xs1 > ys1)
(define (sort-mprocess-raw xs1 ys1)
  (let ([xs1-id (mprocess-state xs1)]
        [ys1-id (mprocess-state ys1)])
    (cond
      ((< xs1-id ys1-id) -1)
      ((> xs1-id ys1-id) 1)
      ;; states are equal, so compare buffers 
      (#t 
       (let* ([xbuf (mprocess-buff xs1)]
              [ybuf (mprocess-buff ys1)]
              [xlen (length xbuf)]
              [ylen (length ybuf)])
         (cond
           ((< xlen ylen) -1)
           ((> xlen ylen) 1)
           ((equal? xbuf ybuf) 0)
           ((which-is-first? xbuf ybuf))))))))
  
; x, y are system states (that is, a list of mprocess structs)
;
; returns true if x is "less than" y
(define (order-by-number x y)
      ; if x and y were completely equal, just say x is first
  (if (or (null? x) (null? y)) #t
    (let ([res (sort-mprocess-raw (car x) (car y))])
      (cond
        ((= res -1) #t)
        ((= res 1) #f)
        (#t (order-by-number (cdr x) (cdr y)))))))

; like order-by-number, except for simple lists of integers
;
; returns -1 if x < y
;
; WARNING: assumes x and y are non-equal
(define (which-is-first? x y)
  (let ([x1 (car x)]
        [y1 (car y)])
    (cond
      ((< x1 y1) -1)
      ((> x1 y1) 1)
      (#t (which-is-first? (cdr x) (cdr y))))))


; return a list of all rotations of a state (including the original)
; XXX: works only for rings
(define all-rotations
  (lambda (x sz)
      (let* ([times (/ (length x) sz)]
              ; pieces should be a list of lists
              ; (of separated process types)
              [pieces (chop-up x times)]
              [rots (map (lambda (y)
                          (list-of-lists->list
                          (map
                            (lambda (z)
                             (back-front z y))
                          pieces)))
                      (build-list times values))])
           rots)))

(define back-front
  (lambda (x split)
    (let-values ([(fr bk) (split-at x split)])
      (append bk fr))))

;;
;; divide a list into segment-sized pieces
;;
(define chop-up
  (lambda (piece segment-size)
    (if (null? piece) '()
      (let-values ([(front back) (split-at piece segment-size)])
        (cons front (chop-up back segment-size))))))
