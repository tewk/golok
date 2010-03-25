;;
;; parser.scm
;;
;; creates protocol-struct from amf file
;;
;; David Samuelson
;; May 19, 2009
;; 


#lang scheme
  
(provide

  ;; (string?) -> (protocol) : filename of .amf file
  parse-amf-file
          
  ;; (procotol?) -> (list-of symbol?) : process names
  protocol-process-names

  ;; (procotol?) -> (list-of automaton?) 
  protocol-ba

  ;; (procotol?) -> (list-of process?)
  protocol-processes

  ;; (protocol?) -> (list-of (proc-type index aut))
  protocol-start-conf

  ;; (protocol?) -> (string?) : filename of .topo file
  protocol-kernel

  ;; (protocol?) -> (list-of addition-rule?)
  protocol-addition-rules

  ;; (process?) -> (automaton?)
  process-default-aut

  ;; (process?) -> (symbol?) 
  process-name
           
  ;; output (list-of process?) is initial state of system model
  ;; output vector? defines legal message passing
  ;; (protocol? topology?) -> (values (list-of process?) vector?)
  instantiate-protocol)

  
  ; for eopl LL(1) parser
  (require  (lib "eopl.ss" "eopl"))

  ;; for file io 
  (require scheme/port)
  
  ;; for automaton datatype
  (require "datatypes.scm")

  ;; for topology datatype
  (require "topo-datatypes.scm")
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("#" (arbno (not #\newline))) skip)
      (identifier (letter (arbno (or letter digit "." "_" "/" "-" "?"))) symbol)
      (number ((or digit "-") (arbno digit)) number)))
  
  (define the-grammar
    '((p_protocol ((arbno "process" p_process)  p_topology initial-config) a-prot)
      (p_process (identifier "{" (arbno automaton) "}") a-process)
      (automaton (identifier ":" "[" identifier "," identifier "]" "->" "[" identifier "," identifier "]" ) simple-automaton)
       ;; link is a single entry in the connectivity table
      (p_link (identifier number link identifier number) a-link)

      (p_topology ( "topology " "{" "connectivity" "{" (arbno p_link) "}" (arbno "additionrule" addition-rule) "msgs" "{" (arbno p_transition) "}"  "}" ) a-p_topology)
                      
      (p_transition ("(" identifier "," identifier "," identifier ")") a-p_transition)

      (addition-rule (identifier "{" (arbno p_expr) "}") an-addition-rule)
      (p_expr ("require:" edge) require-expr)
      (p_expr ("remove:" edge) remove-expr)
      (p_expr ("add:" edge) add-expr)
      (p_expr ("create:" name) create-expr)
      (p_expr ("foreach" identifier identifier "{" (arbno p_expr) "}") extended-expr)

      (link ("--") peer-link)
      (link ("->") directed-link)
      (link ("-E") pc-link)

      (edge ( name link name) an-edge)
      (name ( identifier name-end ) qual-name)
      (name-end ( number ) number-name-end)
      (name-end ( identifier) variable-name-end)

      (initial-config ( "initial-config" "{" (arbno "(" start-spec ")" ) "}" ) the-initial-config)
      (start-spec (identifier number identifier) a-start-spec)

   ))   

  (define-datatype
    edge
    edge? 
    (an-edge (n1 name?)
            (link_type link?)
            (n2 name?)))

(define-datatype
  link
  link?
  (peer-link)
  (directed-link)
  (pc-link))

(define-datatype
  p_link
  p_link?
  (a-link
   (p1 symbol?)
   (i1 number?)
   (lt link?)
   (p2 symbol?)
   (i2 number?)))

  (define-datatype
     addition-rule
     addition-rule?
    (an-addition-rule 
        (name symbol?)
        (exprs (list-of p_expr?))))

  (define-datatype
    p_expr
    p_expr?
    (require-expr (ln edge?))
    (remove-expr (ln edge?))
    (add-expr (ln edge?))
    (create-expr (nm name?))
    (extended-expr
        (process-type symbol?)
        (bound-var symbol?)
        (exprs (list-of p_expr?))))

  (define-datatype
    name
    name?
    (qual-name (id symbol?)
               (end name-end?)))

  (define-datatype
    name-end
    name-end?
    (number-name-end (num number?))
    (variable-name-end (var symbol?)))
      
  ;; parse and scan a passed file
  (define s&p_file
    (lambda (filename)
      ((sllgen:make-string-parser the-lexical-spec the-grammar)
       (slurp-file filename)
       )))

; define amf-directory for later init
(define amf-directory (void))
  
  (define slurp-file
    (lambda (filename)
        (port->string (open-input-file filename))))
  
  ;; datatypes for parsing files
  (define-datatype
    p_protocol
    p_protocol?
    (a-prot
     (proc-list (list-of p_process?))
     (topo p_topology?)
     (init initial-config?)))
     
  
  (define-datatype
    p_process
    p_process?
    (a-process
     (name symbol?)
     (auts (list-of p_automaton?))))
     
  
  (define-datatype
    p_topology
    p_topology?
    (a-p_topology 
     (links (list-of p_link?))
     (add-rules (list-of addition-rule?))
     (trans (list-of p_transition?))))

  (define-datatype
    p_transition
    p_transition?
    (a-p_transition 
               (process-type symbol?)
               (msg symbol?)
               (from-what-link-type symbol?)))
  
    (define-datatype
      initial-config
      initial-config?
      (the-initial-config
       (starts (list-of start-spec?))))
    
    (define-datatype
      start-spec
      start-spec?
      (a-start-spec
       (process-name symbol?)
       (process-index number?)
       (automaton-name symbol?)))
    
    (define-datatype
      p_automaton
      p_automaton?
      (simple-automaton
       (name symbol?)
       (state1 symbol?)
       (in-msg symbol?)
       (state2 symbol?)
       (out-msg symbol?)))
    
    ;;;;;;;;;
    (define-struct protocol (process-names processes addition-rules topo-msgs start-conf kernel) #:transparent)
    
    (define-struct process (name auts default-aut) #:transparent)
    
    (define protocol-ba
      (lambda (prot)
        (list-of-lists->list (map process-auts (protocol-processes prot)))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; (protocol-struct topology-struct) -> (values (list-of automaton) topo-vect)
    ;;
    ;; 
    ;;
    ;;
    (define instantiate-protocol
      (lambda (prot topo)
        (let* ([p_names (protocol-process-names prot)]
               [def-auts (map process-default-aut (protocol-processes prot))]
               [counts (map (lambda (x) (cadr x)) (topology-counts topo))]
               [sc (protocol-start-conf prot)]
               [start-stack (make-start-stack p_names def-auts counts sc 0)]
               [topo-vect (topology-instantiate topo (protocol-topo-msgs prot))])
          (values start-stack topo-vect))))
    
    (define make-start-stack
      (lambda (p_names def-auts counts sc cur)
        (cond 
          ((and (null? def-auts) (null? p_names) (null? counts))  '())
          ((or (null? def-auts) (null? p_names) (null? counts))   
                      (error 'ARGS "bad call to make-start-stack. unbalanced args"))
          ; done with this process type?
          ((= cur (car counts))
           (make-start-stack (cdr p_names) (cdr def-auts) (cdr counts) sc 0))
          
          ; handle additions
          (#t
           (let ([matches (filter (lambda (x) (and (equal? cur (cadr x)) (equal? (car p_names) (car x)))) sc)])
             (cond 
               ((> (length matches) 1) (raise-user-error 'ERROR "mutiple explicit start states for "(car p_names) matches))
               ; add the default automaton
               ((null? matches) (cons (car def-auts) (make-start-stack p_names def-auts counts sc (add1 cur))))
               ; add the explicitly referenced automaton
               (#t (cons (caddar matches) (make-start-stack p_names def-auts counts sc (add1 cur))))))))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; read a protocol and return a protocol structure
    ;;
    (define parseProtocol
      (lambda (prot filename)
        (cases p_protocol prot
          (a-prot (p_processes topo_r init)
            (cases p_topology topo_r
              (a-p_topology (p_links p_add-rules p_transitions)
                (begin
                  ; first do some quick sanity checks
                  (if (zero? (length p_processes)) (raise-user-error 'PARSE "no processes defined!") (void))
                  (if (zero? (length p_add-rules)) (raise-user-error 'PARSE "no addition rules defined!") (void))
                  (let* ([procs (map p_proc2proc p_processes)]
                         [add-rules (map parse-rule p_add-rules)]
                         [the-topo (parse-topo p_transitions)]
                         [names (map process-name procs)]
                         [auts (list-of-lists->list
                                (map process-auts procs))]
                         [start (parse-init init auts)]
                         [kernel (create-kernel filename p_links)])
                    (make-protocol names procs add-rules the-topo start kernel)))))))))
    
    ;; topology object -> list of allowed transitions 
    (define parse-topo
      (lambda (tr)
         (map trans-unpacker tr)))

    (define trans-unpacker
      (lambda (x)
        (cases p_transition x
          (a-p_transition (a b c) (list a b c)))))
    
    
    (define parse-init
      (lambda (init auts)
        (cases initial-config init
          (the-initial-config (loss)
                              (map (lambda (x) (parse-start-spec x auts)) loss)))))
    
    (define parse_link
      (lambda (ln)
        (cases p_link ln
          (a-link (id1 num1 lt id2 num2) (list id1 num1 (unwrap-lt lt) id2 num2)))))
    
    (define parse-start-spec
      (lambda (spec auts)
        (cases start-spec spec
          (a-start-spec (process-type index aut-name)
                        (let* ([aut-list (filter (lambda (x) (and 
                                                              (equal? aut-name (automaton-name x)) (equal? process-type (automaton-proc-type x)))) auts)]
                               [aut (if (= (length aut-list) 1) (car aut-list)
                                        (error 'PARSE "found " (length aut-list) " matches for " process-type aut-name ", need exactly one"))])
                          (list process-type index aut))))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    
    (define parse-amf-file
      (lambda (x)
        (parseProtocol (s&p_file x) x)))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define p_proc2proc
      (lambda (proc)
        (cases p_process proc
          (a-process (name ba)
                     (let* ([auts (map (lambda (x) (to-aut x name)) ba)]
                            [def-aut (if (not (> (length auts) 0)) (error 'PARSE "no behavioral automata given! ") (car auts))])
                       (make-process name (map (lambda (x) (to-aut x name)) ba) def-aut))))))
    
    (define to-aut
      (lambda (aut proc-type)
        (cases p_automaton aut
          (simple-automaton (the-name state1 in-msg state2 out-msg)
                            (make-automaton proc-type the-name state1 in-msg state2 out-msg)))))

  ; turn a rule definition into a function which is applied to a topology structure (from topo-datatypes.scm)
  (define parse-rule
    (lambda (x)
      (cases addition-rule x
        (an-addition-rule (name loe)
          (let* ([exprs (parse-expressions loe)]
                 [req-expr (filter (lambda (x) (eq? 'require (expr-type x))) exprs)]
                 [create-expr (filter (lambda (x) (eq? 'create (expr-type x))) exprs)]
                 [rem-expr (filter (lambda (x) (eq? 'remove (expr-type x))) exprs)]
                 [add-expr (filter (lambda (x) (eq? 'add (expr-type x))) exprs)]
                 [ext-expr (filter (lambda (x) (eq? 'foreach (expr-type x))) exprs)])
            ; TODO: extend this to supply number of possible applications
            (lambda (option topo) ; [index 0]) ; XXX: interpreter won't accept this... broken?
              (let* ([t_counts (topology-counts topo)]
                     [t_links (topology-links topo)]
                     [req-rule-indicies (map (lambda (x) (count-options x t_links)) req-expr)]
                     [num-combos (foldl * 1 req-rule-indicies)])
              (cond 
                ((eq? option 'query) num-combos)
                ((eq? option 'apply) 
                      (let* ([env (new-env)]
                            ; TODO: this is broken
                              ; XXX: binding of first level is special
                            [req-env (foldl (lambda (x z) (add-rule x t_links z)) env req-expr)]
                            [new-counts-and-env (foldl (lambda (x y) (if (not (valid-create-expr? x)) (error "parse-rule failed on broken create rule " x)
                                        (let* ([dtm (expr-datum x)]
                                               [prt (car dtm)]
                                               [vn (cadr dtm)]
                                               [index (cadr (assoc prt (car y)))])
                                                    (list 
                                            (update-counts (car (expr-datum x))  (car y))
                                            (extend-env (cadr y) vn prt index ))))) (list t_counts req-env) create-expr)]
                            [new-counts (car new-counts-and-env)]
                            [full-env (cadr new-counts-and-env)])
                      (make-topology (topology-model topo)
                                      new-counts (apply-exprs (append rem-expr add-expr ext-expr) t_links new-counts full-env))))))))))))

; basic format checking for create expr
(define valid-create-expr?
  (lambda (x)
    (and (eq? 'create (expr-type x)) (symbol? (cadr (expr-datum x))))))

(define apply-exprs
  (lambda (loe link-list counts env)
    (if (null? loe) link-list
     (let ([type (expr-type (car loe))]
           [datum (expr-datum (car loe))])
      (cond
        ((eq? type 'remove) (apply-exprs (cdr loe) (remove (resolve datum env) link-list) counts env))
        ((eq? type 'add) (apply-exprs (cdr loe) (cons (resolve datum env) link-list) counts env))
        ((eq? type 'rec) (apply-exprs (cdr loe) link-list counts (add-rule (car loe) link-list env)))
        ((eq? type 'foreach)
            (let* ([pt (car datum)]
                   [var (cadr datum)]
                   [loe2 (caddr datum)]
                   [size (cadr (assoc pt counts))])
                (foldl (lambda (ct ll)
                            (apply-exprs loe2 ll counts (extend-env env var pt ct))) link-list (build-list size values))))
        (#t (error "apply-exprs called on unknown expr type " type)))))))
                  
                      
(define update-counts 
  (lambda (pn counts)
    (cond
      ((null? counts) '())
      ((eq? pn (caar counts)) (cons (list pn (add1 (cadar counts))) (update-counts pn (cdr counts))))
      (#t (cons (car counts) (update-counts pn (cdr counts )))))))

(define resolve
  (lambda (link env)
    (let ([p1 (car link)]
          [i1 (cadr link)]
          [lt (caddr link)]
          [p2 (fourth link)]
          [i2 (fifth link)])
      (cond
        ((and (eq? 'var p1) (eq? 'var p2)) (append (env i1) (list lt) (env i2)))
        ((eq? 'var p1) (append (env i1) (list lt p2 i2)))
        ((eq? 'var p2) (append (list p1 i1 lt) (env i2)))
        (#t link)))))


; returns false if req rul cannot be applied, otherwise returns extended env
(define add-rule
  (lambda (req-expr links env) ; [match-no 0] ; XXX: broken parser?
    (let* ([datum (expr-datum req-expr)]
           [dbg-dummy (if (not (eq? 'require (expr-type req-expr))) 
                  (error "tried add-rule on non-require expr") (void))]
           [p1 (car datum)]
           [i1 (cadr datum)]
           [lt (caddr datum)]
           [p2 (fourth datum)]
           [i2 (fifth datum)]
           [dbg-var (if (> (length links) 1) (cadr links) (car links))]
           [match-list
    (cond
      ((and (symbol? i1) (symbol? i2))
            (filter (lambda (x) 
                  (and (eq? p1 (car x))
                       (equal? lt (caddr x))
                       (eq? p2 (fourth x)))) links)) 
      ((symbol? i1)
            (filter (lambda (x) 
                  (and (eq? p1 (car x))
                       (equal? lt (caddr x))
                       (eq? p2 (fourth x))
                       (eq? i2 (fifth x)))) links))
      ((symbol? i2)
            (filter (lambda (x) 
                  (and (eq? p1 (car x))
                       (equal? lt (caddr x))
                       (eq? p2 (fourth x))
                       (eq? i1 (cadr x)))) links))
      (#t (if (member datum links) (list datum) #f)))]
            [match (if (> (length match-list) 0) (list-ref match-list 0)
                        #f)])
            (if match 
              (let* (
                [first-bound-env (if (symbol? i1) 
                  (extend-env env i1 p1 (cadr match)) env)]
                [second-bound-env (if (symbol? i2) 
                  (extend-env first-bound-env i2 p2 (fifth match)) 
                                                 first-bound-env)])
                second-bound-env)
                #f))))


(define-struct expr (type datum) #:transparent)

; wrap expressions in struct-expr objects
(define parse-expressions
  (lambda (loe)
   (if (null? loe) '()
      (cases p_expr (car loe)
        (require-expr (ed) (cons (make-expr 'require (edge2list ed)) (parse-expressions (cdr loe))))
        (remove-expr (ed) (cons (make-expr 'remove (edge2list ed)) (parse-expressions (cdr loe))))
        (add-expr (ed) (cons (make-expr 'add (edge2list ed)) (parse-expressions (cdr loe))))
        (create-expr (nm) (cons (make-expr 'create (name2list nm)) (parse-expressions (cdr loe))))
        (extended-expr (p_type bound_var sloe) (cons (make-expr 'foreach (list p_type bound_var (parse-expressions sloe))) (parse-expressions (cdr loe))))))))


(define edge2list
    (lambda (x)
      (cases edge x
        (an-edge (n1 lt n2) (append (name2list n1) (cons (unwrap-lt lt) (name2list n2)))))))

(define unwrap-lt
  (lambda (lt)
    (cases link lt
      (peer-link () '--)
      (directed-link () '->)
      (pc-link () '-E))))

(define name2list
  (lambda (x)
    (cases name x
      (qual-name (id end)
        (cases name-end end
          (number-name-end (z) (list id z))
          (variable-name-end (z) (list id z)))))))
        
; count the number of possible bindings for this requirement
(define count-options
  (lambda (x t_links)
    (let* ([type (expr-type x)]
           [datum (expr-datum x)]
           [p1 (car datum)]
           [i1 (cadr datum)]
           [lt (caddr datum)]
           [p2 (fourth datum)]
           [i2 (fifth datum)])
    (cond
      ((and (symbol? i1) (symbol? i2))
            (length (filter (lambda (x) 
                  (and (eq? p1 (car x))
                       (eq? lt (caddr x))
                       (eq? p2 (fourth x)))) t_links)))
      ((symbol? i1)
            (length (filter (lambda (x) 
                  (and (eq? p1 (car x))
                       (eq? lt (caddr x))
                       (eq? p2 (fourth x))
                       (eq? i2 (fifth x)))) t_links)))
      ((symbol? i2)
            (length (filter (lambda (x) 
                  (and (eq? p1 (car x))
                       (eq? lt (caddr x))
                       (eq? p2 (fourth x))
                       (eq? i1 (fifth x)))) t_links)))
      (#t (if (member datum t_links) 1 0))))))
       
(define new-env
  (lambda ()
    (lambda (x)
      (error "symbol " x " not found!"))))

(define extend-env
  (lambda (env sym proc_t index)
    (lambda (x)
      (if (eq? sym x) (list proc_t index)
          (env x)))))

;;
;; (string? (list-of link?)) -> topology?
;;
(define (create-kernel filename p_links)
  (let* ([links (map parse_link p_links)]
         [counts (count-links links)])
    (make-topology filename counts links)))

;;
;; (list-of link?
;;
(define (count-links links)
  (let ([db (make-hash)])
    (begin
      (count-links-rec links db)
      (hash-map db (lambda (x y) (list x y))))))

(define (count-links-rec links db)
  (if (null? links) (void)
    (let* ([ln (car links)]
           [name1 (car ln)]
           [sz1 (second ln)]
           [name2 (fourth ln)]
           [sz2 (fifth ln)])
      (begin
        (process-link-entry name1 sz1 db)
        (process-link-entry name2 sz2 db)
        (count-links-rec (cdr links) db)))))

(define (process-link-entry name sz db)
  (if (not (hash-has-key? db name))
      (hash-set! db name (add1 sz))
      (let ([prev-sz (hash-ref db name)])
            ;; if already have a larger count, ignore
        (if (> prev-sz sz) (void)
            (hash-set! db name (add1 sz))))))
