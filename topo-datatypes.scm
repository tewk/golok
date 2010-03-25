;
; topo-datatypes.scm
;
; datatypes for working with topologies
;

#lang scheme


(provide
 
  ;; create a topology object  
  ;;    from model name,
  ;;    a list of process name, number pairs,
  ;;    and a list of all links.
  ;;
  ;; (Links are in the format:
  ;;   (list process-name process-index link-type
  ;;              process-name process-index)
  ;;
  ;; (string? (list-of (symbol? number?))
  ;;        (list-of (symbol? number? symbol? number? symbol?))) -> (topology?)
  make-topology

  ;; (topology?) -> (string?)
  topology-model

  ;; returns a list of name, number pairs
  ;; (topology?) -> (list-of (symbol? number?))
  topology-counts

  ;; (topology?) -> 
  ;;   (list-of (symbol? number? symbol? symbol? number?))
  topology-links

  ;; (topology? string?) -> () : write topology out to file
  topology->file

  ;; (string?) -> (topology?) : read a topology from file
  file->topology

  ;; (topology?) -> (string?) : string representation of process counts
  topology->string
  
  ;; the list of (symbol? symbol? symbol?) describes 
  ;; legal message recipients:
  ;; (process-name message-name link-end)
  ;;
  ;; where link-end is lpeer, rpeer, peer, or self
  ;;
  ;; the semantics are: process-name can recieve 
  ;;      message-name from link-end
  ;;
  ;; (topology? (list-of (symbol? symbol? symbol?))) -> (vector?)
  topology-instantiate)
 

; for sllgen parsing
(require (lib "eopl.ss" "eopl"))

; for misc utility functions
(require "datatypes.scm")

; for file io
(require scheme/port)

; model: string path to the amf file for which this 
;        structure represents a particular configuration
;
; counts: list of pairs of symbols (process-name number-in-model)
;
; links: list of tuples of symbols (process-type number link process-type number)
;      (e.g.  ((thread 0 -- thread 1) (object 0 -> thread 2)))
(define-struct topology (model counts links) #:transparent)

(define topology->file
  (lambda (topo filename)
    (let ([model (topology-model topo)]
          [counts (topology-counts topo)]
          [links (topology-links topo)])
      (with-output-to-file filename
        (lambda ()
          (begin
            (display-ln "model: " model "\n")
            (display-ln "counts:\n")
            (for-each (lambda (x)
                        (display-ln (car x) " " (cadr x))) counts)
            (display-ln "\nlinks:\n")
            (for-each (lambda (x)
                        (display-ln (car x) " " (cadr x) " " (caddr x) " " (fourth x) " " (fifth x))) links)))
          'replace))))

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("#" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter "_" "-" "."))) symbol)
    (number (digit (arbno digit)) number)
    (link-type ("-" (or "-" ">" "E")) symbol)))   

(define the-grammar
  '((p_topo ("model:" identifier "counts:" (arbno type-count) "links:" (arbno link)) the-topo)
    (type-count (identifier number) a-type-count)
    (link (identifier number link-type identifier number) a-link)))

(define file->topology
  (lambda (filename)
    (let ([tp (s&p_file filename)])
      (cases p_topo tp
        (the-topo (mf tcs lol)
                  (make-topology (symbol->string mf) (map parse_tc tcs) (map parse_link lol)))))))

(define parse_tc
  (lambda (tc)
    (cases type-count tc
      (a-type-count (id num) (list id num)))))

(define parse_link
  (lambda (ln)
    (cases link ln
      (a-link (id1 num1 lt id2 num2) (list id1 num1 lt id2 num2)))))

;; datatypes for parsing files
(define-datatype
  p_topo
  p_topo?
  (the-topo
   (mf symbol?)
   (tcs (list-of type-count?))
   (lol (list-of link?))))

(define-datatype
  type-count
  type-count?
  (a-type-count
   (name symbol?)
   (count number?)))


(define-datatype
  link
  link?
  (a-link
   (p1 symbol?)
   (i1 number?)
   (lt symbol?)
   (p2 symbol?)
   (i2 number?)))

;; parse and scan a passed file
(define s&p_file
  (lambda (filename)
    ((sllgen:make-string-parser the-lexical-spec the-grammar)
     (slurp-file filename))))

(define slurp-file
  (lambda (filename)
    (port->string (open-input-file filename))))

(define new-env
  (lambda (x)
    (raise-user-error (string-append "Parse error: " 
                (symbol->string x) " is not bound."))))

(define extend-env
  (lambda (env sym proc_t ind)
    (lambda (x)
      (if (eq? x sym) (list proc_t ind)
          (env x)))))

; return a topo-vector for model-builder
;
; output is a vector of size # processes in topology
;
; each entry is a list of duples
; (msg index-of-process-this-message-can-be-sent-to)
;
(define topology-instantiate
  (lambda (topo prot-msgs)
    (let* ([ot (make-offset-table (topology-counts topo))]
           [model (build-model (topology-links topo) ot)]
           [size (vector-length model)]
           [vect (make-vector size (void))]
           [dummy (for-each (lambda (x) 
                              (make-msg-list! x
                                              (vector-ref model x)
                                              prot-msgs vect (ot_ri2pt x ot)))
                            (build-list size values))])
          vect)))

(define make-msg-list!
  (lambda (x type-list prot-msgs vect pt)
    (begin
      ;; set up links from type-list
      (vector-set! vect x (remove-duplicates (list-of-lists->list (map (lambda (x) (type2msg x prot-msgs)) type-list))))
      ;; add any self references 
      (for-each (lambda (y) (cons-to-vec vect x (list (cadr y) x))) 
                (filter (lambda (z) (and (eq? 'self (caddr z)) (eq? pt (car z)))) prot-msgs)))))

(define type2msg
  (lambda (type prot-msgs) ; each prot-msg is (proc-type, msg, link-type)
    (let* ([type-name (car type)]
           [proc-type (cadr type)] 
           [rid (caddr type)])
      
      ;; normal messages from type-link
      (map (lambda (x) (list (cadr x) rid))
           (filter (lambda (y) (and (eq? proc-type (car y)) (eq? type-name (caddr y)))) prot-msgs)))))

; (list-of-links result-from-verify-and-build-offsets) -> listing of each process, process type, and input id
(define build-model
  (lambda (lol offset-table) 
    (let* ([size (ot_size offset-table)]
           [vect (make-vector size (list))]
           [dummy (for-each (lambda (x) (add-link x vect offset-table)) lol)])
      vect)))

(define add-link
  (lambda (x vect offset-table)
    (let ([p1 (car x)]
          [i1 (cadr x)]
          [lt (third x)]
          [p2 (fourth x)]
          [i2 (fifth x)])
      (let ([r1 (ot_pi2ri p1 i1 offset-table)]
            [r2 (ot_pi2ri p2 i2 offset-table)])
        (cond
          ; r1 is the (parent of r2)
          ((eqv? lt '-E) (begin
                           (cons-to-vec vect r1 (list 'parent p2 r2))
                           (cons-to-vec vect r2 (list 'child  p1 r1))))
          ((eqv? lt '--) (begin
                           (cons-to-vec vect r1 (list 'peer  p2 r2))
                           (cons-to-vec vect r2 (list 'peer p1 r1)))
                           (cons-to-vec vect r1 (list 'lpeer p2 r2))
                           (cons-to-vec vect r2 (list 'rpeer p1 r1)))
          ((eqv? lt '->) (begin
                           (display-ln "WARNING: '->' is depreciated! Use '--' instead")
                           (cons-to-vec vect r1 (list 'lpeer p2 r2))
                           (cons-to-vec vect r2 (list 'rpeer p1 r1))))
          (#t (raise-user-error (string-append "Parse error: " 
              "Bad link type " (symbol->string lt) ". Must be one of -E, --, or ->"))))))))

(define topology->string
  (lambda (topo)
    (let* ([items (topology-counts topo)]
           [size (length items)]
           [last-item (entry->string (last items))])
      (cond ((= size 1) last-item)
            ((= size 2) (string-append (entry->string (car items)) " and " last-item))
            (#t (string-append (foldr string-append "" (append (map (lambda (x) (string-append (entry->string x) ", ")) (drop-right items 1)) (list "and " last-item)))))))))

; entry is (list symbol number)
(define entry->string
  (lambda (x)
    (string-append (number->string (cadr x)) " " (symbol->string (car x)))))
