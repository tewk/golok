;;
;; perm.scm
;;
;; included for permutation generation used in 
;; partial order reduction of star topologies
;;

;;
;; generate all possible perumations of a list of numbers (a set of numbers)
;; paste number 11447: Scheme Permutation Generation Algorithm;
;; assumption is that no two numbers are the same!
;;

#lang scheme
(provide generate-permutations)

;; general strategy:
;;  1. generate all possible permutations that keep the first number fixed.
;;  2. generate all possible permutations that have something else for the
;;     first number.
;;  append lists produces by 1. and 2.
;;
;;
;;  1.1 need to generate all permutations of the rest of the list,
;;   and then add element to the beginning of each of the lists produced.
;;   add-element-to-lists is defined below:


;;
;; add-element-to-lists consumes a number and a list of lists,
;;  and produces a list of lists.
;;
;; (add-element-to-lists elem lists) creates a list of lists, containing
;; one entry for each list found in lists (each element of lists is a list of 
;;  numbers). Each list generated gets elem tacked on the beginning of it.
;;
(define (add-element-to-lists elem lists)
  (cond 
   [(null? lists) (list)]
   [ else
   (cons 
    (cons elem (first lists))
    (add-element-to-lists elem (rest lists)))]))

;;
;; 2. strategy is to step through the original list, picking each element 
;;  and generating all permutations that start with that element.
;;  


;; generates all permutations that start with f  fixed 

;; (generate-permutations alist) consumes a list of numbers and produces
;;  a list of lists of numbers. Each list of numbers is unique in the
;;  ordering of elements, each list of numbers contains exactly the same
;;  numbers as are found in alist.
;;
;; sample: (generate-permutations '(1 2 3))
;;
(define (generate-permutations alist)
  (cond
   ;; empty list has no permutations, but we need to return a list)
   [(null? alist) (list empty)]
   ;; single element has only one permutation, but we need to
   ;;  return a list of lists.
   [(null? (rest alist)) (list alist)]
   [ else
   ;; more than one element - generate all lists by generating
   ;; the lists (permutations) that start with each element in turn
   (use-each-element-as-start alist)]))


;;
;; use-each-element-as-start consumes a list and produces a list of
;; lists of numbers. 
;;
;; (use-each-element-as-start alist) basically does the whole job, but does so
;; only by calling the helper function which deals with two lists
;;  (initially one of them is empty).

(define (use-each-element-as-start alist)
  (use-each-element-as-start-helper empty alist))


;;
;; use-each-element-as-start-helper consumes two lists of numbers 
;;    and returns a list of lists of numbers
;;
;; (use-each-element-as-start-helper done remaining) generates all
;; permutations of the elements defined in the two lists combined,
;;  such that the first element in the permutation is found in the list
;;  named remaining. Calls itself recursively to step over all the elements
;;  in remaining.
;;
;;  Note: uses add-element-to-lists to attach an element to the
;;   beginning of lists produces in call to generate-permutations
;;   (sort of a recursive call).
;;

(define (use-each-element-as-start-helper done remaining)
  (cond
   ;; if remaining is empty we don't need to do anything
   [(null? remaining) (list)]
   [else
  ;; combine the lists produced by generating all permutations
  ;; that start with (first remaining), with the lists produced
  ;; that start with everything else.
  (append
   (add-element-to-lists
    (first remaining)
    (generate-permutations
     (append done (rest remaining))))
   (use-each-element-as-start-helper
    (append done (list (first remaining)))
    (rest remaining)))]))
  
;;(generate-permutations '(1 2 3))
