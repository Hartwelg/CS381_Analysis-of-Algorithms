#lang racket
(define (f lst)
  ; takes a list of numbers and adds 1 to each element
  ; (a) ;
  ; if lst is null or empty ;
  (if (null? lst)
      ; (b) ;
      ; return false ;
      '()
      ; (c) ;
      ; else, add 1 to first element of lst, then call f on next element ;
      (cons (+ 1 (car lst)) (f (cdr lst)))))
      ; (cons (+ 1 (3)) (f (1 4 1 5 9))) -> (cons (+ 1 (1)) (f (4 1 5 9)))
      ; -> (cons (+ 1 (4)) (f (1 5 9))) -> (cons (+ 1 (1)) (f (5 9)))
      ; -> (cons ( + 1 (5)) (f (9))) -> (cons (+ 1 (9)) (f ()))

;determine if given arg 'e' is member of lst ;
(define (member? e lst)
  ; if lst is empty or null ;
  (if (null? lst)
      ; return false ;
      '#f
      ; if e and the first item in list are equal, return true ;
      ; else run member? on next member in lst ;
      (cond ((eqv? e (car lst)) '#t)
            (else (member? e (cdr lst))))))

; determine if lst is a well-formed set ;
(define (set? lst)
  ; if list is empty, return true ;
  ; if head of lst has any matches in tail of list, return false ;
  ; else, recursively check tail of list in same fashion until empty ;
  (cond ((null? lst) '#t)
        ((member? (car lst) (cdr lst)) '#f)
        (else (set? (cdr lst)))))

; return the union of two sets ;
(define (union lst1 lst2)
  ; if lst1 is empty, return lst2 ;
  ; if first element of lst1 is a member of lst2, call union with remainder of lst1 ;
  ; else, append first element of lst1 to lst2 and call union on remainder of lst1 ;
  (cond ((null? lst1) lst2)
        ((member? (car lst1) lst2) (union (cdr lst1) lst2))
        (else (union (cdr lst1) (cons (car lst1) lst2)))))

(define (intersect lst1 lst2)
  ; if lst1 is empty/null, return empty set ;
  ; if head of lst1 is in lst2, ignore and call intersect with tail of lst1 ;
  ; else, remove and call intersect on remainder of lst1 ;
  (cond ((null? lst1) '())
        ((member? (car lst1) lst2) (cons (car lst1)(intersect (cdr lst1) lst2)))
        (else (intersect (cdr lst1) lst2))))

(define (difference lst1 lst2)
  ; if lst1 is empty/null, return other lst ;
  ; if head of lst1 is not in lst2, add to the difference of the tail of lst1 ;
  ; else, call difference with tail of lst1 ;
  (cond ((null? lst1) '())
        ((not (member? (car lst1) lst2)) (cons (car lst1) (difference (cdr lst1) lst2)))
        (else (difference (cdr lst1) lst2))))

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))
(let loop ()
  (define line (read-line (current-input-port) 'any))
  (if (eof-object? line)
      (display "")
      (begin (print (eval (read (open-input-string line)) ns)) (newline) (loop))))