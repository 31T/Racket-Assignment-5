;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname settheory) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BRENDAN ZHANG (20720995)
;; CS135 Fall 2017
;; Assignment 05, Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A NumSet is a (listof Num)
;;requires: the numbers are strictly increasing

(define list-a (list 1 2 3 4 5 6))

(define list-b (list 4 5 6 7 8 9))

(define list-c (list 2 3 4 5))

;;Part A

;;(union numset1 numset2) consumes two NumSets (numset1 numset2) and produces
;; a NumSet with all of the numbers in either of the consumed NumSets
;;union: NumSet NumSet -> NumSet
;;Examples:
(check-expect (union list-a list-b) (list 1 2 3 4 5 6 7 8 9))
(check-expect (union empty empty) empty)
(check-expect (union list-a empty) list-a)
(check-expect (union empty list-b) list-b)

(define (union numset1 numset2)
  (cond
    [(empty? numset1) numset2]
    [(empty? numset2) numset1]
    [(= (first numset1) (first numset2))
     (cons (first numset1) (union (rest numset1) (rest numset2)))]
    [(< (first numset1) (first numset2))
     (cons (first numset1) (union (rest numset1) numset2))]
    [else
     (cons (first numset2) (union numset1 (rest numset2)))]))

;;Tests:
(check-expect (union list-a list-a) list-a)
(check-expect (union list-a list-c) list-a)
(check-expect (union list-b list-c) (list 2 3 4 5 6 7 8 9))


;;Part B
               
;;(intersection numset1 numset2) consumes two NumSets (numset1 and numset2) and produces
;; a new NumSet with the numbers in both of the consumes NumSets
;;intersection: NumSet NumSet -> NumSet
;;Examples:
(check-expect (intersection list-a list-b) (list 4 5 6))
(check-expect (intersection empty empty) empty)
(check-expect (intersection list-a empty) empty)
(check-expect (intersection empty list-b) empty)

(define (intersection numset1 numset2)
  (cond
    [(empty? numset1) empty]
    [(empty? numset2) empty]
    [(= (first numset1) (first numset2))
     (cons (first numset1) (intersection (rest numset1) (rest numset2)))]
    [(< (first numset1) (first numset2))
     (intersection (rest numset1) numset2)]
    [else
     (intersection numset1 (rest numset2))]))

;;Tests:
(check-expect (intersection list-b list-c) (list 4 5))
(check-expect (intersection list-a list-c) (list 2 3 4 5))


;;Part C

;;(difference numset1 numset2) consumes two NumSets (numset1 and numset2) and produces
;; a new NumSet with all of the numbers in numset1 but not numset2
;;difference: NumSet NumSet -> NumSet
;;Examples:
(check-expect (difference list-a list-b) (list 1 2 3))
(check-expect (difference empty empty) empty)
(check-expect (difference list-a empty) list-a)
(check-expect (difference empty list-b) empty)

(define (difference numset1 numset2)
  (cond
    [(empty? numset1) empty]
    [(empty? numset2) numset1]
    [(= (first numset1) (first numset2))
     (difference (rest numset1) (rest numset2))]
    [(< (first numset1) (first numset2))
     (cons (first numset1) (difference (rest numset1) numset2))]
    [else
     (difference numset1 (rest numset2))]))

;;Tests:
(check-expect (difference list-b list-a) (list 7 8 9))
(check-expect (difference list-c list-b) (list 2 3))
(check-expect (difference (list -3 -2 -1) (list -3 -2)) (list -1))


;;Part D

;;(symmetric-difference numset1 numset2) consumes two NumSets (numset1 and numset2)
;; and produces a new NumSet containing all of the numbers in one of but not both
;; consumed NumSets
;;symmetric-difference: NumSet NumSet -> NumSet
;;Examples:
(check-expect (symmetric-difference list-a list-b) (list 1 2 3 7 8 9))
(check-expect (symmetric-difference empty empty) empty)
(check-expect (symmetric-difference list-a empty) list-a)
(check-expect (symmetric-difference empty list-b) list-b)

(define (symmetric-difference numset1 numset2)
  (cond
    [(empty? numset1) numset2]
    [(empty? numset2) numset1]
    [else (difference (union numset1 numset2) (intersection numset1 numset2))]))

;;Tests:
(check-expect (symmetric-difference list-b list-c) (list 2 3 6 7 8 9))
(check-expect (symmetric-difference list-c list-a) (list 1 6))
     
     
