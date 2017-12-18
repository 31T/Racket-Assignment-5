;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;**************************
;;  Brendan Zhang (20720995)
;;  CS 135 Fall 2017
;;  Assignment 04, Problem 2  
;;**************************
;;


;;Part A

;;A list of numbers, (listof Num), is one of:
;;*empty
;;*(cons Num (listof Num))

;;(sum-positive lon) consumes a list of numbers (lon) and produces the
;; sum of all positive numbers in the list
;;sum-positive: (listof Num) -> Nat
;;Examples:
(check-expect (sum-positive (cons 2 (cons 3 empty))) 5)

(define (sum-positive lon)
  (cond
    [(empty? lon) 0]
    [(> 0 (first lon)) (sum-positive (rest lon))]
    [else (+ (first lon) (sum-positive (rest lon)))]))

;;Tests:
(check-expect (sum-positive empty) 0)
(check-expect (sum-positive (cons 2 (cons -3 (cons 3 empty)))) 5)


;;Part B

;;A list of any, (listof Any), is one of:
;;*empty
;;*(cons Any (listof Any))

;;(contains? elem loa) consumes a value (elem) and a (listof Any) and produces
;; true if the elem is in the (listof Any) and false if it is not
;;contains?: Any (listof Any) -> Bool
;;Examples:
(check-expect (contains? 4 (cons 4 (cons 'Salmon empty))) true)

(define (contains? elem loa)
  (cond
    [(empty? loa) false]
    [(equal? elem (first loa)) true]
    [else (contains? elem (rest loa))]))

;;Tests:
(check-expect (contains? 4 empty) false)
(check-expect (contains? 'Salmon (cons 4 (cons 'Salmon empty))) true)


;;Part C

;;(elem-has-duplicate? elem-loa loa) consumes a value (elem-loa)
;; and a (listof Any) called loa and produces true if that value appears
;; more than once in the list
;;elem-has-duplicate?: Any (listof Any) -> Bool
;;Examples:
(check-expect (elem-has-duplicate? 2 (cons 2 (cons 2 empty))) true)
(check-expect (elem-has-duplicate? 2 empty) false)
(check-expect (elem-has-duplicate? 2 (cons 2 empty)) false)

(define (elem-has-duplicate? elem-loa loa)
  (cond
    [(empty? loa) false]
    [(empty? (rest loa)) false] 
    [(equal? elem-loa (first (rest loa))) true]
    [else (elem-has-duplicate? (first loa) (rest loa))]))


;;(has-duplicate? loa) consumes a (listof Any) loa and produces true
;; if any element in the list appears more than once and false otherwise
;;has-duplicate?: (listof Any) -> Bool
;;Examples:
(check-expect (has-duplicate? (cons 4 (cons 4 empty))) true)

(define (has-duplicate? loa)
  (cond
    [(empty? loa) false]
    [(empty? (rest loa)) false]
    [(elem-has-duplicate? (first loa) loa) true]
    [else (has-duplicate? (rest loa))]))

;;Tests:
(check-expect (has-duplicate? (cons 4 empty)) false)
(check-expect (has-duplicate? empty) false)
(check-expect (has-duplicate? (cons 3 (cons 2 (cons 4 (cons 1 empty))))) false)


;;Part D

;;A list of integers, (listof Int), is one of:
;;*empty
;;*(cons Int (listof Int))

;;(keep-ints loa) consumes a (listof Any) loa and produces a (listof Int) with all of
;; the integers elements in loa
;;keep-ints: (listof Any) -> (listof Int) 
;;Examples:
(check-expect (keep-ints (cons 4 (cons 'Hi empty))) (cons 4 empty))

(define (keep-ints loa)
  (cond
    [(empty? loa) empty] 
    [(integer? (first loa)) (cons (first loa) (keep-ints (rest loa)))]
    [else (keep-ints (rest loa))]))

;;Tests:
(check-expect (keep-ints (cons 7 (cons 'Hello empty))) (cons 7 empty))
(check-expect (keep-ints empty) empty)

