;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname trains) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BRENDAN ZHANG (20720995)
;; CS135 Fall 2017
;; Assignment 04, Problem 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "a04lib.rkt")


;; The unit structure is defined in a04lib.rkt.  The require
;; statement, above, is all that's needed to have it take
;; effect here.  The following comment is here just so the
;; type definitions that follow make sense.

;; (define-struct unit (type serial))

;; -------- Q4a --------------

;; A Unit-Type is one of:
;;*'L
;;*'B
;;*'T
;;*'P
;;*'C

;; A Unit is a (make-unit Unit-Type Nat)

;; A Train is one of:
;;*empty
;;*(cons Unit Train)


;; -------- Q4b --------------

;; string->train consumes a string and produces a Train. string->train works by running
;; the racket-defined function string->list on the consumed string which turns the string
;; into a list of characters (loc). It then runs the user-defined function loc->train
;; on the new list of characters (loc) and the pre-built list (default-serial-numbers).
;; This function builds a list of Units by running the user-defined function char->unit
;; on the first element of loc and first element of default-serial-numbers, which makes
;; a Unit containing first element of loc and the first element of default-serial-numbers.
;; The function loc->train runs recursively until loc or default-serial-numbers runs out
;; of list elements. If loc runs out first, then it produces a list of Units: a Train.


;; -------- Q4c --------------

;;(headed-by? train unit-type) consumes a Train and a Unit-Type and outputs
;; true if the first Unit of Train has the same Unit-Type as the one given
;; and false otherwise
;;headed-by?: Train Unit-Type -> Bool
;;Examples:
(check-expect (headed-by? (string->train "LBC") 'L) true)

(define (headed-by? train unit-type1)
  (cond
    [(empty? train) false]
    [(symbol=? (unit-type (first train)) unit-type1) true]
    [else false]))

;;Tests:
(check-expect (headed-by? (string->train "LBC") 'C) false)
(check-expect (headed-by? empty 'L) false)


;;-------- Q4d --------------

;;(caboose-in-train? train) consumes a Train and produces true if there
;; is a caboose in the Train and false otherwise
;;caboose-in-train?: Train -> Bool
;;Examples:
(check-expect (caboose-in-train? (string->train "LBC")) true)
(check-expect (caboose-in-train? empty) false)

(define (caboose-in-train? train)
  (cond
    [(empty? train) false]
    [(symbol=? (unit-type (first train)) 'C) true]
    [else (caboose-in-train? (rest train))]))


;;(ends-with-caboose? train) consumes a Train and produces true if and only if
;; there is exactly one caboose in the Train and it is in the last Unit
;; of the Train and it produces false otherwise
;;ends-with-caboose?: Train -> Bool?
;;Examples:
(check-expect (ends-with-caboose? (string->train "LBC")) true)

(define (ends-with-caboose? train)
  (cond
    [(not (caboose-in-train? train)) false]
    [(and (symbol=? (unit-type (first train)) 'C)
          (empty? (rest train)))true]
    [(symbol=? (unit-type (first train)) 'C) false]
    [else (ends-with-caboose? (rest train))]))

;;Tests:
(check-expect (ends-with-caboose? (string->train "LB")) false)
(check-expect (ends-with-caboose? (string->train "LLBBBC")) true)
(check-expect (ends-with-caboose? (string->train "CLB")) false)


;;-------- Q4e --------------

;;(remove-unit t s) consumes a train (t) and a serial number (s) and produces
;; a new Train with the Unit corresponding to the given serial number (s) removed
;;remove-unit: Train Nat -> Train
;;requires: s is a valid serial number (an element of the pre-defined list
;; default-serial-numbers)
;;Examples:
(check-expect (remove-unit (string->train "LBC") 5) (string->train "LB"))

(define (remove-unit t s)
  (cond
    [(empty? t) empty]
    [(= (unit-serial (first t)) s) (remove-unit (rest t) s)]  
    [else (cons (first t) (remove-unit (rest t) s))]))

;;Tests:
(check-expect (remove-unit empty 5) empty)
(check-expect (remove-unit (string->train "LBC") 2)
              (cons (make-unit 'B 3) (cons (make-unit 'C 5) empty)))


;;-------- Q4f --------------

;;(is-car? train) consumes a Train and outputs true if the Unit-Type of the
;; first Unit is one of the three types of cars and false otherwise
;;is-car?: Train -> Bool
;;Examples: 
(check-expect (is-car? (string->train "B")) true)
(check-expect (is-car? empty) false)

(define (is-car? train)
  (cond
    [(empty? train) false]
    [(or (symbol=? (unit-type (first train)) 'B)
         (symbol=? (unit-type (first train)) 'T)
         (symbol=? (unit-type (first train)) 'P))true]
    [else false]))


;;(proper-train-after-caboose? train) consumes a Train and produces true if
;; the Train is a "proper" Train starting from and after the first (if existing)
;;caboose and produces false otherwise
;;proper-train-after-caboose?: Train -> Bool
;;Examples:
(check-expect (proper-train-after-caboose? (string->train "CCC")) true) 

(define (proper-train-after-caboose? train)
  (cond
    [(empty? train) true]
    [(or (is-car? train)
         (symbol=? (unit-type (first train)) 'L)) false]
    [else (proper-train-after-caboose? (rest train))]))


;;(proper-train-after-car? train) consumes a Train and outputs true if
;; the Train is a "proper" Train starting from and after the first (if existing)
;; car Unit-Type and produces false otherwise
;;proper-train-after-car?: Train -> Bool
;;Examples:
(check-expect (proper-train-after-car? (string->train "BTBCC")) true)

(define (proper-train-after-car? train)
  (cond
    [(is-car? train) (proper-train-after-car? (rest train))]
    [(proper-train-after-caboose? train) true]
    [else false]))


;;(proper-train? train) consumes a Train and produces true if the Train is 
;; "proper" and false otherwise
;;proper-train?: Train -> Bool
;;Examples:
(check-expect (proper-train? (string->train "LBBC")) true)

(define (proper-train? train)
  (cond
    [(empty? train) true]
    [(symbol=? (unit-type (first train)) 'L) (proper-train? (rest train))]
    [(proper-train-after-car? train) true]
    [else false]))

;;Tests:
(check-expect (proper-train? (string->train "BBC")) true)
(check-expect (proper-train? (string->train "CBC")) false)
(check-expect (proper-train? empty) true)
(check-expect (proper-train? (string->train "BLBC")) false)
(check-expect (proper-train? (string->train "C")) true)
(check-expect (proper-train? (string->train "L")) true)
(check-expect (proper-train? (string->train "B")) true)
(check-expect (proper-train? (string->train "BLBBC")) false)

