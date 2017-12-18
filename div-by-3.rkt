;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname div-by-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;**************************
;;  Brendan Zhang (20720995)
;;  CS 135 Fall 2017
;;  Assignment 04, Problem 3  
;;**************************
;;


;;Part A

;;A Nat3 is one of:
;;*0
;;*1
;;*2
;;*(+ 3 Nat3)


;;Part B

;;my-nat3-fn: Nat3 -> Any
(define (my-nat3-fn n)
  (cond
    [(or (zero? n) (= 1 n) (= 2 n))...]
    [else (... (my-nat-fn (- n 3))...)]))


;;Part C

;;(div-by-3? nat3) consumes a Nat3 and produces true if it is divisible
;; by three and false otherwise
;;div-by-3?: Nat3 -> Bool
;;Examples:
(check-expect (div-by-3? 3) true)

(define (div-by-3? nat3)
  (cond
    [(zero? nat3) true]
    [(or (= 1 nat3) (= 2 nat3)) false]
    [else (div-by-3? (- nat3 3))]))

;;Tests:
(check-expect (div-by-3? 0) true)
(check-expect (div-by-3? 1) false)
(check-expect (div-by-3? 2) false)
(check-expect (div-by-3? 7) false)
(check-expect (div-by-3? 9) true)


