;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname drawing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BRENDAN ZHANG (20720995)
;; CS135 Fall 2017
;; Assignment 05, Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ask Racket to give us access to the data definitions and functions in
;; the file drawinglib.rkt.
(require "drawinglib.rkt")

;; A demonstration of the drawing in the assignment.
(define samplepic (list
                   (make-square135 (make-posn 0 0) 50 '(255 0 0))
                   (make-square135 (make-posn 50 50) 50 '(0 0 255))
                   (make-circle135 (make-posn 50 50) 25 '(0 255 0))))

;; Type this line into the interactions window to see the picture:
;; (draw-picture samplepic 100 100)

;; --------------------------
;; Part A
;; --------------------------

(define example-drawing
  (list
   (make-square135 (make-posn 0 0) 80 '(255 0 0))
   (make-square135 (make-posn 220 0) 80 '(255 0 0))
   (make-circle135 (make-posn 150 150) 120 '(0 0 0))
   (make-circle135 (make-posn 100 130) 40 '(255 0 0))
   (make-circle135 (make-posn 200 130) 40 '(255 0 0))
   (make-square135 (make-posn 125 175) 50 '(243 213 0))
   (make-circle135 (make-posn 120 140) 25 '(0 0 0))
   (make-circle135 (make-posn 220 140) 25 '(0 0 0))))

;;(draw-picture example-drawing 300 300)

;; --------------------------
;; Part B
;; --------------------------

;;(cull my-draw m n) consumes a Drawing (my-draw) and two natural numbers (m and n)
;; and produces a new Drawing containing the first m squares and first n circles
;; from the consumed Drawing
;;cull: Drawing Nat Nat -> Drawing
;;Examples
(check-expect (cull example-drawing 0 0) empty)
(check-expect (cull empty 1 1) empty)
(check-expect (cull example-drawing 2 0)
              (list    (make-square135 (make-posn 0 0) 80 '(255 0 0))
                       (make-square135 (make-posn 220 0) 80 '(255 0 0))))

(define (cull my-draw m n)
  (cond
    [(and (<= m 0) (<= n 0)) empty]
    [(empty? my-draw) empty]
    [(and (> m 0) (square135? (first my-draw)))
     (cons (first my-draw) (cull (rest my-draw) (sub1 m) n))]
    [(and (> n 0) (circle135? (first my-draw)))
     (cons (first my-draw) (cull (rest my-draw) m (sub1 n)))]
    [else (cull (rest my-draw) m n)]))

;;Tests:
(check-expect (cull example-drawing 10 10)
              (list
               (make-square135 (make-posn 0 0) 80 '(255 0 0))
               (make-square135 (make-posn 220 0) 80 '(255 0 0))
               (make-circle135 (make-posn 150 150) 120 '(0 0 0))
               (make-circle135 (make-posn 100 130) 40 '(255 0 0))
               (make-circle135 (make-posn 200 130) 40 '(255 0 0))
               (make-square135 (make-posn 125 175) 50 '(243 213 0))
               (make-circle135 (make-posn 120 140) 25 '(0 0 0))
               (make-circle135 (make-posn 220 140) 25 '(0 0 0))))
(check-expect (cull example-drawing 2 2)
              (list (make-square135 (make-posn 0 0) 80 '(255 0 0))
                    (make-square135 (make-posn 220 0) 80 '(255 0 0))
                    (make-circle135 (make-posn 150 150) 120 '(0 0 0))
                    (make-circle135 (make-posn 100 130) 40 '(255 0 0))))


;; --------------------------
;; Part C
;; --------------------------

;;(draw-rings width n) consumes the width and a natural number n and produces
;; a bullseye Drawing based on the number and width of the rings
;;draw-rings: Nat Nat -> Drawing

(define (draw-rings width n)
  (cond
    [(= 0 n) empty]
    [(even? n)
     (cons (make-circle135
            (make-posn 100 100) (* n (/ width 2)) '(255 0 0))
           (draw-rings width (sub1 n)))]
    [else
     (cons (make-circle135
            (make-posn 100 100) (* n (/ width 2)) '(255 255 255))
           (draw-rings width (sub1 n)))]))


;;(bullseye n) consumes a natural number n and produces a bullseye Drawing with
;; an n number of rings
;;bullseye: Nat -> Drawing

(define (bullseye n)
  (cond
    [(= 0 n) empty]
    [else (draw-rings (/ 200 n) n)]))

;;(draw-picture (bullseye 6) 200 200)

;; --------------------------
;; Part D
;; --------------------------

;;(draw-row row column r-count c1 c2) consumes three natural numbers (row, column and
;; r-count) and two Colours (c1 and c2) and produces a Drawing of alternating
;; Circle135s and Square135s each with a corresponding Colour
;;draw-row: Nat Nat Nat Colour Colour -> Drawing

(define (draw-row row column r-count c1 c2)
  (cond
    [(= r-count row) empty]
    [(even? (+ column r-count))
     (cons (make-square135 (make-posn (* r-count 10) (* column 10)) 10 c1)
           (draw-row row column (add1 r-count) c1 c2))]
    [else
     (cons (make-circle135 (make-posn (+ (* r-count 10) 5) (+ 5 (* column 10))) 5 c2)
           (draw-row row column (add1 r-count) c1 c2))]))


;;(draw-column row column c-count c1 c2) consumes three natural numbers (row, column
;; and c-count) and two Colours (c1 and c2) and produces a Drawing with a column number
;; of rows of alternating Circle135s and Square135s each with a corresponding Colour
;;draw-column: Nat Nat Nat Colour Colour -> Drawing

(define (draw-column row column c-count c1 c2)
  (cond
    [(= c-count column) empty]
    [else (append (draw-row row c-count 0 c1 c2)
                (draw-column row column (add1 c-count) c1 c2))]))


;;(checkerboard n c1 c2) consumes a natural number n and two Colours (c1 and c2)
;; and produces a checkerboard Drawing with alternating Circle135s and Square135s
;; each with a corresponding Colour
;;checkerboard: Nat Colour Colour -> Drawing

(define (checkerboard n c1 c2)
  (draw-column n n 0 c1 c2))


;;(draw-picture (checkerboard 50 '(0 0 255) '(0 0 0)) 300 300)
