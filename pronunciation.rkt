;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname pronunciation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BRENDAN ZHANG (20720995)
;; CS135 Fall 2017
;; Assignment 05, Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ask Racket to give us access to the code in this file.
;; Do not remove this line.
(require "pronunciationlib.rkt")

;; The data definitions as given in the question.

;; A Vowel is a (list Sym (anyof 0 1 2))

;; A Phoneme is an (anyof Sym Vowel)

;; A Pronunciation is a (listof Phoneme)
;; requires: the list contains exactly one vowel with a stress of 1

;; A Dictionary is a (listof (list Str Pronunciation))
;; requires: The strings in each sub-list appear in alphabetical
;;           order in the Dictionary.

;; --------------------------
;; My Functions Here
;; --------------------------

(define actress-pron (list (list 'AE 1) 'K 'T 'R (list 'AH 0) 'S))

(define ugly-pron (list (list 'AH 1) 'G 'L '(list IY 0)))

;;Part A

;;(count-syllables lst1) consumes a Pronunciation (lst1) and
;; produces the number of syllables in lst1
;;count-syllables: Pronunciation -> Nat
;;Examples:
(check-expect (count-syllables actress-pron) 2)
(check-expect (count-syllables empty) 0)

(define (count-syllables lst1)
  (cond
    [(empty? lst1) 0]
    [(cons? (first lst1)) (+ 1 (count-syllables (rest lst1)))]
    [else (count-syllables (rest lst1))]))


;;(num-syllables mystr dic) consumes a string (mystr) and a Dictionary (dic)
;; and produces the number of syllables in mystr
;;num-syllables: Str Dictionary -> Nat
;;Examples:
(check-expect (num-syllables "dirt" toy-dictionary) 1)
(check-expect (num-syllables "hello" empty) 0)

(define (num-syllables mystr dic)
  (cond
    [(empty? dic) 0]
    [(string=? mystr (first (first dic)))
     (count-syllables (first (rest (first dic))))]
    [else (num-syllables mystr (rest dic))]))

;;Tests:
(check-expect (num-syllables "ugly" toy-dictionary) 2)
(check-expect (num-syllables "dirt" toy-dictionary) 1)


;;Part B

;;(stress-list lst1) consumes a Pronunciation (lst1) and produces the
;; stress pattern as a list of numbers
;;stress-list: Pronunciation -> (listof (anyof 0 1 2))
;;Examples:
(check-expect (stress-list actress-pron) (list 1 0))
(check-expect (stress-list empty) empty)

(define (stress-list lst1)
  (cond
    [(empty? lst1) empty]
    [(cons? (first lst1))
     (cons (first (rest (first lst1))) (stress-list (rest lst1)))]
    [else (stress-list (rest lst1))]))


;;(find-stress-pattern) consumes a (listof (anyof 0 1 2)) called lon and a
;; Dictionary (dic) and produces all words in the Dictionary with the same stress
;; pattern
;;find-stress-pattern: (listof (anyof 0 1 2)) Dictionary -> (listof Str)
;;Examples:
(check-expect (find-stress-pattern (list 0 1) toy-dictionary)
              (list "adopt" "concrete" "deprive" "describe" "petite"))
(check-expect (find-stress-pattern (list 0 1) empty) empty)

(define (find-stress-pattern lon dic)
  (cond
    [(empty? dic) empty]
    [(equal? lon (stress-list (first (rest (first dic)))))
     (cons (first (first dic)) (find-stress-pattern lon (rest dic)))]
    [else (find-stress-pattern lon (rest dic))]))

;;Tests:
(check-expect (find-stress-pattern (list 1 0) toy-dictionary)
              (list "actress" "awful" "billion" "smugly" "ugly"))


;;Part C

;;(rhyme-suffix lst1) consumes a Pronunciation (lst1) and produces the
;; suffix of the Pronunciation
;;rhyme-suffix: Pronunciation -> Pronunciation
;;Examples:
(check-expect (rhyme-suffix (list (list 'AE 1) 'K 'T 'R (list 'AH 0) 'S))
              (list (list 'AE 1) 'K 'T 'R (list 'AH 0) 'S))
(check-expect (rhyme-suffix empty) empty)

(define (rhyme-suffix lst1)
  (cond
    [(empty? lst1) empty]
    [(symbol? (first lst1)) (rhyme-suffix (rest lst1))]
    [(= 1 (first (rest (first lst1)))) lst1]
    [else (rhyme-suffix (rest lst1))]))


;;(my-pronunciation mystr dic) consumes a string (mystr) and a Dictionary (dic)
;; and produces the Pronunciation of the consumed string
;;my-pronunciation: Str Dictionary -> Pronunciation
;;Examples:
(check-expect (my-pronunciation "actress" toy-dictionary)
              (list (list 'AE 1) 'K 'T 'R (list 'AH 0) 'S))
(check-expect (my-pronunciation "actress" empty) empty)

(define (my-pronunciation mystr dic)
  (cond
    [(empty? dic) empty]
    [(string=? mystr (first (first dic)))
     (first (rest (first dic)))]
    [else (my-pronunciation mystr (rest dic))]))


;;(list-of-rhymes mystr new-dic suffix) consumes a string (mystr) and a Dictionary
;; (new-dic) and a Pronunciation (suffix) and produces a list of words in the Dictionary
;; that rhyme with but do not include the consumed string
;;list-of-rhymes: Str Dictionary Pronunciation -> (listof Str)
;;Examples:
(check-expect (list-of-rhymes "dirt" toy-dictionary (list (list 'ER 1) 'T)) (list "blurt"))
(check-expect (list-of-rhymes "dirt" empty empty) empty)

(define (list-of-rhymes mystr new-dic suffix)
  (cond
    [(empty? new-dic) empty]
    [(string=? mystr (first (first new-dic))) (list-of-rhymes mystr (rest new-dic) suffix)]
    [(equal? suffix
             (rhyme-suffix (first (rest (first new-dic)))))
     (cons (first (first new-dic)) (list-of-rhymes mystr (rest new-dic) suffix))]
    [else (list-of-rhymes mystr (rest new-dic) suffix)]))


;;(find-rhymes mystr dic) consumes a string (mystr) and a Dictionary (dic) and
;; produces a list of words in the Dictionary that rhyme with but do not include
;; the consumed string
;;find-rhymes: Str Dictionary -> (listof Str)
;;Examples:
(check-expect (find-rhymes "smugly" toy-dictionary) (list "ugly"))
(check-expect (find-rhymes "smugly" empty) empty)

(define (find-rhymes mystr dic)
  (list-of-rhymes mystr dic (rhyme-suffix (my-pronunciation mystr dic))))

;;Tests:
(check-expect (find-rhymes "ugly" toy-dictionary) (list "smugly"))
(check-expect (find-rhymes "dirt" toy-dictionary) (list "blurt"))

