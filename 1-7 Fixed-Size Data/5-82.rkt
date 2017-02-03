;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 5-82) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct letters [first second third])
; a letters is a structure:
; (make-letters 1String 1String 1String)


; A Word is one of:
; - letters
; - #false


(define a (make-letters "a" "b" "c"))
(define b (make-letters "a" "b" "d"))
(define c (make-letters "a" "b" "c"))
(define d #false)

; letters letters -> Boolean
; compares two letters, if they're the same
; yield #true, else #false
(define (compare-letters l1 l2)
  (and (string=? (letters-first l1) (letters-first l2))
           (string=? (letters-second l1) (letters-second l2))
           (string=? (letters-third l1) (letters-third l2))))
      
; Word Word-> Word
; compares two words, if they agree,
; yield one of them, else yield #false
(define (compare-word1 w1 w2)
  (if (and (letters? w1)
           (letters? w2)
           (compare-letters w1 w2))
      w1
      #false))

(define (compare-word2 w1 w2)
  (and (letters? w1) (equal? w1 w2)))