;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9-140) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-booleans is one of:
; - '()
; - (cons #true List-of-booleans)
; - (cons #false List-of-booleans)
(define b1 (cons #true '()))
(define b2 (cons #false '()))
(define b3 (cons #true (cons #false '())))

; List-of-booleans -> Boolean
; return #true if all of elements of the list are #true;
; otherwise return #false
(check-expect (all-true b1) #true)
(check-expect (all-true b2) #false)
(check-expect (all-true b3) #false)
(define (all-true l)
  (cond
    [(empty? l) #true]
    [(cons? l)
     (and (first l) (all-true (rest l)))]))

; List-of-booleans -> Boolean
; return #true if one of the elements of the list is #true
; otherwise #false
(check-expect (one-true '()) #false)
(check-expect (one-true b1) #true)
(check-expect (one-true b2) #false)
(check-expect (one-true b3) #true)
(define (one-true l)
  (cond
    [(empty? l) #false]
    [(cons? l)
     (or (first l) (one-true (rest l)))]))

