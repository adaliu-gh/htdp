;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9-147) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; a NEList-of-booleans is one of:
; - (cons Boolean '())
; - (cons Boolean NEList-of-booleans)
(define b1 (cons #true '()))
(define b2 (cons #false b1))
(define b3 (cons #true b1))
(define b4 (cons #false '()))
; NEList-of-booleans -> Boolean
; produces #true if all elements are #true
(check-expect (all-true b1) #true)
(check-expect (all-true b2) #false)
(check-expect (all-true b3) #true)
(check-expect (all-true b4) #false)
(define (all-true l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (and (first l) (all-true (rest l)))]))

; NEList-of-booleans -> Boolean
; produces #ture if one of the list's elements is #true
; otherwise produces #false
(check-expect (one-true b1) #true)
(check-expect (one-true b2) #true)
(check-expect (one-true b3) #true)
(check-expect (one-true b4) #false)
(define (one-true l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (or (first l) (one-true (rest l)))]))