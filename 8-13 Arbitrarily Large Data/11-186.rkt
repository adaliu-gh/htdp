;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 11-186) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List-of-numbers -> Boolean
; determines if a list of numbers is sorted in a descending order
(check-expect (sort>? '()) #true)
(check-expect (sort>? (list 12)) #true)
(check-expect (sort>? (list 2 3)) #false)
(check-expect (sort>? (list 3 2)) #true)
(define (sort>? l)
  (cond
    [(empty? l) #true]
    [(empty? (rest l)) #true]
    [else (and (> (first l) (first (rest l)))
               (sort>? (rest l)))]))

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(check-satisfied (sort> '()) sort>?)
(check-satisfied (sort> (list 2)) sort>?)
(check-satisfied (sort> (list 3 2)) sort>?)
(define (sort> l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort> (rest l)))]))

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (>= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(check-satisfied (sort>/bad (list 2 3)) sort>?)
(define (sort>/bad l)
  '(9 8 7 6 5 4 3 2 1 0))
