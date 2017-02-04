;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 11-189) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number List-of-numbers -> Boolean
(define (search n alon)
  (cond
    [(empty? alon) #false]
    [else (or (= (first alon) n)
              (search n (rest alon)))]))

; Number List-of-numbers -> Boolean
; determines if a number exists in a sorted list
(check-expect (search-sorted 1 '()) #false)
(check-expect (search-sorted 1 (list  3 2)) #false)
(check-expect (search-sorted 1 (list 3 1)) #true)
(check-expect (search-sorted 1 (list 0 -1)) #false)
(check-expect (search-sorted 1 (list 1)) #true)
(define (search-sorted n alon)
  (cond
    [(empty? alon) #false]
    [(> n (first alon)) #false]
    [else (or (= (first alon) n)
              (search n (rest alon)))]))
