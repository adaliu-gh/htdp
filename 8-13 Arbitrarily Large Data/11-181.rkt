;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 11-181) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 181
(check-expect (list "a" "b" "c" "d") (cons "a" (cons "b" (cons "c" (cons "d" '())))))
(check-expect (list (list 1 2)) (cons (cons 1 (cons 2 '())) '()))
(check-expect (list "a" (list 1)  #false) (cons "a" (cons (cons 1 '()) (cons #false '()))))
(check-expect (list (list "a" 2)  "hello") (cons (cons "a" (cons 2 '())) (cons "hello" '())))
(check-expect (list (list 1 2)
                    (list 2))
              (cons (cons 1 (cons 2 '()))
                    (cons (cons 2 '())
                          '())))
