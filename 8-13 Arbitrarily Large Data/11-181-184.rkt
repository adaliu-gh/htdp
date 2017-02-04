;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 11-181-184) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; Exercise 182
(check-expect (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '()))))))
              (list 0 1 2 3 4 5))
(check-expect (cons (cons "he" (cons 0 '()))
                    (cons (cons "it" (cons 1 '()))
                          (cons (cons "lui" (cons 14 '()))
                                '())))
              (list (list "he" 0) (list "it" 1) (list "lui" 14)))
(check-expect (cons 1
                    (cons (cons 1 (cons 2 '()))
                          (cons (cons 1 (cons 2 (cons 3 '())))
                                '())))
              (list 1 (list 1 2) (list 1 2 3)))

; Exercise 183
(check-expect (list "a"  0 #false)
              (cons "a" (list 0 #false)))
(check-expect (list (list 1 13))
              (list (cons 1 (cons 13 '()))))
(check-expect (list (list 1 (list 13 '())))
              (cons (list 1 (list 13 '())) '()))
(check-expect (list '() '() (list 1))
              (list '() '() (cons 1 '())))
(check-expect (list "a" (list 1) #false '())
              (cons "a" (cons (list 1) (list #false '()))))

; Exercise 184
(check-expect (list #false #false)
              (list (string=? "a" "b") #false))
(check-expect (list 30 200 1/2)
              (list (+ 10 20) (* 10 20) (/ 10 20)))
(check-expect (list "dana" "jane" "mary" "laura")
              (list "dana" "jane" "mary" "laura"))
