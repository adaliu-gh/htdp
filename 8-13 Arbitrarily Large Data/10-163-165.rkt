;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 10-163-165) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number -> Number
; converts a Fahrenheit to a Celsius
(define (f2c f)
  (/ (- f 32) 1.8))

; List-of-numbers -> List-of-numbers
; converts a list of Fahrenheit to a list of Celsius
(define (f2c* fl)
  (cond
    [(empty? fl) '()]
    [else (cons (f2c (first fl)) (f2c* (rest fl)))]))

; List-of-strings -> List-of-strings
; substitute all "robot" in old with "r2d2"
(check-expect (subst-robot (cons "robot" '())) (cons "r2d2" '()))
(define (subst-robot old)
  (cond
    [(empty? old) '()]
    [else (cons (if (string=? (first old) "robot")
                    "r2d2"
                    (first old))(subst-robot (rest old)))]))
