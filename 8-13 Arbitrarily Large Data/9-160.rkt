;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9-160) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List-of-string String -> N
; determines how often s occurs in los
(define (count los s)
  (cond
    [(empty? los) 0]
    [else
     (+ (if (string=? s (first los))
         1
         0)
         (count (rest los) s))]))

(define set123.v1 (cons 1 (cons 2 ( cons 3 '()))))
(define set123.v2 (cons 1 (cons 3 ( cons 2 '()))))
(define set23.v1 (cons 2(cons 3 '())))
(define set23.v2 (cons 3 (cons 2 '())))

; SON.v1 Number -> SON.v1
; subtracts n from son
(define (set-.v1 n son)
  (remove-all n son))

; Son.v1 Number -> Son.v1
; adds n to son
(define (set+.v1 son n)
  (cons n son))

; Son.v2 Number -> Son.v2
; adds n to son
(define (set+.v2 son n)
  (if (member? n son)
      son
      (cons n son)))