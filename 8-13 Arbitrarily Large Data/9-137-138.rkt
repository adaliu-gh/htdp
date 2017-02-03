;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9-137) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-amounts is one of:
; - '()
; - (cons PositiveNumber List-of-amounts)
; Examples:
(define e1'())
(define e2 (cons 12 e1))
(define e3 (cons 12 e2))

; A List-of-numbers is one of:
; - '()
; - (cons Number List-of-numbers)
(define n1 '())
(define n2 (cons 0 n1))
(define n3 (cons -1 n2))
(define n4 (cons 1 n1))
(define n5 (cons -3 n4))


; List-of-amounts -> Number
; computes the sum of the amounts
(check-expect (sum e1) 0)
(check-expect (sum e2) 12)
(check-expect (sum e3) 24)
(define (sum l)
  (cond
    [(empty? l) 0]
    [(cons? l)
     (+ (first l) (sum (rest l)))]))


; List-of-numbers -> Boolean
; determines whether all numbers are positive numbers
(check-expect (pos? n1) #true)
(check-expect (pos? n2) #false)
(check-expect (pos? n3) #false)
(check-expect (pos? n4) #true)
(check-expect (pos? n5) #false)
(define (pos? l)
  (cond
    [(empty? l) #true]
    [(cons? l)
     (and (> (first l) 0)
          (pos? (rest l)))]))

; List-of-numbers -> Number
;produces their sum if the input also belongs to List-of-amounts;
;otherwise it signals an error
 (check-expect (checked-sum n1) 0)
 (check-expect (checked-sum n4) 1)
 (check-error (checked-sum n5))
(define (checked-sum l)
  (if (pos? l)
      (sum l)
      (error "This is not a List-of-amounts.")))