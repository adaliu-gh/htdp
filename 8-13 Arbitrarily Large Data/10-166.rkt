;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 10-166) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct work [employee rate hours])
; a (piece of) Work is a structure:
; (make-work String Number Number)
; interpretation (make-work n r h) combines the employee's name
; with the pay rate r and the number of hours of work
(define w2 (make-work "Robby" 11.95 39))
(define w3 (make-work "Matthew" 12.95 45))

(define-struct pay [employee pay])
; a Pay is a structure:
; (make-pay String Number)
; interpretation (make-pay n p) combines the employee's name
; with the money s/he should be paied
(define p2 (make-pay "Robby" (* 11.95 39)))
(define p3 (make-pay "Matthew" (* 12.95 45)))

; Low (short for list of works) is one of:
; - '()
; - (cons Work Low)
; interpretation an instance of Low represents the
; hours worked for a number of employees
(define l2 (cons (make-work "Robby" 11.95 39)
                 '()))
(define  l3 (cons (make-work "Matthew" 12.95 45)
                  (cons (make-work "Robby" 11.95 39)
                        '())))

; Lop (short for list of Pays) is one of:
; - '()
; - (cons Pay Lop)
; interpretation an instance of Lop represents the
; pay checks
(define lop2 (cons p2 '()))
(define lop3 (cons p3 lop2))

; Work -> Number
; computes the wage for a given work
(check-expect (wage.v2 w2) (* 11.95 39))
(check-expect (wage.v2 w3) (* 12.95 45))
(define (wage.v2 w)
  (* (work-rate w) (work-hours w)))

; Low -> List-of-numbers
; computes the weekly wages for the given records
(check-expect (wage*.v2 l3) (cons (* 12.95 45) (cons (* 11.95 39) '())))
(define (wage*.v2 an-low)
  (cond
    [(empty? an-low) '()]
    [else (cons (wage.v2 (first an-low))
                (wage*.v2 (rest an-low)))]))

; Work -> Pay
; computes the wage for a given work
(check-expect (wage.v3 w2) p2)
(check-expect (wage.v3 w3) p3)
(define (wage.v3 w)
  (make-pay (work-employee w) (* (work-rate w) (work-hours w))))

; Low -> Lop
; computes the weekly pay check for the given records
(check-expect (wage*.v3 l2) lop2)
(check-expect (wage*.v3 l3) lop3)
(define (wage*.v3 low)
  (cond
    [(empty? low) '()]
    [else (cons (wage.v3 (first low))
                (wage*.v3 (rest low)))]))
