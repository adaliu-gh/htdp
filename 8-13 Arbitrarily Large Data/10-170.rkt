;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 10-170) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct phone [area switch four])
; A Phone is a structure:
; (make-phone Three Three Four)
; A Three is a Number between 100 and 999
; A Four is a Number between 1000 and 9999
(define p1 (make-phone 713 234 5678))
(define p2 (make-phone 234 123 1234))

; Lop (short for list of Phones) is one of:
; - '()
; - (cons Phone Lop)
(define l1 (cons p1 '()))
(define l2 (cons p2 l1))

; Lop -> Lop
; replaces all occurrence of area code 713 with 281
(check-expect (replace l1) (cons (make-phone 281 234 5678) '()))
(check-expect (replace l2) (cons p2 (cons (make-phone 281 234 5678) '())))
(define (replace lop)
  (cond
    [(empty? lop) '()]
    [else (cons (make-phone (if (= 713 (phone-area (first lop)))
                                281
                                (phone-area (first lop)))
                            (phone-switch (first lop))
                            (phone-four (first lop)))
                (replace (rest lop)))]))
