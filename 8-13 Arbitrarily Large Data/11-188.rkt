;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 11-188) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct email [from date message])
; A Email Message is a structure:
;   (make-email String Number String)
; interpretation (make-email f d m) represents text m
; sent by f, d seconds after the beginning of time
(define e1 (make-email "user1" 12 "hello"))
(define e2 (make-email "user2" 22 "hello"))

; a Loe (short for list of emails) is one of:
; - '()
; - (cons email Loe)
(define l1 (list e1))
(define l2 (list e2))
(define l3 (list e1 e2))
(define l4 (list e2 e1))

; Loe -> Loe
; sorts list of emails by name
(check-satisfied (sort> l1) sort>?)
(check-satisfied (sort> l2) sort>?)
(check-satisfied (sort> l3) sort>?)
(check-satisfied (sort> l4) sort>?)
(define (sort> l)
  (cond
    [(empty? l) '()]
    [else (insert (first l)
                  (sort> (rest l)))]))

; email Loe -> Loe
; inserts an email into a sorted list of emails
(check-expect (insert e1 '()) l1)
(check-expect (insert e1 l2) l4)
(define (insert e l)
  (cond
    [(empty? l) (list e)]
    [else (if (>= (email-date e)
                  (email-date (first l)))
              (cons e l)
              (cons (first l)
                    (insert e (rest l))))]))

; Loe -> Boolean
; determines if a list of emails is in a descending order by name
(check-expect (sort>? '()) #true)
(check-expect (sort>? l1) #true)
(check-expect (sort>? l3) #false)
(check-expect (sort>? l4) #true)
(define (sort>? l)
  (cond
    [(empty? l) #true]
    [(empty? (rest l)) #true]
    [else (and (> (email-date (first l))
                  (email-date (first (rest l))))
               (sort>? (rest l)))]))
