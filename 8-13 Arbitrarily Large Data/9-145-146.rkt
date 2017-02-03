;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9-145) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; a NEList-of-temperatures is one of:
; - (cons CTemperature '())
; - (cons CTemperature NEList-of-temperatures)
(define t1 (cons -273 '()))
(define t2 (cons 1 (cons 2 (cons 3 '()))))
(define t3 (cons 3 (cons 2 (cons 1 '()))))

; NEList-of-temperature -> Number
; computes the average of all temperatures
(check-expect (average t1) -273)
(check-expect (average t2) 2)
(check-expect (average t3) 2)
(define (average l)
  (/ (sum l) (how-many l)))

; NEList-of-temperatures -> Number
; computes the sum of all the temperatures
(check-expect (sum t1) -273)
(check-expect (sum t2) 6)
(check-expect (sum t3) 6)
(define (sum l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (+ (first l) (sum (rest l)))]))

; NEList-of-temperatures -> Number
; counts the temperatures on the list
(check-expect (how-many t1) 1)
(check-expect (how-many t2) 3)
(check-expect (how-many t3) 3)
(define (how-many l)
  (cond
    [(empty? (rest l)) 1]
    [else
     (+ 1 (how-many (rest l)))]))

; NEList-of-temperature -> Boolean
; return #true if the temperatures are sorted in descending order
; otherwise return #false
(check-expect (sorted>? t1) #true)
(check-expect (sorted>? t2) #false)
(check-expect (sorted>? t3) #true)
(define (sorted>? ne-l)
  (cond
    [(empty? (rest ne-l)) #true]
    [else (and (>= (first ne-l) (first (rest ne-l)))
               (sorted>? (rest ne-l)))]))