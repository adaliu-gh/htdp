;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 10-167-168-169) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Lop (short for List of Posns) is one of:
; - '()
; - (cons Posn Lop)
; interpretation an instance of Lop is a list of Posns
(define lop1 '())
(define lop2 (cons (make-posn 23 4) lop1))
(define lop3 (cons (make-posn 23 8) lop2))

; Lop -> Number
; computes the sum of all of x-coordinates of posns int he lop
(check-expect (sum lop1) 0)
(check-expect (sum lop2) 23)
(check-expect (sum lop3) 46)
(define (sum lop)
  (cond
    [(empty? lop) 0]
    [else (+ (posn-x (first lop))
             (sum (rest lop)))]) )

; Lop -> Lop
; produces a new list of Posns where every y-coordinate has been increased 1 f
(check-expect (translate lop1) '())
(check-expect (translate lop2) (cons (make-posn 23 5) '()))
(check-expect (translate lop3) (cons (make-posn 23 9) (cons (make-posn 23 5) '())))
(define (translate lop)
  (cond
    [(empty? lop) '()]
    [else (cons (make-posn (posn-x (first lop)) (+ 1 (posn-y (first lop))))
                (translate (rest lop)))]))

; Posn -> Boolean
; produces #true if the x-coordinate is between 0 and 100
; and y-coordinate is between 0 and 200
(check-expect (legal? (make-posn 300 100)) #false)
(check-expect (legal? (make-posn 100 300)) #false)
(check-expect (legal? (make-posn 100 100)) #true)
(define (legal? p)
  (and (<= 0 (posn-x p) 100)
       (<= 0 (posn-y p) 200)))

; Lop -> Lop
; only keep those Posns whose x-coordinates are between 0 and 100 and
; y-coordinates are between 0 and 200
(check-expect (legal (cons (make-posn 300 300) (cons (make-posn 34 90) '())))
              (cons (make-posn 34 90) '()))
(define (legal lop)
  (cond
    [(empty? lop) '()]
    [else (if (legal? (first lop))
              (cons (first lop) (legal (rest lop)))
              (legal (rest lop)))]))

