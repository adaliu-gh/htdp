;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9-152-153) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; a N is one of:
; - 0
; - (add1 N)
; interpretation represents the counting numbers

; a List-Posns is one of:
; - '()
; - (cons Posn List-Posn)
; interpretation represents a list of Posns
(define p1 '())
(define p2 (cons (make-posn 12 34) p1))
(define p3 (cons (make-posn 50 150) p2))

(define cell (rectangle 10 10 "outline" "black"))

; N Image -> Image
; produces a column - a vertical arrangement - of n copies of imge
(define (col n img)
  (cond
    [(zero? n) (error "n must be positive")]
    [(= 1 n) img]
    [else (beside (col (sub1 n) img) img)]))

; N Image -> Image
; produces a row-a horizontal arrangment-of n copies of img
(define (row n img)
  (cond
    [(zero? n) (error "n must be positive")]
    [(= 1 n) img]
    [else (above (row (sub1 n) img) img)]))

(define HALL (place-image (row 18 (col 8 cell))
                          40 90
                          (empty-scene 80 180)))
(define DOT (circle 3 "solid" "red"))

; List-Posns -> Image
; produces a image with red dots scattered on Hall-image
(define (add-balloons l)
  (cond
    [(empty? l) HALL]
    [else
     (place-image DOT
                  (posn-x (first l))
                  (posn-y (first l))
                  (add-balloons (rest l)))]))