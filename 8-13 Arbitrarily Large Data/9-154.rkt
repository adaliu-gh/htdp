;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9-154) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct layer [color doll])

; an RD (Russion doll) is one of:
; - String
; - (make-layer String RD)
(define r1 "red")
(define r2 (make-layer "green" r1))
(define r3 (make-layer "yellow" r2))

; RD -> String
; produces a string of all colors of RD
(check-expect (colors r1) "red")
(check-expect (colors r2) "green, red")
(check-expect (colors r3) "yellow, green, red")
(define (colors rd)
  (cond
    [(string? rd) rd]
    [else (string-append (layer-color rd)
                         ", "
                         (colors (layer-doll rd)))]))

; RD -> String
; produces the color of the innermost doll
(check-expect (inner r1) "red")
(check-expect (inner r2) "red")
(check-expect (inner r3) "red")
(define (inner rd)
  (cond
    [(string? rd) rd]
    [(layer? rd) (inner (layer-doll rd))]))
