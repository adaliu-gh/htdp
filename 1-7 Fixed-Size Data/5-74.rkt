;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 5-74) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define MTS (empty-scene 100 100))
(define DOT (circle 3 "solid" "red"))

; A Posn represents the state of the world.

; Posn -> Image
; adds a red spot to MTS at p
(define (scene+dot ps)
  (place-image DOT (posn-x ps) (posn-y ps) MTS))

; Posn -> Posn
; increases the x-coordinate of p by 3 for every clock tick
(define (x+ p)
  (make-posn (+ 3 (posn-x p)) (posn-y p)))

; Posn Number Number MouseEvent -> Posn
; for mouse clicks, (make-posn x y); otherwise p
(define (reset-dot p x y me)
  (if (mouse=? me "button-down")
      (make-posn x y)
      p))

; Posn -> Posn
(define (main p0)
  (big-bang p0
            [on-tick x+]
            [on-mouse reset-dot]
            [to-draw scene+dot]))