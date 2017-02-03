;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 3-47) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; scene
(define FRAME
  (rectangle 10 100 "outline" "black"))
(define BAR
  (rectangle 10 100 "solid" "red"))

; Happiness -> Image
; places the happiness bar by hp pixels from
; the bottom of the frame (to the top of the bar)
(define (render hp)
  (place-image BAR
               5
               (- 150 hp)
               FRAME))

; Happiness -> Happiness
; decreases the score by 0.1 for every clock tick
; but never falls below 0
; given 1.2, expected 1.1
(define (tock hp)
  (if (< hp 0.1)
      0
      (- hp 0.1)))

; Happiness String -> Happiness
; 