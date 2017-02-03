;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 4-59) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define RADIUS 50)

(define BOARD (rectangle (* 10 RADIUS) (* 3.5 RADIUS)
               "outline" "black"))
(define R-RING (circle RADIUS "outline" "red"))
(define Y-RING (circle RADIUS "outline" "yellow"))
(define G-RING (circle RADIUS "outline" "green"))
(define SPACE (rectangle RADIUS RADIUS "solid" "white"))

;BACKGROUND
(define BACKG (overlay (beside R-RING SPACE Y-RING SPACE G-RING) BOARD))

; TrafficLight -> Image
; renders a light bulb according to the TrafficLight tl
(define (draw/bulb tl)
  (circle RADIUS "solid" tl))

; TrafficLight -> Image
; renders the current state cs as an image
(define (tl-render cs)
  (place-image (draw/bulb cs)
               (* RADIUS
                  (cond
                    [(string=? "red" cs) 2]
                    [(string=? "yellow" cs) 5]
                    [(string=? "green" cs) 8]))
               (/ (image-height BACKG) 2)
               BACKG))

; TrafficLight -> TrafficLight
; yields the next state given current state cs
(define (tl-next cs)
  (cond
    [(string=? "red" cs) "green"]
    [(string=? "green" cs) "yellow"]
    [(string=? "yellow" cs) "red"]))

; TrafficLight -> TrafficLight
; simulates a clock-based American traffic light
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
            [to-draw tl-render]
            [on-tick tl-next 1]))