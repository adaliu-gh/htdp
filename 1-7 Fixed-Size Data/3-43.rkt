;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 3-42) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)



;car
(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 4))
(define WHEEL (circle WHEEL-RADIUS "solid" "black"))
(define BOTH-WHEELS
  (overlay/xy WHEEL
              (* 4 WHEEL-RADIUS)
              0
              WHEEL))
(define CAR-MIDDLE
  (rectangle WHEEL-DISTANCE
             (* 3 WHEEL-RADIUS) "solid" "red"))
(define CAR-BOTTOM
  (rectangle (* 8 WHEEL-RADIUS)
             (* 2 WHEEL-RADIUS)
             "solid" "red"))
(define CAR-BODY
  (overlay/xy CAR-MIDDLE
           (- (* 2 WHEEL-RADIUS))
           WHEEL-RADIUS
           CAR-BOTTOM))
(define CAR
  (overlay/xy BOTH-WHEELS
              (- WHEEL-RADIUS)
              (- (* WHEEL-RADIUS 2))
              CAR-BODY))


;background
(define TREE
  (underlay/xy (circle WHEEL-RADIUS "solid" "green")
               (* 9/10 WHEEL-RADIUS)
               (* 3/2 WHEEL-RADIUS)
               (rectangle
                (/ WHEEL-RADIUS 5) (* 2 WHEEL-RADIUS) "solid" "brown")))
(define LAND
  (rectangle (* 50 WHEEL-RADIUS)
             (* 5 WHEEL-RADIUS)
             "outline" "black"))
(define BACKGROUND
  (overlay/xy TREE
              (- (* 2/3 (image-width LAND)))
              (- (- (image-height LAND) (image-height TREE)))
              LAND))
          

; speed
(define SPEED 3)

; Number -> Number
; obtains the x-distance between the car
; and the left margin of the background,
; given the clock ticks T
; given 3, expected (* 3 SPEED)
(define (x-distance t)
  (* SPEED t))

; Number -> Number
; obtains the y-distance between the car
; and the top margin of the background,
; given the clock ticks T
(define (y-distance t)
  (- (/ (image-height BACKGROUND) 2)
     (sin t)))

; A AnimationState is a Number.
; interpretation the number of clock ticks
; since the animation started

; WorldState -> Image
; places the image of the car ws pixels from
; the left margin of the BACKGROUND image
(define (render t)
  (place-image CAR (x-distance t) (y-distance t) BACKGROUND))

; key-stroke-handler
; AnimationState String -> WorldState
; for each key stroke, re-initiate the world to 0
(define (stroke t ke)
  0)

; AnimationState -> AnimationState
; obtains the next AnimationState for every clock tick
; given 34, expected 35
(define (tock t)
  (+ 1 t))

; AnimationState -> Boolean
; when the car has disappeared on the right side
; stop the animation
(define (end? t)
  (> (- (x-distance t) (/ (image-width CAR) 2))
     (image-width BACKGROUND)))


; main
; AnimationState -> AnimationState
; launches the program from some initial state
(define (main t)
  (big-bang t
            [on-tick tock]
            [to-draw render]
            [on-key stroke]
            [stop-when end?]))