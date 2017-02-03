;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 3-44) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
              
  
; the relevant position of the car
(define Y-CAR (* 3 WHEEL-RADIUS))

; speed
(define SPEED 2)


; A WorldState is a Number.
; interpretation the number of pixels between
; the left border of the scene and the car

; WorldState -> Image
; places the image of the car ws pixels from
; the left margin of the BACKGROUND image
(define (render ws)
  (place-image CAR ws Y-CAR BACKGROUND))

; WorldState -> WorldState
; moves the car by 3 pixels for every clock tick
; given 20, expect 23
; given 78, expect 81
(define (tock ws)
  (+ SPEED ws))

; key-stroke-handler
; WorldState String -> WorldState
; for each key stroke, re-initiate the world to 0
(define (stroke ws ke)
  0)

; mouse-event-handler
; WorldState Number Number String -> WorldState
; places the car at the x-coordinate
; if the given me is "button-down"
; given 21 10 20 "enter", expected 21
; given 42 10 20 "button-down", expected 10
; given 42 10 20 "move", expected 42
(define (hyper x-coordinate x-mouse y-mouse me)
  (if (string=? me "button-down")
      x-mouse
      x-coordinate))

; WorldState -> Boolean
; when the car has disappeared on the right side
; stop the animation
(define (end? ws)
  (> (- ws (/ (image-width CAR) 2))
     (image-width BACKGROUND)))


; main
; WorldState -> WorldState
; launches the program from some initial state
(define (main ws)
  (big-bang ws
            [on-tick tock]
            [to-draw render]
            [on-key stroke]
            [on-mouse hyper]
            [stop-when end?]))