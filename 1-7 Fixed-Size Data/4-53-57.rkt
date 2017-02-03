;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 4-53-57) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 300); distance in pixels
(define WIDTH 100)
(define YDELTA 30)

(define BACKG (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))

(define CENTER (- HEIGHT (/ (image-height ROCKET ) 2))
)
; A LRCD (for launching rocket count down) is one of:
; - "resting"
; - a Number between -3 and -1
; - a Nonnegativenumber
; interpretation a grounded rocket,
; a number denotes the number of pixels between the
; bottom of the canvas and the rocket (its height)

; Number -> Image
; places the rocket (- CENTER n) pixels from the top of the canvas
(define (show/rocket n)
  (place-image ROCKET 10 (- CENTER n) BACKG))

; LRCD -> Image
; renders the state as a resting or flying rocket
(define (show x)
  (cond
    [(string? x)
     (show/rocket 0)]
    [(<= -3 x -1)
     (place-image (text (number->string x) 20 "red")
                  10 (* 3/4 WIDTH)
                  (show/rocket 0))]
    [(>= x 0)
     (show/rocket x)]))

; LRCD KeyEvent -> LRCD
; starts the count-down when space bar is pressed
; if the rocket is still resting
(define (launch x ke)
  (cond
    [(string? x) (if (string=? " " ke) -3 x)]
    [(<= -3 x -1) x]
    [(>= x 0) x]))

; LRCD -> LRCD
; raises the rocket by YDELTA,
; if it is moving already
(define (fly x)
  (cond
    [(string? x) x]
    [(<= -3 x -1) (+ 1 x)]
    [(>= x 0) (+ x YDELTA)]))

; LRCD -> Boolean
; ends the program when the rocket disappears
; at the top of the canvas
(define (end? x)
  (and (number? x) (> x HEIGHT)))     

; LRCD -> LRCD
(define (main s)
  (big-bang s
            [to-draw show]
            [on-key launch]
            [on-tick fly 1]
            [stop-when end?]))