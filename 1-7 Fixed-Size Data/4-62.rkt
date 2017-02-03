;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 4-62) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define LOCKED "locked") ; A DoorState is one of:
(define CLOSED "closed") ; - Locked
(define OPEN "open")     ; - Closed
                         ; - OPEN

; DoorState -> DoorState
; close the door when it is open
(define (door-closer s)
  (if
    (equal? s OPEN)
    CLOSED
    s))

; DoorState -> Image
; renders the corresponding image according to DoorState s
(define (door-render s)
  (text s 40 "red"))

; DoorState KeyEvent -> DoorState
; manipulates the door in response to pressing a key
; given CLOSED, " ", expected OPEN
; given LOCKED, "u", expected CLOSED
; given CLOSED, "l", expected LOCKED
(define (door-actions s ke)
  (cond
    [(and (equal? s LOCKED) (string=? ke "u")) CLOSED]
    [(and (equal? s CLOSED) (string=? ke " ")) OPEN]
    [(and (equal? s CLOSED) (string=? ke "l")) LOCKED]
    [else s]))

; DoorState -> DoorState
; simulates a door with an automatic door closer
(define (door-simulation initial-state)
  (big-bang initial-state
            [on-tick door-closer 3]
            [on-key door-actions]
            [to-draw door-render]))