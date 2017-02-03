;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 6-109) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;----------------------
(define AA "start, expected an 'a'")
(define BB "expect 'b', 'c', or 'd'")
(define DD "finished")
(define ER "error, illegal key")

; A State is one of:
; - AA
; - BB
; - DD
; - ER
;-----------------------

;-----------------------
(define GREEN (rectangle 100 100 "solid" "green"))
(define YELLOW (rectangle 100 100 "solid" "yellow"))
(define RED (rectangle 100 100 "solid" "red"))
;----------------------

;-----------------------
; -> State
(define (main s)
  (big-bang AA
            [on-key stroke]
            [to-draw render]))

; State KeyEvent -> State
; AA->BB when "a" pressed
; BB->BB when "b" or "c" pressed
; BB->DD when "d" pressed
; AA->ER when keys other than "a" pressed
; BB->ER when keys other than "b" "c" or "d" pressed
;(check-expect (stroke AA "a") BB)
;(check-expect (stroke AA "b") ER)
;(check-expect (stroke BB "b") BB)
;(check-expect (stroke BB "c") BB)
;(check-expect (stroke BB "d") DD)
;(check-expect (stroke BB "e") ER)
;(check-expect (stroke DD "a") DD)
;(check-expect (stroke ER "a") ER)
(define (stroke s ke)
  (cond
    [(equal? s AA)
     (if (key=? ke "a") BB ER)]
    [(equal? s BB)
     (cond
       [(or (key=? ke "b") (key=? ke "c")) BB]
       [(key=? ke "d") DD]
       [else ER])]
    [else s]))

; State -> Image
; render the corresponding image according to specific state
; when AA and BB, show yellow
; when DD, show green
; when ER, show red
;(check-expect (render AA) YELLOW)
;(check-expect (render BB) YELLOW)
;(check-expect (render DD) GREEN)
;(check-expect (render ER) RED)
(define (render s)
  (cond
    [(or (equal? s AA) (equal? s BB))
     YELLOW]
    [(equal? s DD) GREEN]
    [(equal? s ER) RED]))

