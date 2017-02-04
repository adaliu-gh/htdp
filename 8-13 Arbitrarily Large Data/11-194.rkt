;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 11-194) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;--------------------------------
(define MT (empty-scene 50 50))
;--------------------------------
; Image NELoP Posn-> Image
; connects the Posns to the last Posn in p in an image
(define (connect-dots img p pos)
  (cond
    [(empty? (rest p)) (render-line img (first p) pos)]
    [else (render-line (connect-dots img (rest p) pos)
                       (first p)
                       (second p))]))

; Image Posn Posn -> Image
; draws a red line from Posn p to Posn q into im
(define (render-line im p q)
  (scene+line
   im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))

; Polygon -> Posn
; extracts the last item from p
(define (last p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))

;-----------------------------------
; Image Polygon -> Image
; adds an image of p to MT
(define (render-poly img p)
  (connect-dots img p (first p)))
