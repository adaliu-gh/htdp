;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12-220) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Simple Tetris Game

(require 2htdp/image)
(require 2htdp/universe)

;; constants
(define WIDTH 10) ; # of blocks, horizontally
(define HEIGHT (* WIDTH 3/2))
(define SIZE 10) ; blocks are squares
(define SCENE-SIZE (* WIDTH SIZE))
(define BLOCK ; red squares with black rims
  (overlay
   (square (- SIZE 1) "solid" "red")
   (square SIZE "outline" "black")))
(define MT (empty-scene SCENE-SIZE
                        (* SIZE HEIGHT)))

;; data definitions
(define-struct tetris [block landscape])
(define-struct block [x y])

;  A Tetris is a structure:
; (make-tetris Block Landscape)
; A Landscape is one of:
; – '()
; – (cons Block Landscape)
; Block is a structure:
; (make-block N N)

; interpretations
; (make-block x y) depicts a block whose left
; corner is (* x SIZE) pixels from the left and
; (* y SIZE) pixels from the top;
; (make-tetris b0 (list b1 b2 ...)) means b0 is the
; dropping block, while b1, b2, and ... are resting

(define landscape0 '())
(define block-dropping (make-block 2 2))
(define tetris0-drop (make-tetris block-dropping landscape0))
(define block-landed (make-block 0 (- HEIGHT 1)))
(define block-on-block (make-block 0 (- HEIGHT 2)))
(define landscape1 (list block-landed block-on-block block-dropping))

; Tetris -> Image
(define (tetris-render tetris)
  (blocks-render (cons (tetris-block tetris)
                       (tetris-landscape tetris))
                 MT))

; Landscape Image -> Image
(define (blocks-render blocks img)
  (cond
    [(empty? blocks) img]
    [else (single-render (first blocks)
                         (blocks-render (rest blocks) img)  )]))

; Block Image -> Image
(define (single-render block img)
  (place-image BLOCK
               (+ (* (block-x block) SIZE) (/ SIZE 2))
               (+ (* (block-y block) SIZE) (/ SIZE 2))
               img))
