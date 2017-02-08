;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12-220) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Simple Tetris Game

(require 2htdp/image)
(require 2htdp/universe)

;; constants

(define SPEED 0.2) ; of the dropping of blocks
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

; ===================
; 220
; ===================

(define landscape0 '())

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

; ==================
; 221
; ==================

; Any -> Tetris
(define (tetris-main any)
  (big-bang (make-tetris (block-generate WIDTH)
                         landscape0)
            [stop-when touch-top? last-image]
            [on-key tetris-control]
            [on-tick tetris-tock SPEED]
            [to-draw tetris-render]))

; Tetris -> Tetris
(define (tetris-tock t)
  (if (land? t)
      (tetris-update t)
      (tetris-dropping t)))

; Tetris -> Tetris
(define (tetris-update t)
  (make-tetris (block-generate WIDTH)
               (cons (tetris-block t)
                     (tetris-landscape t))))

; Tetris -> Tetris
(define (tetris-dropping t)
  (make-tetris (block-dropping (tetris-block t))
               (tetris-landscape t)))

; Tetris -> Boolean
(define (land? t)
  (or (member? (block-dropping (tetris-block t))
               (tetris-landscape t))
      (= (block-y (tetris-block t))
         (- HEIGHT 1))))

; Block -> Block
(define (block-dropping b)
  (make-block (block-x b)
              (+ (block-y b) 1)))

; Number -> Block
(define (block-generate n)
  (make-block (random n) -1))

; ======================
; 222
; ======================

; Tetris KeyEvent -> Tetris
(define (tetris-control t k)
  (if (touch-wall? t k)
      t
      (make-tetris (block-slide (tetris-block t) k)
                                (tetris-landscape t))))

; Tetris KeyEvent -> Tetris
(define (touch-wall? t k)
  (or (member? (block-slide (tetris-block t) k)
               (tetris-landscape t))
      (member? (block-x (tetris-block t))
               (list 0 (- WIDTH 1)))))

; Block String -> Block
(define (block-slide b d)
  (cond
    [(string=? d "left")
     (make-block (- (block-x b) 1)
                 (block-y b))]
    [(string=? d "right")
     (make-block (+ (block-x b) 1)
                 (block-y b))]
    [else b]))

; =============================
; 223
; =============================

; Tetris -> Boolean
(define (touch-top? t)
  (= 0 (block-y (highest (tetris-landscape t)))))

; Landscape -> Block
(define (highest l)
  (cond
    [(empty? l) (make-block 0 HEIGHT)]
    [(empty? (rest l)) (first l)]
    [else (if (< (block-y (first l))
                 (block-y (first (rest l))))
              (highest (cons (first l)
                             (rest (rest l))))
              (highest (rest l)))]))


;; Polishment


; =================
; Score
; =================

; Tetris -> Image
(define (last-image t)
  (place-image (text (number->string (get-score t))
                      (* SIZE 2) "blue")
               (/ (image-width MT) 2)
               (/ (image-height MT) 2)
               (tetris-render t)))

; Tetris -> Number
(define (get-score t)
  (length (tetris-landscape t)))

