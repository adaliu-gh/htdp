;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12-220) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Simple Tetris Game

(require 2htdp/image)
(require 2htdp/universe)

;; constants

(define SPEED 0.01) ; of the dropping of blocks
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

; Number -> List-of-numbers
(check-expect (generate-rows 3) (list 2 1 0))
(define (generate-rows n)
  (cond
    [(zero? n) '()]
    [else (cons (- n 1)
                (generate-rows (- n 1)))]))
(define ROWS (generate-rows HEIGHT))
;; data definitions
(define-struct tetris [block landscape score])
(define-struct block [x y])
(define-struct row [index many])
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
                         landscape0
                         0)
            [stop-when touch-top? last-image]
            [on-key tetris-control]
            [on-tick tetris-tock SPEED]
            [to-draw tetris-render]))

; Tetris -> Tetris
(define (tetris-tock t)
  (cond
    [(land? t)(tetris-update t)]
    [(has-full-lines? t) (remove-lines-in-tetris t)]
    [else (tetris-dropping t)]))


; Tetris -> Tetris
(define (tetris-update t)
  (make-tetris (block-generate WIDTH)
               (cons (tetris-block t)
                     (tetris-landscape t))
               (tetris-score t)))

; Tetris -> Tetris
(define (tetris-dropping t)
  (make-tetris (block-dropping (tetris-block t))
               (tetris-landscape t)
               (tetris-score t)))

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
  (cond
    [(member? k (list "left" "right"))
      (control-block t k)]
    [else t]))

; Tetris String -> Tetris
(define (control-block t k)
  (cond
    [(touch-side? t k) t]
    [else (make-tetris (block-slide (tetris-block t) k)
                       (tetris-landscape t)
                       (tetris-score t))]))


; Tetris String-> Boolean
(define (touch-side? t k)
  (or (member? (block-slide (tetris-block t) k)
               (tetris-landscape t))
      (= (block-x (tetris-block t))
         (if (string=? k "left")
             0
             (- WIDTH 1)))))

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
               2  2
               (tetris-render t)))

; Tetris -> Number
(define (get-score t)
  (tetris-score t))

; ====================
; my own features - removing full lines
; ====================

; Tetris -> Boolean
(define (has-full-lines? t)
  (> (length (final-full-lines t)) 0))

; Tetris -> List-of-numbers
(define (final-full-lines t)
  (full-lines (all-lines (tetris-landscape t))))

; List-of-rows -> List-of-numbers
(check-expect (full-lines (list (make-row 8 10) (make-row 7 3)))
              (list 8))
(define (full-lines r)
  (cond
    [(empty? r) '()]
    [else (if (= (row-many (first r)) WIDTH)
              (cons (row-index (first r))
                    (full-lines (rest r)))
              (full-lines (rest r)))]))

; Landscape -> List-of-rows
(define (all-lines l)
  (get-all-lines l ROWS))

; Landscape List-of-numbers -> List-of-rows
(define (get-all-lines l r)
  (cond
    [(empty? r) '()]
    [else (cons (make-row (first r )
                          (count-a-line l (first r)))
                (get-all-lines l (rest r)))]))

; Landscape Number -> Number
(check-expect (count-a-line (list (make-block 2 3)) 3) 1)
(define (count-a-line l n)
  (cond
    [(empty? l) 0]
    [else (if (= n (block-y (first l)))
              (+ 1 (count-a-line (rest l) n))
              (count-a-line (rest l) n))]))

; Tetris -> Tetris
(define (remove-lines-in-tetris t)
  (make-tetris (tetris-block t)
               (remove-lines (tetris-landscape t)
                             (final-full-lines t))
               (+ (length (final-full-lines t)) (tetris-score t))))

; Landscape List-of-numbers -> Landscape
(define (remove-lines l n)
  (cond
    [(empty? l) '()]
    [else (if (member? (block-y (first l)) n)
              (remove-lines (rest l) n)
              (cons (make-block (block-x (first l))
                                (+ 1 (block-y (first l))))
                    (remove-lines (rest l) n)))]))
