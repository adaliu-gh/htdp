;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12-215-219) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;------------------------
; DATA DEFINITION
;------------------------

; a Segment is a Posn
; (make-posn Number Number)
; interpretation:
; x represents how many DIAMETERs the seg is from the left
; y represents how many DIAMETERs the seg is from the top
(define seg1 (make-posn 12 12))

; a Tail is a List-of-segments, one of:
; - (list Seg)
; - (cons Posn Tail)
; A worm’s tail is a sequence of “connected” segments.
; Here “connected” means that the coordinates of a segment
; differ from those of its predecessor in at most one direction.
(define tail1 (list seg1))

(define-struct worm [d tail])
; a Worm is a structure:
; (make-worn String Tail)
; interpretation:
; d represents the direction the worm is moving
; tail represents the worm segments
(define worm1 (make-worm "up" tail1))

; a Food is a Posn, which represents its position
(define food1 (make-posn 5 5))

; a Ws (short for world state) is a structure:
(define-struct ws [worm food])
; (make-ws Worm Posn)

;------------------------
; CONSTANTS
;------------------------
; how many pixels a segment is
(define DIAMETER 10)
(define RADIUS (/ DIAMETER 2))

; the speed of worms' movement
(define SPEED 0.2)

(define DIRECTIONS (list "right" "left" "up" "down"))

; a segment of worm
(define SEGMENT (circle RADIUS "solid" "red"))

; Food
(define FOOD (circle RADIUS "solid" "blue"))

; the length of MT
(define MAX 25)
; background
(define MT (empty-scene (* DIAMETER MAX)
                        (* DIAMETER MAX)))

;-----------------------
; FUNCTIONS
;-----------------------

; Any -> Ws
; main function
(define (main any)
  (big-bang (make-ws worm1 food1)
            [on-tick move SPEED]
            [on-key control]
            [to-draw render]
            [stop-when hit? last-image]
            ))

;-------------------------
; GAME-OVER
;------------------------
; Ws -> Boolean
; determines if the worm hits the wall or runs into itself
(define (hit? w)
  (or (hit-wall? (first (worm-tail (ws-worm w))))
      (eat-self? (worm-tail (ws-worm w)))))

; Posn -> Boolean
; determines if the segment hits the wall
(define (hit-wall? s)
  (or
   (< (posn-x s) 0)
   (< (posn-y s) 0)
   (>= (posn-x s) MAX)
   (>= (posn-y s) MAX)
   ))

; Tail -> Boolean
; determines if the worm runs into itself
(define (eat-self? t)
  (member? (first t) (rest t)))

; Ws -> Image
; renders the game-over image
(define (last-image w)
  (place-image (text (string-append
                      "Your Score is: "
                      (number->string
                       (length (worm-tail
                                (ws-worm w)))))
                     (* 2 DIAMETER) "red")
               (/ (image-width MT) 2)
               (/ (image-height MT) 2)
               (render w)))

;--------------------------
; AUTOMATIC MOVE
;--------------------------
; Ws -> Ws
; for every clock tick, w moves 1 DIAMETER
(define (move w)
  (if (eat-food? w)
      (after-eat (ws-worm w) (ws-food w))
      (keep-move (ws-worm w) (ws-food w))))

; Ws -> Boolean
; determines if the food has been eaten by the worm
(define (eat-food? w)
  (equal? (first (worm-tail (ws-worm w)))
          (ws-food w)))

; Worm Posn -> Ws
; inserts one segment at the position of food that has been eatn
; and generates new food
(define (after-eat w f)
  (make-ws
   (make-worm (worm-d w)
              (move-tail (worm-tail w)
                         (worm-d w)
                         (worm-tail w)))
   (food-create f)))

; Worm Posn -> Ws
; the worm keeps moving
(define (keep-move w f)
  (make-ws
   (make-worm (worm-d w)
              (move-tail (worm-tail w)
                         (worm-d w)
                         (remove-last (worm-tail w))))
   f))

; Tail String Tail -> Tail
; produces a new tail by adding a new segment and deleting the last one
(define (move-tail t d rest-tail)
  (cond
    [(string=? d "left")
     (cons (make-posn (- (posn-x (first t)) 1)
                     (posn-y (first t)))
           rest-tail)]
    [(string=? d "right")
     (cons (make-posn (+ (posn-x (first t)) 1)
                     (posn-y (first t)))
           rest-tail)]
    [(string=? d "up")
     (cons (make-posn (posn-x (first t))
                     (- (posn-y (first t)) 1))
           rest-tail)]
    [(string=? d "down")
     (cons (make-posn (posn-x (first t))
                     (+ (posn-y (first t)) 1))
           rest-tail)]))

; Tail -> Tail
; removes the last segment of the given t
(define (remove-last t)
  (cond
    [(= 1 (length t)) '()]
    [else (cons (first t)
                (remove-last (rest t)))]))

;------------------------
; Key Event Handler
;-----------------------
; Ws KeyEvent -> Ws
; moves the worm in corresponding directions
(define (control w k)
  (cond
    [(member? k DIRECTIONS)
     (make-ws (make-worm k (worm-tail (ws-worm w)))
              (ws-food w))]
    [else w]))

;-----------------------
; RENDERING
;-----------------------
; Ws -> Image
; rendering function
(define (render w)
  (render-tail (worm-tail (ws-worm w))
               (render-food (ws-food w) MT)))

; Posn Image -> Image
; renders the food
(define (render-food f img)
  (place-image FOOD
               (posn-x (locate f))
               (posn-y (locate f))
               img))

; Tail Image -> Image
; renders the tail
(define (render-tail t img)
  (cond
    [(empty? t) img]
    [else (place-image SEGMENT
                       (posn-x (locate (first t)))
                       (posn-y (locate (first t)))
                       (render-tail (rest t) img))]))

; Posn -> Posn
; gets the physical position of a segment
(define (locate s)
  (make-posn (+ RADIUS (* (posn-x s) DIAMETER))
              (+ RADIUS (* (posn-y s) DIAMETER))))

;--------------------------
; GENERATE FOOD
;--------------------------
; Posn -> Posn
; ???
(check-satisfied (food-create (make-posn 1 1))
                 not-equal-1-1?)
(define (food-create p)
  (food-check-create
   p (make-posn (random MAX) (random MAX))))
; Posn Posn -> Posn
; generative recursion
; ???
(define (food-check-create p candidate)
  (if (equal? p candidate) (food-create p) candidate))
; Posn -> Boolean
; use for testing only
(define (not-equal-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))
