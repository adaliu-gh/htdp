(require 2htdp/image)

(define COLOR 'red)
(define MT (empty-scene 400 400))
(define A (make-posn 10 10))
(define B (make-posn 200 300))
(define C (make-posn 400 100))

;;======================
;; 529

;; Image Posn Posn Posn -> Image
;; draw a smooth curve between a and c according to the perspective of b
(define (draw-curve img a b c)
  (cond
   [(<= (distance a c) 1) img]
   [else
    (local ((define mid-a-b (mid-point a b))
            (define mid-c-b (mid-point b c))
            (define mid-mid (mid-point mid-a-b mid-c-b))
            (define img1
              (scene+line img (posn-x mid-mid) (posn-y mid-mid)
                          (posn-x mid-mid) (posn-y mid-mid) COLOR))
            (define img2
              (draw-curve img1 a mid-a-b mid-mid)))
      (draw-curve img2 mid-mid mid-c-b c))]))





;;======================
;; Auxiliary Functions


;; Posn Posn -> Boolean
;; calculate the distance between two posns
(define (distance a b)
  (sqrt (+ (expt (- (posn-y a) (posn-y b)) 2)
           (expt (- (posn-x a) (posn-x b)) 2))))

;; Posn Posn -> Posn
;; determine the midpoint between a and b
(define (mid-point a b)
  (make-posn (/ (+ (posn-x a) (posn-x b)) 2)
             (/ (+ (posn-y a) (posn-y b)) 2)))
