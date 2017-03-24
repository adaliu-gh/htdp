(require 2htdp/image)

(define COLOR 'red)
(define MT (empty-scene 400 400))
(define A (make-posn 200  50))
(define B (make-posn  27 350))
(define C (make-posn 373 350))
;;=========================
;; 526

;; Image Posn Posn Posn -> Image
;; add the black triangle a, b, c to scene
(define (add-triangle scene a b c)
  (local (;; Image [List-of (list Posn Posn)] -> Image
          ;; add a list of lines to img
          (define (add-triangle/line img lol)
            (cond
             [(empty? lol) img]
             [else (local ((define first-line (first lol)))
                     (add-triangle/line
                      (scene+line img (posn-x (first first-line))
                                  (posn-y (first first-line))
                                  (posn-x (second first-line))
                                  (posn-y (second first-line))
                                  COLOR)
                      (rest lol)))])))
    (add-triangle/line scene (list (list a b) (list a c) (list b c)))))


;; Posn Posn -> Boolean
;; calculate the distance between two posns
(define (distance a b)
  (sqrt (+ (expt (- (posn-y a) (posn-y b)) 2)
           (expt (- (posn-x a) (posn-x b)) 2))))

;; Posn Posn Posn -> Boolean
;; is the triangle a, b, c too small to be divided
(define (too-small? a b c)
  (<= (+ (distance a b)
         (distance a c)
         (distance b c))
      e))

;; Posn Posn -> Posn
;; determine the midpoint between a and b
(define (mid-point a b)
  (make-posn (/ (+ (posn-x a) (posn-x b)) 2)
             (/ (+ (posn-y a) (posn-y b)) 2)))


;; Image Posn Posn Posn -> Image
;; generative adds the triangle (a, b, c) to s,
;; sub-divides it into three triangles by taking the
;; midpoints of its sides; stop if (a, b, c) is too small
;; accumulator the function accumulates the triangles scene0

(define (add-sierpinski scene0 a b c)
  (cond
   [(too-small? a b c) scene0]
   [else
    (local
        ((define scene1 (add-triangle scene0 a b c))
         (define mid-a-b (mid-point a b))
         (define mid-b-c (mid-point b c))
         (define mid-c-a (mid-point c a))
         (define scene2
           (add-sierpinski scene1 a mid-a-b mid-c-a))
         (define scene3
           (add-sierpinski scene2 b mid-b-c mid-a-b)))
      (add-sierpinski scene3 c mid-c-a mid-b-c))]))


;;=====================
;; 527

(define CENTER (make-posn 200 200))
(define RADIUS 200); the radius in pixels

;; Number -> Posn
;; determine the point on the circle with CENTER
;; and RADIUS whose angle is factor

;; e.x.
;; what are the x and y coordinates of the desired
;; point, when given: 120/360??

(define (circle-pt factor)
  (make-posn
   (+ 200 (floor (cos (* 2 pi factor))))
   (- 200 (floor (sin (* 2 pi factor))))))
