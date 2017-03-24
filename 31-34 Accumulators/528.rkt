(require 2htdp/image)


(define MT (empty-scene 400 400))
(define COLOR 'red)
(define LEFT-SHORT 33)
(define LEFT-ROTATE 6)
(define RIGHT-SHORT 20)
(define RIGHT-ROTATE 8)
(define X 200)
(define Y 400)
(define L 200)
(define D 90)

;;==============================
;; 528

;; Image Number Number Number Number -> Image

(define (add-savannah img x y l d)
  (cond
   [(<= l 8) img]
   [else
    (local
        ((define cos-l (floor (* l (cos (degree->radian d))) ))
         (define sin-l (floor (* l (sin (degree->radian d))) ))
         (define img1
           (scene+line img x y (+ x cos-l) (- y sin-l) COLOR))
         (define left-branch
           (add-savannah img1
                         (+ x (* 1/3 cos-l ))
                         (- y (* 1/3 sin-l ))
                         (* l (- 1 (/ LEFT-SHORT 100)))
                         (+ d LEFT-ROTATE))))
      (add-savannah left-branch
                    (+ x (* 2/3 cos-l ))
                    (- y (* 2/3 sin-l ))
                    (* l (- 1 (/ RIGHT-SHORT 100)))
                    (- d RIGHT-ROTATE)))]))

;; Number -> Number
;; convert a degree to a radian
(define (degree->radian d)
  (* 2 pi (/ d 360)))


