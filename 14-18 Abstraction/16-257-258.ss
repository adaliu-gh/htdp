;;===================
;;257

;; [X] N [N -> X] -> [List-of X]
(check-expect (build-l*st 10 add1)
              (build-list 10 add1))
(define (build-l*st n f)
  (cond
   [(zero? n) '()]
   [else (add-at-end (f (- n 1)) (build-l*st (- n 1) f))]))

;; [X] X [List-of X] -> [List-of X]
(define (add-at-end x l)
  (cond
   [(empty? l) (list x)]
   [else (cons (first l)
               (add-at-end x (rest l)))]))

;;===========================
;;258

(require 2htdp/image)

(define MT (empty-scene 50 50))
;; a Polygon is one of:
;; - (list Posn Posn Posn)
;; - (cons Posn Polygon)

;; Image Polygon -> Image
;; adds an image of p to MT
(define (render-polygon img p)
  (local (; Image Polygon -> Image
          (define (connect-dots img p)
            (cond
             [(empty? (rest p)) MT]
             [else (render-line (connect-dots img (rest p))
                                (first p)
                                (second p))]))
          ;; connected dots except the first and the last
          (define partly-connected (connect-dots MT p))

          ;;Polygon -> Posn
          ;;extracts the last item from p
          (define (last p)
            (cond
             [(= 3 (length p)) (third p)]
             [else (last (rest p))]))        ))
  (render-line partly-connected (first p) (last p)))
