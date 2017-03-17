(require 2htdp/image)

;;================================
(define QUEENS 8)
(define WIDTH 10)
(define IMAGE-QUEEN (triangle WIDTH "solid" "red"))
(define CELL-WIDTH (+ 2 WIDTH))
(define CELL (overlay (rectangle WIDTH WIDTH "solid" "white")
                      (rectangle CELL-WIDTH CELL-WIDTH "solid" "black")))

;; a QP (queen position) is a structure;
;; (make-posn CI CI)
;; a CI is an N in [0, QUEENS).
;; interpretation (make-posn r c) denotes the square at
;; the r-th row and c-th column

;;=========================
;; 479

;; QP QP -> Boolean
;; check if two queens threaten each other
(check-expect (threatening? (make-posn 0 0) (make-posn 0 7)) #true)
(check-expect (threatening? (make-posn 0 0) (make-posn 1 0)) #true)
(check-expect (threatening? (make-posn 0 0) (make-posn 4 4)) #true)
(check-expect (threatening? (make-posn 1 0) (make-posn 5 4)) #true)
(check-expect (threatening? (make-posn 0 7) (make-posn 7 0)) #true)
(check-expect (threatening? (make-posn 7 7) (make-posn 0 0)) #true)
(check-expect (threatening? (make-posn 0 0) (make-posn 4 3)) #false)
(define (threatening? qp1 qp2)
  (local ((define x1 (posn-x qp1))
          (define x2 (posn-x qp2))
          (define y1 (posn-y qp1))
          (define y2 (posn-y qp2)))
    (or (= x1 x2)
        (= y1 y2)
        (= (abs (- x2 x1) ) (abs (- y2 y1) )))))

;;================================
;; 480

(define test-queens (list (make-posn 0 0)
                          (make-posn 7 7)
                          (make-posn 4 3)))


;; Number [List-of QP] Image -> Image
;; render an image of an n by n chess board with image i placed according to l

(define (render-queens n l i)
  (place-queens l i (chess-board n)))

;; Number -> Image
;; create an n by n chess board
(define (chess-board n)
  (local (
          (define (create-row n)
            (cond
             [(= 1 n) CELL]
             [else (beside CELL (create-row (sub1 n)))]))
          (define row (create-row n))
          (define (chess-board-helper n)
            (cond
             [(= 1 n) row]
             [else (above row (chess-board-helper (sub1 n)))]))
          )
    (chess-board-helper n)))

;; [List-of QP] Image Image -> Image
;; places queens according to l upto the chess-board
(define (place-queens l i c)
  (local ((define (real-coordinate x)
            (+ (/ CELL-WIDTH 2) (* CELL-WIDTH x))))
    (cond
     [(empty? l) c]
     [else (place-queens (rest l)
                         i
                         (place-image i
                                      (real-coordinate (posn-x (first l)))
                                      (real-coordinate (posn-y (first l)))
                                      c))])))
