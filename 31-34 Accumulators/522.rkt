(require 2htdp/image)

(define RADIUS 10)
(define CANNIBAL (circle RADIUS "solid" "red"))
(define MISSIONARY (circle RADIUS "solid" "black"))
(define BOAT (above (flip-vertical (triangle RADIUS "solid" "black") )
                    (rectangle RADIUS RADIUS "solid" "black")))
(define BANK (rectangle (* 5 RADIUS) (* 7 RADIUS) "outline" "black"))
(define RIVER (rectangle (* 8 RADIUS) (* 7 RADIUS) "solid" "skyblue"))

;;==========================
;; 522

(define-struct state [left boat right])
;; a *Puzzle State* is a structure:
;; - (make-state Bank Boat Bank)

;; a *Bank* is a list of 2 Numbers (list x y)
;; where x represents the number of missionaries
;; and y represents the number of cannibals

;; a *Boat* is one of:
;; - 'left (representing the left side of the river)
;; - 'right (representing the right side)

;; e.x.
(define initial (make-state '( 3 3) 'left '(0 0)))
(define state1 (make-state '(2 2) 'right '(1 1)))
(define final (make-state '(0 0) 'right '(3 3)))

;; State -> Boolean
;; detect where in a given state all people are on the right river bank
(check-expect (final? final) #true)
(check-expect (final? initial) #false)
(define (final? state)
  (equal? (state-right state) '(3 3)))


;; State -> Image
;; render the given state to an image

(define (render-mc s)
  (local (;; Number Image -> Image
          (define (render-human n img)
            (cond
             [(zero? n) empty-image]
             [else (above img (render-human (sub1 n) img))]))

          ;; [List-of Number] -> Image
          (define (render-bank lon)
            (overlay (beside (render-human (first lon) MISSIONARY)
                             (render-human (second lon) CANNIBAL))
                     BANK))

          ;; Symbol -> Image
          (define (render-river location)
            (overlay/align location 'middle BOAT RIVER)))

    (beside (render-bank (state-left s))
            (render-river (state-boat s))
            (render-bank (state-right s)))))


