(require 2htdp/universe)
(require 2htdp/image)

(define RADIUS 10)
(define BOAT-MAX-PASSENGERS 2)
(define ANIMATION-SPEED 1)
(define INITIAL-MISSIONARIES 3)
(define INITIAL-CANNIBALS 3)

(define CANNIBAL (circle RADIUS "solid" "red"))
(define MISSIONARY (circle RADIUS "solid" "black"))
(define BOAT (above (flip-vertical (triangle RADIUS "solid" "black") )
                    (rectangle RADIUS RADIUS "solid" "black")))
(define RIVER (rectangle (* 8 RADIUS)
                         (* (* 2 (max INITIAL-CANNIBALS INITIAL-MISSIONARIES)) RADIUS)
                         "solid" "skyblue"))


;;==========================
;; 523


(define-struct ps [left boat right path])
;; a *Ps* (short for Puzzle-State) is a structure:
;; - (make-ps Bank Boat Bank Ps])
;; where Ps is the last ps traversed to get to the current ps

(define-struct bank [mis can])
;; a *Bank* is a structure:
;; - (make-bank x y)
;; where x represents the number of missionaries
;; and y represents the number of cannibals

;; a *Boat* is one of:
;; - 'left (representing the left side of the river)
;; - 'right (representing the right side)

;; e.x.
(define initial (make-ps (make-bank INITIAL-MISSIONARIES INITIAL-CANNIBALS) 'left (make-bank 0 0) '()))
(define ps1 (make-ps (make-bank 2 2) 'right (make-bank 1 1) initial))

;; Ps -> Boolean
;; detect where in a given ps all people are on the right river bank
(check-expect (final? initial) #false)
(define (final? ps)
  (equal? (ps-right ps) (make-bank INITIAL-MISSIONARIES INITIAL-CANNIBALS)))

;; Ps Ps -> Boolean
;; check is two pss equal except their paths
(define (ps=? s1 s2)
  (and (equal? (ps-left s1) (ps-left s2))
       (equal? (ps-right s1) (ps-right s2))
       (equal? (ps-boat s1) (ps-boat s2))))

;; Ps -> Image
;; render the given ps to an image

(define (render-mc s)
  (local (;; Number Image -> Image
          (define (render-human n img)
            (cond
             [(zero? n) empty-image]
             [else (above img (render-human (sub1 n) img))]))

          ;; Bank -> Image
          (define (render-bank bank)
            (beside (render-human (bank-mis bank) MISSIONARY)
                    (render-human (bank-can bank) CANNIBAL)))

          ;; Symbol -> Image
          (define (render-river location)
            (overlay/align location 'middle BOAT RIVER)))

    (beside (render-bank (ps-left s))
            (render-river (ps-boat s))
            (render-bank (ps-right s)))))


;;=================================
;; 524



;; [List-of Ps] -> [List-of Ps]
;; generate all pss that a boat ride can reach
(define (create-next-pss los)
  (local (;; Ps Ps -> [List-of Ps]
          ;; check if the given ps is already in the path
          (define (check-repreat-path path s)
              (cond
               [(empty? s) '()]
               [else
                (local ((define left (ps-left s))
                        (define right (ps-right s))
                        (define boat (ps-boat s)))
                  (cond
                   [(empty? path) (list s)]
                   [else (if (ps=? s path)
                             '()
                             (check-repreat-path (ps-path path) s))]))]))

          ;; Ps -> [List-of Ps]
          (define (create-next-pss/one s)
            (foldr (lambda (x y)
                     (append (check-repreat-path (ps-path s)
                                            (ride-boat s (first x) (second x))) y))
                   '() (generate-boat-rides BOAT-MAX-PASSENGERS))))

    (foldr (lambda (x y)
             (append (create-next-pss/one x) y)) '() los)))

;; Number -> [List-of (list Number Number)]
(define (generate-boat-rides max)
  (local ((define (generate-boat-rides/total total)
            (foldr (lambda (x y)
                     (append y
                             (cond
                              [(= x (/ total 2))
                               (list (list x (- total x)))]
                              [else (list (list x (- total x)) (list (- total x) x))])))
                   '()
                   (build-list total (lambda (x) x)))))
    (foldr (lambda (x y)
             (append y (generate-boat-rides/total x)))
           '()
           (build-list max (lambda (x) (add1 x))))))

;; Ps Number Number -> [Maybe Ps]
;; ride mis missionaries and can cannibals between the river
(check-expect (ride-boat initial 1 1) ps1)
(define (ride-boat s mis can)
  (local (;; Number Number -> Boolean
          ;; check if the cannibals would not-eat the missionaries
          (define (not-eat? mis can)
            (or (zero? mis)
                (>= mis can)))

          ;; Bank Bank Boat -> Ps
          ;; ride the boat from b1 to b2 and the direction of the boat is dir
          (define (ride-boat/from b1 b2 dir)
            (local ((define from-bank (make-bank (- (bank-mis b1) mis)
                                                 (- (bank-can b1) can)))
                    (define to-bank (make-bank (+ (bank-mis b2) mis)
                                               (+ (bank-can b2) can))))
              (cond
               [(and (>= (bank-mis from-bank) 0)
                     (>= (bank-can from-bank) 0)
                     (not-eat? (bank-mis to-bank) (bank-can to-bank))
                     (not-eat? (bank-mis from-bank) (bank-can from-bank)))
                (if (equal? dir 'right)
                    (make-ps from-bank 'right to-bank s)
                    (make-ps to-bank 'left from-bank s))]
               [else '()]))))
    (cond
     [(equal? (ps-boat s) 'left)
      (ride-boat/from (ps-left s) (ps-right s) 'right)]
     [else (ride-boat/from (ps-right s) (ps-left s) 'left)])))

;;===========================
;; 525


;; Ps -> Ps
;; solve the missionaries-and-cannibals puzzle from the initial ps
(define (solve ps0)
  (local ((define (solve/a los)
            (cond
             [(ormap final? los)
              (first (filter final? los))]
             [else (solve/a (create-next-pss los))])))
    (solve/a (list ps0))))


;; Ps -> Ps
;; run an animation to show the solution to the missionaries-and-cannibals puzzle

(define (run-solution initial)
  (local ((define solution (solve initial))
          (define (generate-images solution)
            (cond
             [(empty? solution)  '()]
             [else (cons (render-mc solution)
                         (generate-images (ps-path solution)))])))
    (run-movie ANIMATION-SPEED (reverse (generate-images solution) ))))


;; BONUS
;;=================================
;; MY OWN ABSTRACTION OF THE PUZZLE

;; Number Number Number -> Ps
;; run an animation to show the solution to the missionaries-and-cannibals puzzle

(define (run-solution-abs mis can max)
  (local ((define solution (solve-abs mis can max))
          (define (generate-images solution)
            (cond
             [(empty? solution)  '()]
             [else (cons (render-mc solution)
                         (generate-images (ps-path solution)))])))
    (run-movie ANIMATION-SPEED (reverse (generate-images solution) ))))


;; Number Number Number -> Ps
;; given the number of missionaries and cannibals
;; and the max number of passengers on the boat
;; solve the problem
(define (solve-abs mis can max)
  (local ((define ps0
            (make-ps (make-bank mis can) 'left (make-bank 0 0) '()))

          (define (final-abs? s)
            (equal? (ps-right s)
                    (make-bank mis can)))

          (define (solve-abs/a los)
            (cond
             [(ormap final-abs? los)
              (first (filter final-abs? los))]
             [else (local ((define next-los (create-next-pss-abs los (generate-boat-rides max))))
                     (if (<= (length next-los) (length los))
                         #false
                         (solve-abs/a next-los)))])))
    (solve-abs/a (list ps0))))

;; [List-of Ps] [List-of (list Number Number)]-> [List-of Ps]
;; generate all pss that a boat ride can reach
(define (create-next-pss-abs los rides)
  (local (;; Ps Ps -> [List-of Ps]
          ;; check if the given ps is already in the path
          (define (check-repreat-path path s)
              (cond
               [(empty? s) '()]
               [else
                (local ((define left (ps-left s))
                        (define right (ps-right s))
                        (define boat (ps-boat s)))
                  (cond
                   [(empty? path) (list s)]
                   [else (if (ps=? s path)
                             '()
                             (check-repreat-path (ps-path path) s))]))]))

          ;; Ps -> [List-of Ps]
          (define (create-next-pss/one s)
            (foldr (lambda (x y)
                     (append (check-repreat-path (ps-path s)
                                            (ride-boat s (first x) (second x))) y))
                   '() rides)))

    (foldr (lambda (x y)
             (append (create-next-pss/one x) y)) '() los)))
