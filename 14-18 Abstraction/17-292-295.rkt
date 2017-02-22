;;===================
;;292

;; [X X -> Boolean] [NEList-of X] -> Boolean
(check-expect (sorted? < '(1 2 3)) #true)
(check-expect (sorted? < '(2 1 3)) #false)

(define (sorted? cmp l)
  (cond
   [(empty? (rest l)) #true]
   [else (and (if (cmp (first l) (first (rest l)))
                  #true
                  #false)
              (sorted? cmp (rest l)))]))

;;========================
;;293

;; X -> [[Maybe List-of X] -> Boolean]
(define (found? x l0)
  (lambda (l)
    (if (list? l)
        (equal? x (first l))
        (andmap (lambda (i) (not (equal? i x))) l0))))

;; X [List-of X] -> [Maybe [List-of X]]
;; returns the first sublist of l that starts
;; with x, #false otherwise
(check-satisfied (find 2 '(1 2 3)) (found? 2 '(1 2 3)))
(check-satisfied (find 2 '(1 4 3)) (found? 2 '(1 4 3)))
(define (find x l)
  (cond
   [(empty? l) #false]
   [else
    (if (equal? (first l) x) l (find x (rest l)))]))

;;======================================
;;294

;; X [List-of X] -> [[Maybe N] -> Boolean]
(define (is-index? x l)
  (lambda (i)
    (if (boolean? i)
        (andmap (lambda (n) (not (equal? n x))) l)
        (local ((define (is-x? i l0)
                  (cond
                   [(zero? i) (equal? x (first l0))]
                   [else (is-x? (sub1 i) (rest l0))])))
          (is-x? i l))
        )))

;; X [List-of X] -> [Maybe N]
;; determine the index of the first occurrence
;; of x in l, #false otherwise
(check-satisfied (index 2 '(1 2 3)) (is-index? 2 '(1 2 3)))
(check-satisfied (index 2 '(1 4 3)) (is-index? 2 '(1 4 3)))
(define (index x l)
  (cond
   [(empty? l) #false]
   [else (if (equal? (first l) x)
             0
             (local ((define i (index x (rest l))))
               (if (boolean? i) i (+ i 1))))]))

;;===============================
;;295

(define WIDTH 300)
(define HEIGHT 300)

;; N -> [[List-of Posn] -> Boolean]
(define (n-inside-playground? n)
  (lambda (l)
    (and (= n (length l))
         (andmap (lambda (p) (and (< -1 (posn-x p) WIDTH)
                                  (< -1 (posn-y p) HEIGHT)))
                 l))))

;; N -> [List-of Posn]
;; generate n random Posns in [0,WIDTH) by [0,HEIGHT)
(check-satisfied (random-posns 3)
                 (n-inside-playground? 3))
(define (random-posns n)
  (build-list
   n
   (lambda ( i )
     (make-posn (random WIDTH) (random HEIGHT)))))
;;this function cannot check if the list is random.
