;;========================
;; 504

;; Matrix -> Matrix
;; find a row that doesn't start with 0 and
;; use it as the first one
;; generative move the first row to last place
;; no termination if all rows start with 0
(check-expect (rotate.v1 '((0 4 5) (1 2 3)))
              '((1 2 3) (0 4 5)))
(define (rotate.v1 M)
  (cond
   [(not (= (first (first M)) 0)) M]
   [else
    (rotate.v1 (append (rest M) (list (first M))))]))


(check-expect (rotate.v1 '((0 4 5) (1 2 3)))
              '((1 2 3) (0 4 5)))
(define (rotate.v2 M)
  (local ((define (rotate/a M a)
            (cond
             [(empty? M) (reverse a)]
             [else (if (= (first (first M)) 0)
                       (rotate/a (rest M) (cons (first M) a))
                       (append M (reverse a)))])))
    (rotate/a M '())))


;; Number -> Matrix
;; create a n by 1 matrix randomly
;; with all first (n-1) rows starting with 0
;; except the last row
(define (create-matrix n)
  (cond
   [(= n 1) '((1))]
   [else (cons '(0) (create-matrix (sub1 n)))]))
(define matrix-1000 (create-matrix 1000))
(define matrix-2000 (create-matrix 2000))


;;=======================
;; 505

;; [List-of Number] -> Number
;; convert a list of digits into a number
(check-expect (to10 '(1 0 2)) 102)
(define (to10 lod)
  (local ((define (to10/a lod a)
            (cond
             [(empty? lod) a]
             [else (to10/a (rest lod) (+ (first lod) (* 10 a)))])))
    (to10/a lod 0)))


;;==========================
;; 506

;; Number -> Boolean
;; check if n is a prime number
(check-expect (is-prime 2) #true)
(check-expect (is-prime 3) #true)
(check-expect (is-prime 12) #false)
(check-expect (is-prime 13) #true)
(define (is-prime n)
  (local ((define (is-prime/a n a)
            (cond
             [(= a 1) #true]
             [else (and  (not (zero? (remainder n a)))
                         (is-prime/a n (sub1 a)))])))
    (cond
     [(= n 1) #false]
     [else (is-prime/a n (sub1 n))])))


;;=========================
;; 507

;; [X][Y] [X -> Y] [List-of X] -> [List-of Y]
;; an accumulator-version of map
(check-expect (map-a add1 '(1 2 3)) '( 2 3 4))
(define (map-a f l)
  (local ((define (map-a/a l a)
            (cond
             [(empty? l) (reverse a)]
             [else (map-a/a (rest l) (cons (f (first l)) a))])))
    (map-a/a l '())))


;;=========================
;; 508

;; [X] Number [Number -> X] -> [List-of X]
;; build-l*st
(check-expect (build-l*st 45 add1) (build-list 45 add1))
(define (build-l*st n f)
  (local ((define (build-l*st/a n a)
            (cond
             [(zero? n) a]
             [else (build-l*st/a (sub1 n) (cons (f (sub1 n)) a))])))
    (build-l*st/a n '())))
