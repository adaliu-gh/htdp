;;========================
;; 500

;; [List-of Number] -> Number
;; calculate the product of a list of numbers
(check-expect (product '( 1 2 3)) 6)
(define (product lon)
  (local ((define (product/a lon a)
            (cond
             [(empty? lon) a]
             [else (product/a (rest lon) (* a (first lon)))])))
    (product/a lon 1)))


;;===========================
;; 501

;; [X] [List-of X] -> Number
;; count the number of items in a given list
(check-expect (how-many '( 2 3 4 5)) 4)
(define (how-many l)
  (local ((define (how-many/a l a)
            (cond
             [(empty? l) a]
             [else (how-many/a (rest l) (add1 a))])))
    (how-many/a l 0)))


;;=============================
;; 502

;; Number -> Number
;; add n to pi without use +
(check-within (add-to-pi 2) (+ 2 pi) 0.001)
(define (add-to-pi n)
  (local ((define (add-to-pi/a n a)
            (cond
             [(zero? n) a]
             [else (add-to-pi/a (sub1 n) (add1 a))])))
    (add-to-pi/a n pi)))


;;============================
;; 503

;; [X] [List-of X] -> [List-of X]
;; create a polindrome
(check-expect (polindrome (explode "abc")) (explode "abcba"))
(define (polindrome l)
  (local ((define (polindrome/a l a)
            (cond
             [(empty? (rest l)) a]
             [else (polindrome/a (rest l) (cons (first l) a))])))
    (append l (polindrome/a l '()))))


;; [X] [List-of X] -> [List-of X]
;; create a polindrome
(define (mirror s0)
  (local ((define (all-but-last l)
            (cond
             [(empty? (rest l)) '()]
             [else (cons (first l) (all-but-last (rest l)))]))
          (define (last l)
            (cond
             [(empty? (rest l)) (first l)]
             [else (last (rest l))])))
    (append (all-but-last s0)
            (list (last s0))
            (reverse (all-but-last s0)))))
