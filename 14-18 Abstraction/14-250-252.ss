;;=====================
;;250

;; [Number -> Number] Number  -> [List-of Number]
;; tabulates a number
(define (tab f n)
  (cond
   [(= n 0) (list (f n))]
   [else (cons
          (f n)
          (tab  f (sub1 n)))]))

;; Number -> [List-of Number]
(define (tab-sqr n)
  (tab sqrt n))

;; Number -> [List-of Number]
(define (tab-tan n)
  (tab tan n))


;;==============
;;251

;; [Number Number -> Number] Number [List-of Number] -> Number
(define (fold f i l)
  (cond
   [(empty? l) i]
   [else (f (first l)
            (fold f i (rest l)))]))

;; [List-of Number] -> Number
(check-expect (fold-plus '(1 2)) 3)
(define (fold-plus l)
  (fold + 0 l))

;; [List-of Number] -> Number
(check-expect (fold-prod '(1 2)) 2)
(define (fold-prod l)
  (fold * 1 l))

;;=====================
;;252

;; [Item Item -> Result] Result [List-of Item] -> Result
(define (fold2 f i l)
  (cond
   [(empty? l) i]
   [else (f (first l)
            (fold2 f i (rest l)))]))
