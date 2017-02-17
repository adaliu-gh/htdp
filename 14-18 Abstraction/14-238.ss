;;==========================
;;238

(define list1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
                    12 11 10 9 8 7 6 5 4 3 2 1))
(define list2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
                    17 18 19 20 21 22 23 24 25))

;; Function Nelon -> Number
;; extract a number from l
(define (extract f l)
  (cond
   [(empty? (rest l))
    (first l)]
   [else (if (f (first l)
                (extract f (rest l)))
             (first l)
             (extract f (rest l)))]))

;; Nelon -> Number
;;(check-expect (inf-1 list1) 1)
(define (inf-1 l)
  (extract < l))

;; Nelon -> Number
;;(check-expect (sup-1 list2) 25)
(define (sup-1 l)
  (extract > l))

;; Function Nelon -> Number
(define (extract2 f l)
  (cond
   [(empty? (rest l)) (first l)]
   [else (extract2 f
                   (cons (f (first l)
                            (first (rest l)))
                         (rest (rest l))))]))

;; Nelon -> Number
(check-expect (inf-2 list1) 1)
(define (inf-2 l)
  (extract2 min l))

;; Nelon -> Number
(check-expect (sup-2 list2) 25)
(define (sup-2 l)
  (extract2 max l))
