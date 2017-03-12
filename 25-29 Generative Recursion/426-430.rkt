;;===========================
;; 426

;; [List-of Number] Number -> [List-of Number]
(check-expect (quick-sort< '( 11  8 14 7)) '( 7 8 11 14))
(define (quick-sort< alon)
  (cond
   [(empty? alon) '()]
   [(= 1 (length alon)) alon]
   ;;[(<= (length alon) threshold) (sort< threshold)]
   [else (local ((define pivot (first alon)))
           (append (quick-sort< (smallers alon pivot))
                   (equal alon pivot)
                   (quick-sort< (largers alon pivot))))]))

;; [List-of Number] Number -> [List-of Number]
(define (largers alon n)
  (filter (lambda (x) (> x n)) alon))

;; [List-of Number] Number -> [List-of Number]
(define (smallers alon n)
  (filter (lambda (x) (< x n)) alon))

;; [List-of Number] Number -> [List-of Number]
;; extract all ns from l
(define (equal alon n)
  (filter (lambda (x) (= x n)) alon))


;;======================
;; 430

;; [X] [List-of X] [X X -> Boolean] -> [List-of X]
;; sort the list according to the function f
(check-expect (my-sort '( 11 8 14 7) <) '( 7 8 11 14))

(define (my-sort alon f)
    (cond
     [(empty? alon) '()]
     [(= 1 (length alon)) alon]
     [else
      (local ((define pivot (first alon))
              (define leftside (filter (lambda (x) (f x pivot)) alon))
              (define rightside (filter (lambda (x)
                                          (not (or (f x pivot)
                                                   (equal? x pivot)))) alon))
              (define same (filter (lambda (x) (equal? x pivot)) alon)))
        (append (my-sort leftside f)
                same
                (my-sort rightside f)))]))
