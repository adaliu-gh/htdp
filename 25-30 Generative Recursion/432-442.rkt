;;================================
;; 432

(define MAX 100000000)

;; Posn -> Posn
(define (food-create p)
  (local ((define new-food (make-posn (random MAX) (random MAX))))
    (if (equal? new-food p)
        (food-create p)
        new-food)))

;;=============================
;; 433

;; [X] [List-of X] N -> [List-of [List-of X]]
;; bundle every n items together
(define (bundle-check alon n)
  (cond
   [(and (= n 0) (> (length alon) 0))
    (error "wrong input")]
   [else (bundle alon n)]))

;; [X] [List-of X] N -> [List-of [List-of X]]
(define (bundle alon n)
  (cond
   [(<= (length alon) n) alon]
   [else (cons (take alon n)
               (bundle (drop alon n) n))]))

;; [List-of Any] N -> [List-of Any]
;; extract the first n items from list
(define (take l n)
  (cond
   [(empty? l) '()]
   [(zero? n) '()]
   [else (cons (first l) (take (rest l) (sub1 n)))]))


;; [List-of Any] N -> [List-of Any]
;; remove the first n items from list l
(define (drop l n)
  (cond
   [(empty? l) '()]
   [(zero? n) l]
   [else (drop (rest l) (sub1 n))]))

;;=======================
;; 434

;; when in l, there are some numbers equal to n, then it forms an infinite loop

;;===========================
;; 436

;; terminate (food-create p) loops until the result is different from p

;;========================
;; 437

;; special computes the length of its input
(define (special.v1 p)
  (cond
   [(empty? p) 0]
   [else (add1 (special.v1 (rest p)))]))

;; special negates each number on the given list of number
(define (special.v2 p)
  (cond
   [(empty? p) '()]
   [else (cons (/ (first p) -1)
               (special.v2 (rest p)))]))


;;=============================
;; 442

;; [X] [List-of X] [X X -> Boolean] -> [List-of X]
;; sort the list according to the function f
(check-expect (quick-sort '( 11 8 14 7) <) '( 7 8 11 14))

(define (quick-sort alon f)
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
      (append (quick-sort leftside f)
              same
              (quick-sort rightside f)))]))

;; [List-of Number] -> [List-of Number]
(check-expect (sort< '( 11 8 14 7) ) '( 7 8 11 14))
(define (sort< alon)
  (local (;; Number [List-of Number] -> [List-of Number]
          ;; inserts a number into a sorted number in a descending order
          (define (insert n alon)
            (cond
             [(empty? alon) (list n)]
             [else (if (< n (first alon))
                       (cons n alon)
                       (cons (first alon) (insert n (rest alon))))])))
    (cond
     [(empty? alon) '()]
     [else (insert (first alon) (sort< (rest alon)))])))

;; Number -> [List-of Number]
;; create a large list of numbers randomly
(define (create-list n)
  (cond
   [(zero? n) '()]
   [else (cons (random MAX)
               (create-list (sub1 n)))]))
