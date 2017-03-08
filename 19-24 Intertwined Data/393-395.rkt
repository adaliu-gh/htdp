;;=====================
;;393

;; a Set-of-Number (Son)is one of:
;; - empty
;; - (cons Number Set-of-Number)
;; constrains: numbers in Son should not appear more than once
(define son-1 '(1 4 2 6))
(define son-2 '(43 12 56 4 6))

;; Son Son -> Boolean
;; check if the first son is in the second
(check-expect (in-set? '(1 2) '( 1 2 4)) #true)
(check-expect (in-set? '( 1 2) '( 1 3 4)) #false)
(define (in-set? sub sup)
  (foldl (lambda (x y) (and y (member? x sup))) #true sub))

;; List -> Boolean
;; check if the given list is a set
(check-expect (set? '( 1 2)) #true)
(check-expect (set? '( 1 2 2)) #false)
(define (set? l)
  (cond
   [(empty? l) #true]
   [else (if (member? (first l) (rest l))
             #false
             (set? (rest l)))]))

;; Son Son -> [Son -> Boolean]
(define (union? s1 s2)
  (lambda (s)
    (and (in-set? s1 s) (in-set? s2 s) (set? s))))

;; Son Son -> Son
;; make a union out of two sets
(check-satisfied (union son-1 son-2) (union? son-2 son-1))
(define (union s1 s2)
  (cond
   [(and (empty? s1) (empty? s2)) '()]
   [(empty? s1) s2]
   [(empty? s2) s1]
   [else (if (member? (first s2) s1)
             (union s1 (rest s2))
             (cons (first s2) (union s1 (rest s2))))]))

;; Son Son -> [Son -> Boolean]
;; this function is not complete
(define (intersect? s1 s2)
  (lambda (s)
    (and (in-set? s s1) (in-set? s s2) (set? s))))

;; Son Son -> Son
(check-satisfied (intersect son-2 son-1) (intersect? son-2 son-1))
(define (intersect s1 s2)
  (cond
   [(or (empty? s1) (empty? s2)) '()]
   [else (if (member? (first s1) s2)
             (cons (first s1) (intersect (rest s1) s2))
             (intersect (rest s1) s2))]))

(check-satisfied (intersect.v2 son-2 son-1) (intersect? son-2 son-1))
(define (intersect.v2 s1 s2)
  (foldl (lambda (x y) (if (member? x s2) (cons x y) y)) '() s1))


;;===========================
;;394

;; [List-of Number] -> Boolean
(check-expect (sort<? '( 1 2 2 3)) #true)
(check-expect (sort<? '( 1 3 2)) #false)
(define (sort<? l)
  (cond
   [(empty? l) #true]
   [(empty? (rest l)) #true]
   [else (if (not (> (first l) (first (rest l))))
             (sort<? (rest l)) #false)]))

;; [List-of Number] [List-of Number] -> [ [List-of Number] -> Boolean]
;; this function is not complete
(define (merge? l1 l2)
  (lambda (l)
    (and (in-set? l1 l) (in-set? l2 l) (sort<? l))))

;; Number [List-of Number] -> [List-of Number]
;; inserts a number into a sorted list of numbers
(define (insert n l)
  (cond
   [(empty? l) (list n)]
   [else (if (< n (first l))
             (cons n (cons (first l) (rest l)))
             (cons (first l) (insert n (rest l))))]))

;; [List-of Number] [List-of Number] -> [List-of Number]
;; merges two sorted list of numbers into one sorted list of numbers
(check-satisfied (merge '( 1 2 2 3 6 7) '( 2 3 3 4 6 7)) (merge? '( 1 2 2 3 6 7) '( 2 3 3 4 6 7)))
(define (merge l1 l2)
  (cond
   [(and (empty? l1) (empty? l2)) '()]
   [(empty? l2) l1]
   [(empty? l1) l2]
   [else (merge (rest l1) (insert (first l1) l2))]))

;;============================
;; 395

;; [List-of Number] Number -> [List-of Number]
;; extracts the first n numbers from the list or the whole list if the list is too short
(check-expect (take '() 2) '())
(check-expect (take '( 1 2 3) 1) '(1))
(check-expect (take '( 1 2 3) 2) '( 1 2))
(check-expect (take '( 1 2 3) 34) '( 1 2 3))
(define (take l n)
  (cond
   [(empty? l) l]
   [(= n 0) '()]
   [else (cons (first l) (take (rest l) (sub1 n)))]))

;; [List-of Number] Number -> [List-of Number]
;; removes the first n numbers from the list or all of them if the list is too short
(check-expect (drop '() 34) '())
(check-expect (drop '( 1 2) 34) '())
(check-expect (drop '( 1 2) 0) '(1 2))
(check-expect (drop '( 1 2 3) 2) '( 3))
(define (drop l n)
  (cond
   [(empty? l) '()]
   [(= n 0) l]
   [else (drop (rest l) (sub1 n))]))
