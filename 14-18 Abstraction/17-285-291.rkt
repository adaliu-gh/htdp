
;;===================
;;285

;; [List-of Number] -> [List-of Number]
(check-expect (convert-euro '(1 2))
              '(1.22 2.44))
(define (convert-euro l)
  (map (lambda (x) (* 1.22 x)) l))

;; [List-of Posn] -> [List-of [List-of 2N]]
(check-expect (translate (list (make-posn 12 23) (make-posn 10 45)))
              '((12 23) (10 45)))
(define (translate l)
  (map (lambda (p) (list (posn-x p) (posn-y p))) l))


;;=====================
;;286

;; Inventory is:
(define-struct inv [name des in-pri sale-pri])
;; name: String - name of the product
;; des: String - description of the product
;; in-pri: Number - the price when acquired
;; sale-pri: Number - recommended price of sales
(define one-list-inv (list (make-inv "a" "a" 12 25)
                           (make-inv "a" "a" 12 13)
                           (make-inv "b" "b" 34 45)))

;; [List-of Inventory] -> [List-of Inventory]
(check-expect (sort-inv one-list-inv)
              (list (make-inv "a" "a" 12 13)
                    (make-inv "b" "b" 34 45)
                    (make-inv "a" "a" 12 25)))
(define (sort-inv l)
  (local (;; Inventory -> Number
          (define (diff-between-prices i)
            (abs (- (inv-in-pri i) (inv-sale-pri i)))))
  (sort l (lambda (x y) (< (diff-between-prices x) (diff-between-prices y))))))

;;=====================
;;287

;; N [List-of Inventory] -> [List-of Inventory]
(check-expect (eliminate-expensive 40 one-list-inv)
              (list (make-inv "a" "a" 12 25)
                    (make-inv "a" "a" 12 13)))
(define (eliminate-expensive ua l)
  (filter (lambda (x) (< (inv-sale-pri x) ua)) l))

;; Inventory [List-of Inventory] -> [List-of Inventory]
(check-expect (recall (first one-list-inv) one-list-inv)
              (list (make-inv "b" "b" 34 45)))
(define (recall ty l)
  (local ((define yardstick (inv-name ty)))
    (filter (lambda (i) (not (string=? (inv-name i) yardstick))) l)))

;; [List-of String] [List-of String] -> [List-of String]
(check-expect (selection '(1 2) '(2 3 4))
              '(2))
(define (selection l1 l2)
  (filter (lambda (x) (member? x l1)) l2))

;;============================
;;288

;; N -> [List-of N]
(check-expect (build-list1 3) '(0 1 2))
(define (build-list1 n)
  (build-list n (lambda (x) x)))

;; N -> [List-of N]
(check-expect (build-list2 3) '(1 2 3))
(define (build-list2 n)
  (build-list n (lambda (x) (add1 x))))

;; N -> [List-of N]
(check-expect (build-list3 3) '(1 1/2 1/3))
(define (build-list3 n)
  (build-list n (lambda (x) (/ 1 (add1 x)))))

;; N -> [List-of N]
(check-expect (build-list4 3) '(0 2 4))
(define (build-list4 n)
  (build-list n (lambda (x) (* 2 x))))

;; N -> [List-of [List-of N]]
(check-expect (identityM 3) '((1 0 0)
                              (0 1 0)
                              (0 0 1)))
(define (identityM n)
  (local ((define cols n)
          ;; N N -> [List-of N]
          (define (generate-row-helper n i)
            (cond
             [(= i cols) '()]
             [else (cons (if (= n i) 1 0)
                         (generate-row-helper n (add1 i)))])))
  (build-list n (lambda (x) (generate-row-helper x 0)))))

;; [Number -> Number] Number -> [List-of Number]
(define tab (lambda (f n) (build-list n f)))

;;==========================
;;289

;; String [List-of String] -> Boolean
(check-expect (find-name? "ada" '("adaliuada")) #true)
(check-expect (find-name? "ada" '("b")) #false)
(define (find-name? name l)
  (local ((define len (string-length name)))
    (ormap (lambda (s) (if (>= (string-length s) len)
                           (string=? name (substring s 0 len))
                           #false)) l)))

;; [List-of String] -> Boolean
;; all strings start with "a"??
(check-expect (all-start-with-a? '("a" "ada")) #true)
(check-expect (all-start-with-a? '("b" "ada")) #false)
(define (all-start-with-a? l)
  (andmap (lambda (x) (string=? "a" (substring x 0 1))) l))

;; Number [List-of String]-> Boolean
(check-expect (*no-longer-than? 2 '("a" "ad")) #true)
(check-expect (*no-longer-than? 2 '("abd" "ad")) #false)
(define (*no-longer-than? max-length l)
  (andmap (lambda (x) (<= (string-length x) max-length)) l))

;;==============================
;;290

;; [X] [List-of X] [List-of X] -> [List-of X]
(check-expect (append-from-fold '(1 2) '(2 3)) (append '(1 2) '(2 3)))
(define (append-from-fold l1 l2)
  (foldr (lambda (x y) (cons x y)) l2 l1))

;; [List-of Number] -> Number
(check-expect (sum-from-fold '( 1 2)) 3)
(define (sum-from-fold l)
  (foldr (lambda (x y) (+ x y)) 0 l))

;; [List-of Number] -> Number
(check-expect (product-from-fold '( 1 2)) 2)
(define (product-from-fold l)
  (foldr (lambda (x y) (* x y)) 1 l))

;;==========================
;;291

;; [X Y] [X -> Y] [List-of X] -> [List-of Y]
(check-expect (map-from-fold add1 '(1 2))
              (map add1 '( 1 2)))
(define (map-from-fold f l)
  (foldr (lambda (x y) (cons (f x) y)) '() l))

