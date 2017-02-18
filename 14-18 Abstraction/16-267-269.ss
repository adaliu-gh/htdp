;;=======================
;;267

;; [List-of N] -> [List-of N]
(check-expect (convert-euro '(1 2))
              '(1.22 2.44))
(define (convert-euro l)
  (local (;; N -> N
          (define (convert-1 n)
            (* 1.22 n)))
    (map convert-1 l)))


;; [List-of Posn] -> [List-of [List-of 2N]]
(check-expect (translate (list (make-posn 12 23) (make-posn 10 45)))
              '((12 23) (10 45)))
(define (translate l)
  (local (;; Posn -> [List-of 2N]
          (define (translate-1 p)
            (list (posn-x p) (posn-y p))))

    (map translate-1 l)))

;;====================
;;268

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
  (local (;; Inventory Inventory -> Boolean
          (define (sort-between-2 a b)
            (<= (diff-between-prices a)
               (diff-between-prices b)))

          ;; Inventory -> N
          (define (diff-between-prices i)
            (abs (- (inv-in-pri i)
                    (inv-sale-pri i)))))

    (sort l sort-between-2)))

;;=========================
;;269

;; N [List-of Inventory] -> [List-of Inventory]
(check-expect (eliminate-expensive 40 one-list-inv)
              (list (make-inv "a" "a" 12 25)
                    (make-inv "a" "a" 12 13)))
(define (eliminate-expensive ua l)
  (local (;; Inventory -> Boolean
          (define (not-expensive? i)
            (< (inv-sale-pri i) ua)))
    (filter not-expensive? l)))

;; Inventory [List-of Inventory] -> [List-of Inventory]
(check-expect (recall (first one-list-inv) one-list-inv)
              (list (make-inv "b" "b" 34 45)))
(define (recall ty l)
  (local (;; Inventory -> Boolean
          (define (!same-name? i)
            (not (string=? (inv-name i) (inv-name ty)))))
    (filter !same-name? l)))

;; [List-of String] [List-of String] -> [List-of String]
(check-expect (selection '(1 2) '(2 3 4))
              '(2))
(define (selection l1 l2)
  (local (;; String -> Boolean
          (define (in-l1? s)
            (member? s l1)))
    (filter in-l1? l2)))
