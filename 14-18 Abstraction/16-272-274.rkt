;;===================
;;272

;; [X] [List-of X] [List-of X] -> [List-of X]
(check-expect (append-from-fold '(1 2) '(2 3)) (append '(1 2) '(2 3)))
(define (append-from-fold l1 l2)
    (foldr cons l2 l1 ))

;; [List-of Number] -> Number
(check-expect (sum-from-fold '( 1 2)) 3)
(define (sum-from-fold l)
  (foldr + 0 l))

;; [List-of Number] -> Number
(check-expect (product-from-fold '( 1 2)) 2)
(define (product-from-fold l)
  (foldr * 1 l))


;;=======================
;;273

;; [X Y] [X -> Y] [List-of X] -> [List-of Y]
(check-expect (map-from-fold add1 '(1 2))
              (map add1 '( 1 2)))
(define (map-from-fold f l)
  (local (;; X [List-of Y] -> [List-of Y]
          (define (join x ly)
            (cons (f x) ly)))
    (foldr join '() l)))


;;=====================
;;274

;; String -> [List-of String]
(check-expect (prefixes "ada") '("a" "ad" "ada"))
(define (prefixes s)
  (local (;; Number -> String
          (define (get-prefix n)
            (substring s 0 (add1 n))))
    (build-list  (string-length s) get-prefix)))

;; String -> [List-of String]
(check-expect (suffixes "ada") '("ada" "da" "a"))
(define (suffixes s)
  (local ((define len (string-length s))
          ;; Number -> String
          (define (get-suffix n)
            (substring s n len)))
    (build-list len get-suffix)))
