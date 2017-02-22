;;=====================
;;262

;; N -> [List-of [List-of N]]
(check-expect (identityM 1) (list (list 1)))
(check-expect (identityM 3) '((1 0 0)
                              (0 1 0)
                              (0 0 1)))
(define (identityM n)
  (local (;; the length of columns
          (define cols n)

          ;; N -> [List-of [List-of N]]
          (define (generate-matrix n)
            (cond
             [(zero? n) '()]
             [else (cons (generate-row n cols)
                         (generate-matrix (- n 1)))]))

          ;; N N -> [List-of N]
          (define (generate-row n len)
            (cond
             [(zero? len) '()]
             [else (cons (if (= n len) 1 0)
                         (generate-row n (- len 1)))])))
    (generate-matrix n)))
