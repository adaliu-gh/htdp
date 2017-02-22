(require 2htdp/abstraction)
;;========================
;;305

;; [List-of Number] -> [List-of Number]
(check-expect (convert-euro '(1 2))
              '(1.22 2.44))
(define (convert-euro l)
  (for/list ( [x l] ) (* 1.22 x)))

;;=======================
;;306

;; N -> [List-of N]
(check-expect (build-list1 3) '(0 1 2))
(define (build-list1 n)
  (for/list ( [i n] ) i))

;; N -> [List-of N]
(check-expect (build-list2 3) '(1 2 3))
(define (build-list2 n)
  (for/list ( [i n] ) (add1 i)))

;; N -> [List-of N]
(check-expect (build-list3 3) '(1 1/2 1/3))
(define (build-list3 n)
  (for/list ( [i n] ) (/ 1 (add1 i))))

;; N -> [List-of N]
(check-expect (build-list4 3) '(0 2 4))
(define (build-list4 n)
  (for/list ( [i n] ) (* 2 i)))

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
  (for/list ( [i n] ) (generate-row-helper i 0))))

;;========================
;;307

;; String [List-of String] -> Boolean
(check-expect (find-name? "ada" '("adaliuada")) #true)
(check-expect (find-name? "ada" '("b")) #false)
(define (find-name? name l)
  (local ((define len (string-length name)))
    (for/or ([i l]) (if (>= (string-length i) len)
                        (string=? name (substring i 0 len))
                        #false))))

;; Number [List-of String]-> Boolean
(check-expect (*no-longer-than? 2 '("a" "ad")) #true)
(check-expect (*no-longer-than? 2 '("abd" "ad")) #false)
(define (*no-longer-than? max-length l)
  (for/and ([i l]) (<= (string-length i) max-length)))
