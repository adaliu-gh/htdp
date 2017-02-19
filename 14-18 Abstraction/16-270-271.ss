;;================
;;270

;; N -> [List-of N]
(check-expect (build-list1 3) '(0 1 2))
(define (build-list1 n)
  (local (;; N -> N
          (define (helper n)
            n))
    (build-list n helper)))

;; N -> [List-of N]
(check-expect (build-list2 3) '(1 2 3))
(define (build-list2 n)
  (local (;; N => N
          (define (helper n)
            (add1 n)))
    (build-list n helper)))

;; N -> [List-of N]
(check-expect (build-list3 3) '(1 1/2 1/3))
(define (build-list3 n)
  (local (;; N -> N
          (define (helper n)
            (/ 1 (add1 n))))
    (build-list n helper)))

;; N -> [List-of N]
(check-expect (build-list4 3) '(0 2 4))
(define (build-list4 n)
  (local (;; N -> N
          (define (helper n)
            (* 2 n)))
    (build-list n helper)))

;; N -> [List-of [List-of N]]
(check-expect (identityM 3) '((1 0 0)
                              (0 1 0)
                              (0 0 1)))
(define (identityM n)
  (local (;; the length of columns
          (define cols n)

          ;; N -> [List-of N]
          (define (generate-row n)
            (generate-row-helper n 0))

          ;; N N -> [List-of N]
          (define (generate-row-helper n i)
            (cond
             [(= i cols) '()]
             [else (cons (if (= n i) 1 0)
                         (generate-row-helper n (add1 i)))])))
    (build-list n generate-row)))

;; [Number -> Number] Number -> [List-of Number]
(define (tab f n)
  (build-list n f))


;;===========================
;;271

;; String [List-of String] -> Boolean
(check-expect (find-name? "ada" '("adaliuada")) #true)
(check-expect (find-name? "ada" '("b")) #false)
(define (find-name? name l)
  (local (;; the length of name
          (define len (string-length name))
          ;; String -> Boolean
          (define (equal-or-extend? s)
            (if (> (string-length s) len)
                (string=? name (substring s 0 len))
                #false)))
    (ormap equal-or-extend? l)))

;; [List-of String] -> Boolean
;; all strings start with "a"??
(check-expect (all-start-with-a? '("a" "ada")) #true)
(check-expect (all-start-with-a? '("b" "ada")) #false)
(define (all-start-with-a? l)
  (local (;; String -> Boolean
          (define (start-with-a? s)
            (string=? "a" (substring s 0 1))))
    (andmap start-with-a? l)))

;; Number [List-of String]-> Boolean
(check-expect (*no-longer-than? 2 '("a" "ad")) #true)
(check-expect (*no-longer-than? 2 '("abd" "ad")) #false)
(define (*no-longer-than? max-length l)
  (local (;; String -> Boolean
          (define (no-longer-than? s)
            (<= (string-length s) max-length)))
    (andmap no-longer-than? l)))
