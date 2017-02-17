;;===========================

; String Los -> Boolean
; determines whether l contains the string s
(define (contains? s l)
  (cond
    [(empty? l ) #false]
    [else (or (string=? (first l) s)
              (contains? s (rest l)))]))

(define one-list (list "atom" "zoo" "dog"))
;;=============================
;;235

; Los -> Boolean
(check-expect (contains-atom? one-list) #true)
(define (contains-atom? l)
  (contains? "atom" l))

; Los -> Boolean
(check-expect (contains-basic? one-list) #false)
(define (contains-basic? l)
  (contains? "basic" l))

; Los -> Boolean
(check-expect (contains-zoo? one-list) #true)
(define (contains-zoo? l)
  (contains? "zoo" l))

;;==========================
;; 236

;; Lon -> Lon
;; add 1 to each item on l
(check-expect (add1* '( 1 2 3)) '( 2 3 4))
(define (add1* l)
  (cond
   [(empty? l) '()]
   [else
    (cons (add1 (first l))
          (add1* (rest l)))]))


;; Lon -> Lon
;; adds 5 to each item on 1
(check-expect (plus5 '(0 1)) '(5 6))
(define (plus5 l)
  (cond
   [(empty? l ) '()]
   [else (cons (+ (first l) 5)
               (plus5 (rest l)))]))

;;=================
;; abstraction

;; Number Lon -> Lon
(define (plus* n l)
  (cond
   [(empty? l) '()]
   [else (cons (+ (first l) n)
               (plus* n (rest l)))]))

;; Lon -> Lon
;; add 1 to each item on l
(check-expect (plus-1 '( 1 2 3)) '( 2 3 4))
(define (plus-1 l)
  (plus* 1 l))

;; Lon -> Lon
(check-expect (plus-5 '(0 5)) '(5 10))
(define (plus-5 l)
  (plus* 5 l))

