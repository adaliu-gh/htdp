;; N Number N -> Inex
;; make an instance of Inex after checking the arguments
(define (create-inex m s e)
  (cond
    [(and (<= 0 m 99) (<= 0 e 99) (or (= s 1) (= s -1)))
     (make-inex m s e)]
    [else (error "bad values given")]))

; Inex -> Number
; convert an inex into its numeric equivalent
(define (inex->number an-inex)
  (* (inex-mantissa an-inex)
     (expt
       10 (* (inex-sign an-inex) (inex-exponent an-inex)))))

(define-struct inex [mantissa sign exponent])
;; An Inex is a structure:
;;   (make-inex N99 S N99)
;; An S is one of:
;; – 1
;; – -1
;; An N99 is an N between 0 and 99 (inclusive).

;; =========================
;; 412

;; Inex Inex -> Inex
;; add two inexes
(check-expect (inex+ (create-inex 1 1 0) (create-inex 2 1 0)) (create-inex 3 1 0))
(check-expect (inex+ (create-inex 55 1 0) (create-inex 55 1 0)) (create-inex 11 1 1))
(check-expect (inex+ (create-inex 56 1 0) (create-inex 56 1 0)) (create-inex 11 1 1))

(define (inex+ n1 n2)
  (local ((define m1 (inex-mantissa n1))
          (define m2 (inex-mantissa n2))
          (define sign (inex-sign n1))
          (define expo (inex-exponent n1))
          (define possible-m (+ m1 m2))

          ;; Number -> Number
          ;; add1 expo if valid , or signal an error
          (define (add1-expo n)
            (if (<= -99 n 98) (add1 n) (error "out of range"))))
    (cond
     [(<= 0 possible-m 99)
      (make-inex possible-m sign expo)]
     [(> possible-m 99)
      (local ((define new-expo (add1-expo expo)))
        (make-inex (quotient possible-m 10)
                   (sgn new-expo) (abs new-expo)))])))

;; Inex Inex -> Inex
(check-expect (inex+.v2 (create-inex 1 1 0) (create-inex 1 -1 1)) (create-inex 11 -1 1))
(define ( inex+.v2 n1 n2 )
  (local ((define m1 (inex-mantissa n1))
          (define m2 (inex-mantissa n2))
          (define s1 (inex-sign n1))
          (define s2 (inex-sign n2))
          (define e1 (inex-exponent n1))
          (define e2 (inex-exponent n2)))
    (cond
     [(> (* s1 e1) (* s2 e2))
      (inex+ ( inex*n (make-inex m1 s2 e2) 10) n2)]
     [else
      (inex+ n1 (inex*n (make-inex m2 s1 e1) 10))])))

;; Inex N -> Inex
;; multiplay inex by n (>= n 0)
(define (inex*n inex n)
  (cond
   [(= n 0) (make-inex 0 1 0)]
   [(= n 1) inex]
   [else (inex+ inex (inex*n inex (sub1 n)))]))

