(require 2htdp/abstraction)
(define-struct add-exp [left right])
(define-struct mul-exp [left right])

;;=================================
;;345

;; (+ 10 -10) => (make-add-exp 10 -10)
;; (+ (* 20 3) 33) => (make-add-exp (make-mul-exp 20 3) 33)
;; (+ (* 3.14 (* 2 3)) (* 3.14 (* -1 -9))) => (make-add-exp (make-mul-exp 3.14 (make-mul-exp 2 3)) (make-mul-exp 3.14 (make-mul-exp -1 -9)))

;; (make-add-exp -1 2) => (+ 1 2)
;; (make-add-exp (make-mul-exp -2 -3) 33) => (+ (* -2 -3) 33)
;; (make-mul-exp (make-add-exp 1 (make-mul-exp 2 3)) 3.14) => (* (+ 1 (* 2 3)) 3.14)

;; Rerepsentation of a BSL expression -> Value
(check-expect (calculator 3) 3)
(check-expect (calculator (make-add-exp 1 1)) 2)
(check-expect (calculator (make-mul-exp 3 10)) 30)
(check-expect (calculator (make-add-exp (make-mul-exp 1 1) 10)) 11)
(define (calculator exp)
  (match exp
    [(add-exp left right) (+ (calculator left ) (calculator right ))]
    [(mul-exp left right) (* (calculator left ) (calculator right ))]
    [n n]))
;;===========================
;;346

(define-struct div-exp [top down])

;;=========================
;;348

;; #true : #true
;; #false : #false
(define-struct and-exp [first second])
(define-struct or-exp [first second])
(define-struct not-exp [boolean])

;; Rerepsentation -> Value
(check-expect (eval-bool-expression #true) #true)
(check-expect (eval-bool-expression (make-and-exp #true #false)) #false)
(check-expect (eval-bool-expression (make-or-exp (make-not-exp #true) #true)) #true)
(define (eval-bool-expression exp)
  (match exp
    [(and-exp first second)
     (and (eval-bool-expression first) (eval-bool-expression second))]
    [(or-exp first second)
     (or (eval-bool-expression first) (eval-bool-expression second))]
    [(not-exp boolean) (not (eval-bool-expression boolean ))]
    [boolean boolean]))

;;=================================
;;349

(define WRONG "wrong expression")

;; Any -> Boolean
;; determines if the given thing is an atom
(define (atom? x)
  (or (string? x)
      (number? x)
      (symbol? x)))

(check-expect (parse 3) 3)
(check-expect (parse '(+ 3 3)) (make-add-exp 3 3))
(check-expect (parse '(+ 3 (* 3 3))) (make-add-exp 3 (make-mul-exp 3 3)))
;; S-expr -> BSL-expr
(define (parse s)
  (cond
   [(atom? s) (parse-atom s)]
   [else (parse-sl s)]))

;; SL -> BSL-expr
(define (parse-sl s)
  (local ((define L (length s)))
    (cond
     [(< L 3) (error WRONG)]
     [(and (= L 3) (symbol? (first s)))
      (cond
       [(symbol=? (first s) '+)
        (make-add-exp (parse (second s)) (parse (third s)))]
       [(symbol=? (first s) '*)
        (make-mul-exp (parse (second s)) (parse (third s)))]
       [else (error WRONG)])]
     [else (error WRONG)])))

;; Atom -> BSL-expr
(define (parse-atom s)
  (cond
   [(number? s) s]
   [(string? s) (error WRONG)]
   [(symbol? s) (error WRONG)]))

;;===============================
;;351

;; S-expr -> Value
(define (interpreter-expr sexp)
  (calculator (parse sexp)))
