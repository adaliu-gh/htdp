(require 2htdp/abstraction)
(define-struct add-expr [left right])
(define-struct mul-expr [left right])

;;=================================
;;345

;; (+ 10 -10) => (make-add-expr 10 -10)
;; (+ (* 20 3) 33) => (make-add-expr (make-mul-expr 20 3) 33)
;; (+ (* 3.14 (* 2 3)) (* 3.14 (* -1 -9))) => (make-add-expr (make-mul-expr 3.14 (make-mul-expr 2 3)) (make-mul-expr 3.14 (make-mul-expr -1 -9)))

;; (make-add-expr -1 2) => (+ 1 2)
;; (make-add-expr (make-mul-expr -2 -3) 33) => (+ (* -2 -3) 33)
;; (make-mul-expr (make-add-expr 1 (make-mul-expr 2 3)) 3.14) => (* (+ 1 (* 2 3)) 3.14)

;; Rerepsentation of a BSL expression -> Value
(check-expect (calculator 3) 3)
(check-expect (calculator (make-add-expr 1 1)) 2)
(check-expect (calculator (make-mul-expr 3 10)) 30)
(check-expect (calculator (make-add-expr (make-mul-expr 1 1) 10)) 11)
(define (calculator exp)
  (match exp
    [(add-expr left right) (+ (calculator left ) (calculator right ))]
    [(mul-expr left right) (* (calculator left ) (calculator right ))]
    [n n]))
;;===========================
;;346

(define-struct div-expr [top down])

;;=========================
;;348

;; #true : #true
;; #false : #false
(define-struct and-expr [first second])
(define-struct or-expr [first second])
(define-struct not-expr [boolean])

;; Rerepsentation -> Value
(check-expect (eval-bool-expression #true) #true)
(check-expect (eval-bool-expression (make-and-expr #true #false)) #false)
(check-expect (eval-bool-expression (make-or-expr (make-not-expr #true) #true)) #true)
(define (eval-bool-expression exp)
  (match exp
    [(and-expr first second)
     (and (eval-bool-expression first) (eval-bool-expression second))]
    [(or-expr first second)
     (or (eval-bool-expression first) (eval-bool-expression second))]
    [(not-expr boolean) (not (eval-bool-expression boolean ))]
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
(check-expect (parse '(+ 3 3)) (make-add-expr 3 3))
(check-expect (parse '(+ 3 (* 3 3))) (make-add-expr 3 (make-mul-expr 3 3)))
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
        (make-add-expr (parse (second s)) (parse (third s)))]
       [(symbol=? (first s) '*)
        (make-mul-expr (parse (second s)) (parse (third s)))]
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

;;==========================
;;352

;; A BSL-var-expr is one of:
;; - Number
;; - Symbol
;; - (make-add-expr BSL-var-expr BSL-var-expr)
;; - (make-mul-expr BSL-var-expr BSL-var-expr)

;; BSL-var-expr Symbol Number -> BSL-var-expr
(check-expect (subst 3 'x 1) 3)
(check-expect (subst 'y 'x 1) 'y)
(check-expect (subst 'x 'x 1) 1)
(check-expect (subst (make-add-expr 'x 2) 'x 1) (make-add-expr 1 2))
(check-expect (subst (make-add-expr (make-add-expr 'x 2) 2) 'x 1)
              (make-add-expr (make-add-expr 1 2) 2))
(define (subst ex x v)
  (match ex
    [(add-expr left right)
     (make-add-expr (subst left x v) (subst right x v))]
    [(mul-expr left right)
     (make-mul-expr (subst left x v) (subst right x v))]
    [(? (lambda (i) (equal? i x))) v]
    [else ex]))

;;============================
;;353

;; BSL-var-expr -> Boolean
;; determines if the given ex is also a BSL-expr
(check-expect (numeric? 1) #true)
(check-expect (numeric? 'x) #false)
(check-expect (numeric? (make-add-expr 'x 1)) #false)
(check-expect (numeric? (make-add-expr (make-add-expr 1 2) 3)) #true)
(define (numeric? ex)
  (match ex
    [(add-expr left right)
     (and (numeric? left) (numeric? right))]
    [(mul-expr left right)
     (and (numeric? left) (numeric? right))]
    [(? number?) #true]
    [(? symbol?) #false]))

;;==============================
;;354


;; BSL-var-expr -> Number
(check-expect (eval-variable 1) 1)
(check-expect (eval-variable (make-add-expr 1 1)) 2)
(check-error (eval-variable 'x) WRONG)
(define (eval-variable ex)
  (if (numeric? ex)
      (calculator ex)
      (error WRONG)))

;; an AL (short for association list) is [List-of Association]
;; an Association is a list of two items:
;; (cons Symbol (cons Number '()))

(define one-al '((x 1) (y 2) (z 3)))

;; BSL-var-expr AL -> Number
(check-expect (eval-variable* (make-add-expr 'x 'y) one-al) 3)
(check-error (eval-variable* (make-add-expr 'a 'b) one-al) WRONG)
(define (eval-variable* ex da)
  (eval-variable
   (foldl (lambda (a b) (subst b (first a) (second a))) ex da)))

;;============================
;;355

;; BSL-var-expr AL -> Number
(check-expect (eval-variable* (make-add-expr 'x 'y) one-al) 3)
(check-error (eval-variable* (make-add-expr 'a 'b) one-al) WRONG)
(define (eval-var-lookup e da)
  (local (;; Symbol -> Number
          (define (find-the-value s da)
            (cond
             [(empty? da) (error WRONG)]
             [else (if (equal? s (first (first da)))
                       (second (first da))
                       (find-the-value s (rest da)))])))
  (match e
    [(add-expr left right)
     (+ (eval-var-lookup left da) (eval-var-lookup right da))]
    [(mul-expr left right)
     (* (eval-var-lookup left da) (eval-var-lookup right da))]
    [(? number?) e]
    [(? symbol?) (find-the-value e da)])))
