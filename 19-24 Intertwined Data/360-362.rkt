(require 2htdp/abstraction)

;;=========================
;;360

(define-struct add-expr [left right])
(define-struct mul-expr [left right])
(define-struct fun-expr [name argu])
;; a BSL-expr is one of:
;; - Number
;; - Symbol
;; - (make-add-expr BSL-expr)
;; - (make-mul-expr BSL-expr)
;; - (make-fun-expr Symbol BSL-expr)

(define-struct con [name value])
(define-struct fun [name para body])

;; a BSL-da is one of:
;; - (make-con Symbol Number)
;; - (make-fun Symbol Symbol BSL-expr)
(define con1 (make-con 'x 1))
(define con2 (make-con 'y 2))
(define f (make-fun 'f 'x (make-add-expr 3 'x)))
(define g (make-fun 'g 'y (make-fun-expr 'f (make-mul-expr 2 'y))))
(define h (make-fun 'h 'v (make-add-expr (make-fun-expr 'f 'v) (make-fun-expr 'g 'v))))

;; a BSL-da-all is [List-of BSL-da]
(define one-da (list con1 con2 f g h))

(define CON-ERROR "NO CONSTANT DEFINITION FOUND")

;; BSL-da-all Symbol -> Number
(check-expect (lookup-con-def one-da 'x) con1)
(check-error (lookup-con-def one-da 'f) CON-ERROR)
(define (lookup-con-def da x)
  (cond
   [(empty? da) (error CON-ERROR)]
   [else (if (and (con? (first da))
                  (equal? x (con-name (first da))))
             (first da)
             (lookup-con-def (rest da) x))]))

(define FUN-ERROR "NO FUNCTION DEFINITION FOUND")
;; BSL-da-all Symbol -> [BSL-da fun]
(check-expect (lookup-fun-def one-da 'f) f)
(check-error (lookup-fun-def one-da 'x) FUN-ERROR)
(define (lookup-fun-def da x)
  (cond
   [(empty? da) (error FUN-ERROR)]
   [else (if (and (fun? (first da))
                  (equal? x (fun-name (first da))))
             (first da)
             (lookup-fun-def (rest da) x))]))

;;============================
;;391

;; BSL-var-expr Symbol Number -> BSL-var-expr
(define (subst ex x v)
  (match ex
    [(add-expr left right)
     (make-add-expr (subst left x v) (subst right x v))]
    [(mul-expr left right)
     (make-mul-expr (subst left x v) (subst right x v))]
    [(fun-expr  f argu)
     (make-fun-expr  f (subst argu x v))]
    [(? (lambda (i) (equal? i x))) v]
    [else ex]))

;; BSL-expr BSL-da-all -> Number
(check-expect (eval-all 1 one-da) 1)
(check-expect (eval-all 'x one-da) 1)
(check-expect (eval-all (make-add-expr 'x 1) one-da) 2)
(check-expect (eval-all (make-fun-expr 'h 'y) one-da) 12)
(define (eval-all ex da)
  (match ex
    [(? number?) ex]
    [(? symbol?) (con-value (lookup-con-def da ex))]
    [(add-expr left right)
     (+ (eval-all left da) (eval-all right da))]
    [(mul-expr left right)
     (* (eval-all left da) (eval-all right da))]
    [(fun-expr  f argu)
     (local ((define function (lookup-fun-def da f)))
       (eval-all
        (subst (fun-body function) (fun-para function)
               (eval-all argu da)) da))]))

;;==============================
;;392

;; S-expr -> BSL-expr
(check-expect (parse-expr 1) 1)
(check-expect (parse-expr 'x) 'x)
(check-expect (parse-expr '(+ 1 1)) (make-add-expr 1 1))
(check-expect (parse-expr '(f x)) (make-fun-expr 'f 'x))
(define (parse-expr sexp)
  (match sexp
    [(? number?) sexp]
    [(? symbol?) sexp]
    [(list '+  f s)
     (make-add-expr (parse-expr f) (parse-expr s) )]
    [(list '* f s)
     (make-mul-expr (parse-expr f) (parse-expr s) )]
    [(list s b)
     (make-fun-expr s (parse-expr b))]))

;; SL -> BSL-da-all
(check-expect (parse-da (list '(define x 1))) (list (make-con 'x 1)))
(check-expect (parse-da (list '(define (f x) (+ 3 x)))) (list (make-fun 'f 'x (make-add-expr 3 'x))))
(check-expect (parse-da '((define x 1) (define (f x) (+ 3 x)))) (list (make-con 'x 1) (make-fun 'f 'x (make-add-expr 3 'x))))
(define (parse-da sl)
  (match sl
    [(? empty?) '()]
    [(cons (list 'define (list s para) body) rest-sl)
     (cons (make-fun s para (parse-expr body)) (parse-da rest-sl))]
    [(cons (list 'define s v) rest-sl)
     (cons (make-con s (parse-expr v)) (parse-da rest-sl))]
    ))

(define sl1 '( (define x 1) (define y 2)
               (define (f x) (+ 3 x)) (define (g x) (f (* 2 x)))
               (define (h x) (+ (f x) (g x)))))
;; S-expr SL -> Number
(check-expect (interpreter 1 sl1) 1)
(check-expect (interpreter 'x sl1) 1)
(check-expect (interpreter '(f y) sl1) 5)
(check-expect (interpreter '(h y) sl1) 12)
(define (interpreter sexp sl)
  (eval-all (parse-expr sexp) (parse-da sl) ))
