(require 2htdp/abstraction)
;; BSL-var-expr Symbol Number -> BSL-var-expr
(define (subst ex x v)
  (match ex
    [(add-expr left right)
     (make-add-expr (subst left x v) (subst right x v))]
    [(mul-expr left right)
     (make-mul-expr (subst left x v) (subst right x v))]
    [(? (lambda (i) (equal? i x))) v]
    [else ex]))

;;=============================
;;356

(define-struct add-expr [left right])
(define-struct mul-expr [left right])
(define-struct fun-expr [name arg])
(define WRONG "wrong expression")
;; a BSL-fun-expr is one of:
;; - Number
;; - Symbol
;; - (make-add-expr BSL-fun-expr)
;; - (make-mul-expr BSL-fun-expr)
;; - (make-fun-expr Symbol BSL-fun-expr)


;;==============================
;;357

;; BSL-fun-expr Symbol Symbol BSL-fun-expr -> Number
(check-expect (eval-definition1 (make-fun-expr 'k (make-add-expr 1 1)) 'k 'x (make-mul-expr 'x 'x)) 4)
(define  (eval-definition1 ex f x b)
  (match ex
    [(? number?) ex]
    [(add-expr left right)
     (+ (eval-definition1 left f x b) (eval-definition1 right f x b))]
    [(mul-expr left right)
     (* (eval-definition1 left f x b) (eval-definition1 right f x b))]
    [(fun-expr s exp)
     (if (equal? s f)
         (eval-definition1
          (subst b x (eval-definition1 exp f x b)) f x b)
         (error WRONG))]))

;;==============================
;;358

(define-struct fun-def [name para body])
;; a BSL-fun-def is:
;; (make-fun-def Symbol Symbol BSL-fun-expr)
;; ex:
(define f (make-fun-def 'f 'x (make-add-expr 3 'x)))
(define g (make-fun-def 'g 'y (make-fun-expr 'f (make-mul-expr 2 'y))))
(define h (make-fun-def 'h 'v (make-add-expr (make-fun-expr 'f 'v) (make-fun-expr 'g 'v))))

;; a BSL-fun-def* is [List-of BSL-fun-def]
(define da-fgh (list f g h))

;; BSL-fun-def* Symbol -> BSL-fun-def
;; retrives the definition of f in da
;; signals an error is there is none
(check-expect (lookup-def da-fgh 'g) g)
(check-error (lookup-def da-fgh 't) WRONG)
(define (lookup-def da f)
  (cond
   [(empty? da) (error WRONG)]
   [else (if (equal? f (fun-def-name (first da)))
             (first da)
             (lookup-def (rest da) f))]))
