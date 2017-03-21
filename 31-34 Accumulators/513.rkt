
;; a Lam is one of:
;; - a Symbol
;; - (list 'λ (list Symbol) Lam)
;; - (list Lam Lam)
(define ex1 '(λ (x) x))
(define ex2 '(λ (x) y))
(define ex3 '(λ (y) (λ (x) y)))
(define ex4 '((λ (x) (x x)) (λ (x) (x x))))
(define ex5 'x)

;;===================
;; 513

;; Lam -> Boolean
;; checks if lam is a varible
(check-expect (is-var? ex5) #true)
(check-expect (is-var? ex1) #false)
(check-expect (is-var? ex4) #false)
(define (is-var? lam)
  (symbol? lam))

;; Lam -> Boolean
;; checks if lam is a λ
(check-expect (is-λ? ex1) #true)
(check-expect (is-λ? ex4) #false)
(check-expect (is-λ? ex5) #false)
(define (is-λ? lam)
  (and (cons? lam)
       (equal? (first lam) 'λ)))

;; Lam -> Boolean
;; checks if lam is an application
(check-expect (is-app? ex1) #false)
(check-expect (is-app? ex4) #true)
(check-expect (is-app? ex5) #false)
(define (is-app? lam)
  (and (cons? lam)
       (not (equal? (first lam) 'λ))))

;; Lam -> [List-of Symbol]
;; extract the parameter from a λ expression
(check-expect (λ-para ex1) '(x))
(check-error (λ-para ex4) "wrong")
(define (λ-para lam)
  (cond
   [(is-λ? lam)
    (second lam)]
   [else (error "wrong")]))


;; Lam -> Lam
;; extract the body from a λ expression
(check-expect (λ-body ex1) 'x)
(check-error (λ-body ex4) "wrong")
(define (λ-body lam)
  (cond
   [(is-λ? lam)
    (second (rest lam))]
   [else (error "wrong")]))

;; Lam -> Lam
;; extract the function from an application
(check-expect (app-fun ex4) '(λ (x) (x x)))
(check-error (app-fun ex1) "wrong")
(define (app-fun lam)
  (cond
   [(is-app? lam)
    (first lam)]
   [else (error "wrong")]))


;; Lam -> Lam
;; extract the argument from an application
(check-expect (app-arg ex4) '(λ (x) (x x)))
(check-error (app-arg ex1) "wrong")
(define (app-arg lam)
  (cond
   [(is-app? lam)
    (second lam)]
   [else (error "wrong")]))

;; Lam -> [List-of Symbol]
;; extract the list of all symbols used as parameters
(check-expect (declareds ex1) '(x))
(check-expect (declareds ex3) '(y x))
(check-expect (declareds ex4) '(x x))
(define (declareds lam)
  (cond
   [(is-var? lam) '()]
   [(is-λ? lam) (append (λ-para lam)
                        (declareds (λ-body lam)))]
   [(is-app? lam) (append (declareds (app-fun lam))
            (declareds (app-arg lam)))]))
