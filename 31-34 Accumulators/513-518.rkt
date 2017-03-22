
;; a Lam is one of:
;; - a Symbol
;; - (list 'λ (list Symbol) Lam)
;; - (list Lam Lam)
(define ex1 '(λ (x) x))
(define ex2 '(λ (x) y))
(define ex3 '(λ (y) (λ (x) y)))
(define ex4 '((λ (x) (x x)) (λ (x) (x x))))
(define ex5 'x)
(define ex6 '((λ (x) ( (λ (y) (y x)) x)) (λ (z) z)))

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

;;====================
;; 515

;; '(x (λ (x) (x x)) )

;;=======================
;; 516


;; Lam -> Lam
;; replace all symbols s in le with '*undeclared
;; if they do not occur within the body of a λ
;; expression whose parameter is s

(check-expect (undeclareds ex2) '(λ (x) *undeclared:y))
(check-expect (undeclareds ex1) '(λ (x) *declared:x))


(define (undeclareds le0)
  (local (;; String Symbol -> Symbol
          (define (declare key s)
            (string->symbol
             (string-append key ":"
                            (symbol->string s))))

          ;; Lam [List-of Symbol] -> Lam
          ;; accumulator declareds is a list of all λ
          ;; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
             [(is-var? le)
              (if (member? le declareds)
                  (declare "*declared" le)
                  (declare "*undeclared" le))]
             [(is-λ? le)
              (local ((define para (λ-para le))
                      (define body (λ-body le))
                      (define newd (append para declareds)))
                (list 'λ para
                      (undeclareds/a body newd)))]
             [(is-app? le)
              (local ((define fun (app-fun le))
                      (define arg (app-arg le)))
                (list (undeclareds/a fun declareds)
                      (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))


;;=======================
;; 518

;; Lam -> Lam
;; replace all symbols with its distance from the root le0
(check-expect (static-distance ex6)
              '((λ (x) ( (λ (y) (0 1)) 0)) (λ (z) 0)))

(define (static-distance le0)
  (local (;; String Symbol -> Symbol
          (define (declare key s)
            (string->symbol
             (string-append key ":"
                            (symbol->string s))))

          ;; Symbol [List-of Symbol] -> Number
          (define (how-far s l)
            (if (equal? s (first l))
                0
                (add1 (how-far s (rest l)))))


          ;; Lam [List-of Symbol] -> Lam
          ;; accumulator declareds is a list of all λ
          ;; parameters on the path from le0 to le
          (define (static-distance/a le declareds)
            (cond
             [(is-var? le)
              (if (member? le declareds)
                  (how-far le declareds)
                  (declare "*undeclared" le))]
             [(is-λ? le)
              (local ((define para (λ-para le))
                      (define body (λ-body le))
                      (define newd (append para declareds)))
                (list 'λ para
                      (static-distance/a body newd)))]
             [(is-app? le)
              (local ((define fun (app-fun le))
                      (define arg (app-arg le)))
                (list (static-distance/a fun declareds)
                      (static-distance/a arg declareds)))])))
    (static-distance/a le0 '())))
