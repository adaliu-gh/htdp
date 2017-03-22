
;;======================
;; 514

(define-struct la [para body])
(define-struct ap [arg body])

;; a Lam is one of:
;; - a Symbol
;; - (make-la [List-of Symbol] Lam)
;; - (make-ap Lam Lam)

(define ex1 (make-la '(x) 'x))
(define ex2 (make-la '(x) 'y))
(define ex3 (make-la '(y) (make-la '(x) 'y)))
(define ex4 (make-ap (make-la '(x) '(x x)) (make-la '(x) '(x x))))
(define ex5 'x)


;;========================
;; 517

;; Lam -> Lam
(check-expect (undeclareds ex1) (make-la '(x) '*declared:x))
(check-expect (undeclareds ex2) (make-la '(x) '*undeclared:y))
(define (undeclareds lam)
  (local (;; String Symbol -> Symbol
          (define (declare key s)
            (string->symbol
             (string-append key ":"
                            (symbol->string s))))

          ;; Lam [List-of Symbol] -> Lam
          (define (undeclareds/a lam declareds)
            (cond
             [(symbol? lam)
              (if (member? lam declareds)
                  (declare "*declared" lam)
                  (declare "*undeclared" lam))]
             [(la? lam)
              (make-la (la-para lam)
                       (undeclareds/a (la-body lam)
                                      (append (la-para lam) declareds)))]
             [(ap? lam)
              (make-ap (undeclareds/a (ap-arg lam) declareds)
                       (undeclareds/a (ap-body lam) declareds))])))
    (undeclareds/a lam '())))
