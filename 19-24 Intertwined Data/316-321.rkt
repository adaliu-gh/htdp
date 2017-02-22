;; An S-expr is one of:
;; – Atom
;; – SL

;; An Atom is one of:
;; – String
;; – Number
;; – Symbol

;; An SL is one of:
;; – '()
;; – (cons S-expr SL)

;;========================
;;316

;; Any -> Boolean
;; determines if the given thing is an atom
(define (atom? x)
  (or (string? x)
      (number? x)
      (symbol? x)))

;;========================
;;317

;; S-expr Symbol -> N
;; counts all occurences of sy in sexp
(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(define (count sexp sy)
  (local (;; Atom -> N
          (define (count-atom a)
            (cond
             [(string? a) 0]
             [(number? a) 0]
             [(symbol? a) (if (symbol=? sy a) 1 0)]))

          ;; SL -> N
          (define (count-sl sl)
            (cond
             [(empty? sl) 0]
             [else
              (+ (count (first sl) sy) (count-sl (rest sl)))])))
    (cond
     [(atom? sexp) (count-atom sexp)]
     [else (count-sl sexp)])))


;;=============================
;;318

;; S-expr -> N
;; determines the depth of sexp
(check-expect (depth 10) 1)
(check-expect (depth '( "sd" 34)) 2)
(check-expect (depth '((21 12))) 3)
(check-expect (depth '( "sd" 34 (12 er))) 3)
(define (depth sexp)
  (local (;; Atom -> N
           (define (depth-atom a) 1)

           ;; SL -> N
           (define (depth-sl l)
             (cond
              [(empty? l) 1]
              [else  (max (depth (first l))
                          (depth-sl (rest l)))])))
    (cond
     [(atom? sexp) (depth-atom sexp)]
     [else (add1 (depth-sl sexp))])))

;;======================
;;319

;; S-expr Atom Atom -> S-expr
;; replaces all occurences of old with new in sexp
(check-expect (substitute "as" "as" "ad") "ad")
(check-expect (substitute '("as" 1 2) "as" "ad") '("ad" 1 2))
(check-expect (substitute '("as" 1 2 ("as")) "as" "ad") '("ad" 1 2 ("ad")))
(define (substitute sexp old new)
  (local (;; Atom -> Atom
           (define (sub-atom a)
             (if (equal? a old) new a))

           ;; SL -> SL
           (define (sub-sl l)
             (cond
              [(empty? l) l]
              [else (cons (substitute (first l) old new)
                          (sub-sl (rest l)))])))
    (cond
     [(atom? sexp) (sub-atom sexp)]
     [else (sub-sl sexp)])))

;;==========================
;;320

;; a S-expr is one of:
;; - String
;; - Number
;; - Symbol
;; - [List-of S-expr]

;; S-expr [String or Number or Symbol] -> Number
(check-expect (count.v2 'world 'hello) 0)
(check-expect (count.v2 '(world hello) 'hello) 1)
(define (count.v2 sexp sy)
  (local (;; [List-of S-expr] -> Number
          (define (count-sl l)
            (cond
             [(empty? l) 0]
             [else (+ (count.v2 (first l) sy)
                      (count-sl (rest l)))])))
    (cond
     [(list? sexp) (count-sl sexp)]
     [else (if (equal? sexp sy) 1 0)])))

;;  a S-expr is one of:
;; - '()
;; - String
;; - Number
;; - Symbol
;; - [NEList-of S-expr]
(check-expect (count.v2 'world 'hello) 0)
(check-expect (count.v2 '(world hello) 'hello) 1)
(define (count.v3 sexp sy)
  (cond
   [(empty? sexp) 0]
   [(list? sexp)
    (foldl (lambda (x y) (+ (count.v3 x sy) y)) 0 sexp)]
   [else (if (equal? sexp sy) 1 0)]))


;;==================
;;321

;; an Atom is:
;; - Any Item

;; a S-expr is one of:
;; - Atom
;; - SL

;; a SL is one of:
;; - '()
;; - (cons S-expr SL)

