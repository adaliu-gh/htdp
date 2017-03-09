;;=====================
;;397

(define WRONG "WRONG")

(define-struct employee [name no rate])
(define-struct punch [no time])
(define-struct wage [name money])

;; e.x.
(define one-loe (list (make-employee "Adam" "012" 40.34) (make-employee "Eve" "013" 50.23)))
(define one-lop (list (make-punch "012" 34) (make-punch "013" 43)))

;; [List-of Employee] [List-of Punch] -> [List-of Wage]
(check-expect (wages*.v3 one-loe one-lop) (list (make-wage "Adam" (* 40.34 34)) (make-wage "Eve" (* 50.23 43))))
(check-error (wages*.v3 one-loe (list (make-punch "Peter" 34))) WRONG)

(define (wages*.v3 loe lop)
  (local (;; [List-of Employee] Punch -> Wage
          (define (get-wage loe p)
            (cond
             [(empty? loe) (error WRONG)]
             [else (local ((define first-em (first loe)))
                     (if (equal? (punch-no p) (employee-no first-em))
                         (make-wage (employee-name first-em)
                                    (* (punch-time p)
                                       (employee-rate first-em)))
                         (get-wage (rest loe) p)))])))
    (cond
     [(empty? lop) '()]
     [else (cons (get-wage loe (first lop))
                 (wages*.v3 loe (rest lop)))])))

;;===============================
;; 398

;; [List-of Number] [List-of Number] -> Number
(check-expect (value '(1 2 3) '( 2 3 4)) (+ 2 6 12))

(define (value lol lov)
  (cond
   [(empty? lol) 0]
   [else (+ (* (first lol) (first lov))
            (value (rest lol) (rest lov)))]))

;;================================
;;399

;; [NEList-of X] -> X
;; returns a random item from the list
(check-random (random-pick '(1 2 3)) (list-ref '(1 2 3) (random 3)))

(define (random-pick l)
  (list-ref l (random (length l))))

;; [List-of String] [List-of [List-of String]] -> [List-of [List-of String]]
;; produces the lsit of those lists in ll that do not agree with names at any place

(define (non-same names ll)
  (cond
   [(empty? ll) '()]
   [else (if (equal? names (first ll))
             (non-same names (rest ll))
             (cons (first ll) (non-same names (rest ll))))]))

;;==================================
;; 400

(define one-dna-pattern '(a c g t t g))
(define one-dna-string '(a c g t t g t a c t g))

;; [List-of Symbol] [List-of Symbol] -> Boolean
;; checks if the string starts with pattern
(check-expect (DNAprefix one-dna-pattern one-dna-string) #true)
(check-expect (DNAprefix one-dna-pattern (cons 'a one-dna-string)) #false)

(define (DNAprefix pattern string)
  (cond
   [(empty? pattern) #true]
   [(empty? string) #false]
   [else (if (equal? (first pattern) (first string))
             (DNAprefix (rest pattern) (rest string))
             #false)]))

;; [List-of Symbol] [List-of Symbol] -> [List-of Symbol]
;; gets the symbols in string beyond the pattern
(check-expect (DNAdelta one-dna-pattern one-dna-string) '(t a c t g))
(check-expect (DNAdelta one-dna-pattern (cons 'a one-dna-string)) #false)

(define (DNAdelta pattern string)
  (cond
   [(empty? pattern) string]
   [(empty? string) #false]
   [else (if (equal? (first pattern) (first string))
             (DNAdelta (rest pattern) (rest string))
             #false)]))

;;=================================
;; 401

;; S-expr S-expr -> Boolean
;; checks if the two S-exprs are equal
(check-expect (sexp=? 12 12) #true)
(check-expect (sexp=? '( 12 a (34 d) ) '(12 a (34 d))) #true)
(check-expect (sexp=? '( 12 a (34 d) ) '(12 a (34 e))) #false)

(define (sexp=? s1 s2)
  (local (; S-expr -> Boolean
          ;; checks if sexpr is an atom
          (define (atom? s) (or (number? s) (string? s) (symbol? s) (empty? s))))
  (cond
   [(and (atom? s1) (atom? s2)) (equal? s1 s2)]
   [(or (atom? s1) (atom? s2)) #false]
   [else (if (sexp=? (first s1) (first s2))
             (sexp=? (rest s1) (rest s2))
             #false)])))
