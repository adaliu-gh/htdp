(require 2htdp/abstraction)

(define-struct no-parent [])
(define NP (make-no-parent))
(define-struct child [father mother name date eyes])
;;a FT (short for family tree) is one of:
;; - NP
;; - (make-child FT FT String N String)

;; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))

;; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))

;; Youngest Generation:
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

;; a FF (short for family forest) is one of:
;; - '()
;; - (cons FT FF)
(define ff1 (list Carl Bettina))
(define ff2 (list Fred Eva))
(define ff3 (list Fred Eva Carl))

;; FT -> Boolean
;; does a-ftree contain a child
;; structure with "blue" in the eyes field
(check-expect (blue-eyed-child? Carl) #false)
(check-expect (blue-eyed-child? Gustav) #true)

(define (blue-eyed-child? a-ftree)
  (cond
   [(no-parent? a-ftree) #false]
   [else (or (string=? (child-eyes a-ftree) "blue")
             (blue-eyed-child? (child-father a-ftree))
             (blue-eyed-child? (child-mother a-ftree)))]))
;;===========================
;;310

;; FT -> Number
;; counts the child knots in the family tree ftree
(check-expect (count-persons Gustav) 5)
(check-expect (count-persons Carl) 1)
(define (count-persons ftree)
  (match ftree
    [(no-parent) 0]
    [(child father mother name date eyes)
     (+ 1 (count-persons father) (count-persons mother))]))

;;=================================
;;311

;; FT -> Number
;; sums the ages of all child structures in the ftree
(define (sum-ages ftree present)
  (match ftree
    [(no-parent) 0]
    [(child father mother name date eyes)
     (+ (- present date) (sum-ages father present) (sum-ages mother present))]))

;; FT -> Number
;; computes the average age of all child structures in the ftree
(define (average-age ftree)
  (/ (sum-ages ftree) (count-persons ftree)))

;;==================================
;;312

;; FT -> [List-of String]
;; gets a list of eye colors in the tree
(check-expect (eye-colors Carl) '("green"))
(check-expect (eye-colors Dave) '("black" "green" "green"))
(define (eye-colors ftree)
  (match ftree
    [(no-parent) '()]
    [(child father mother name date eyes)
     (cons eyes (append (eye-colors father) (eye-colors mother)))]))

;;=================================
;;313

;; FT -> Boolean
;; determines if any of ancestors of the ftree has blue eyes
(check-expect (blue-eyed-ancestor? Eva) #false)
(check-expect (blue-eyed-ancestor? Gustav) #true)
(define (blue-eyed-ancestor? ftree)
  (match ftree
    [(no-parent) #false]
    [(child father mother name date eyes)
     (or (blue-eyed-child? father) (blue-eyed-child? mother))]))

;;================
;;314

;; FF -> Boolean
;; does the forest contain any child with "blue" eyes?
(check-expect (blue-eyed-child-in-forest? ff1) #false)
(check-expect (blue-eyed-child-in-forest? ff2) #true)
(check-expect (blue-eyed-child-in-forest? ff3) #true)
(define (blue-eyed-child-in-forest? forest)
  (ormap blue-eyed-child? forest))

;;===========================
;;315

;; FF -> Number
(define (average-age-in-forest forest present)
  (/ (foldl (lambda (x y) (+ (sum-ages x present) y)) 0 forest)
     (foldl (lambda (x y) (+ (count-persons x) y)) 0 forest)))
