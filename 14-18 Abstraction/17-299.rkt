;;==================
;;299

;; the sets of all odd numbers
;; N -> Boolean
(define (odd-numbers n)
  (= 1 (/ n 2)))

;; the set of all even numbers
;; N -> Boolean
(define (even-numbers n)
  (zero? (/ n 2)))

;; the set of all numbers divisible by 10
;; N -> Boolean
(define (divisible-by-10 n)
  (zero? (/ n 10)))

;; Set Element -> Set
(define (add-element s e)
  (lambda (x)
    (or (s x)
        (equal? x e))))

;; Set Set -> Set
(define (union s1 s2)
  (lambda (x)
    (or (s1 x)
        (s2 x))))

;; Set Set -> Set
(define (intersect s1 s2)
  (lambda (x)
    (and (s1 x)
         (s2 x))))
