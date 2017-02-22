(require 2htdp/abstraction)

;;======================
;;308
(define-struct phone [area switch four])

;; [List-of Phone] -> [List-of Phone]
(check-expect (replace (list (make-phone 713 234 1234) (make-phone 713 456 6789))) (list (make-phone 281 234 1234) (make-phone 281 456 6789)))
(check-expect (replace (list (make-phone 613 234 1234) (make-phone 713 456 6789))) (list (make-phone 613 234 1234) (make-phone 281 456 6789)))
(define (replace lop)
  (match lop
    ['() '()]
    [(cons (phone 713 switch four) tail)
     (cons (make-phone 281 switch four) (replace tail))]
    [(cons x tail) (cons x (replace tail))]))

;;==========================
;;309
;; [List-of [List-of String]] -> [List-of Number]
;; determines the number of words on each line
(check-expect (words-on-line '(("a" "b") ("c"))) '(2 1))
(define (words-on-line lls)
  (match lls
    ['() '()]
    [(cons head tail)
     (cons (length head) (words-on-line tail))]))
