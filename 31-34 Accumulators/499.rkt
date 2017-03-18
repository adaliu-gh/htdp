;;===========================
;; 499

(define-struct node [left right])
;; a Tree is one of:
;; - '()
;; - (make-node Tree Tree)
(define example (make-node (make-node '()
                                      (make-node '() '())) '()))


;; Tree -> Number
(check-expect (height.v1 example) 3)
(define (height.v1 abt)
  (cond
   [(empty? abt) 0]
   [else (+ (max (height.v1 (node-left abt))
                 (height.v1 (node-right abt))) 1)]))

;; Tree -> Number
(check-expect (height.v1 example) 3)
(define (height.v2 abt)
  (local (;; Tree N -> Number
          ;; measure the height of abt
          ;; accumulator a is the number of steps
          ;; it takes to reach abt from abt0
          (define (height/a abt a)
            (cond
             [(empty? abt) a]
             [else (max (height/a (node-left abt) (add1 a))
                        (height/a (node-right abt) (add1 a)))])))
    (height/a abt 0)))


;; Tree -> Number
(check-expect (height.v1 example) 3)
(define (height.v3 abt)
  (local (;; Tree N N -> Number
          ;; measure the height of steps
          ;; accumulator s is the number of steps
          ;; it takes to reach abt from abt0
          ;; accumulator m is the maximal height of
          ;; the part of abt0 that is to the left of abt
          (define (h/a abt s m)
            (cond
             [(empty? abt) (max s m)]
             [else (h/a (node-right abt (add1 s)
                                    (h/a (node-left abt) (add1 s) m)))])))
    (h/a abt 0 0)))
