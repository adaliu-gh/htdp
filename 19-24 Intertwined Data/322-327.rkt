(require 2htdp/abstraction)
(define-struct no-info [])
(define NONE (make-no-info))

(define-struct node [ssn name left right])
;; a BinaryTree (short for BT) is one of:
;; - None
;; - (make-node Number Symbol BT BT)

(define bt1 (make-node 15 'd NONE (make-node 24 'i NONE NONE)))
(define bt2 (make-node 15 'd (make-node 87 'h NONE NONE) NONE))

;;===========================
;;322

;; BT Number -> Boolean
;;(check-expect (contains-bt? bt1 87) #false)
;;(check-expect (contains-bt? bt2 87) #true)
(define (contains-bt? bt n)
  (match bt
    [(no-info) #false]
    [(node nu sy bt1 bt2)
     (or (= nu n)
         (contains-bt? bt1 n)
         (contains-bt? bt2 n))]))

;;==========================
;;323

;; BT Number -> [Maybe [Symbol]]
(check-expect (search-bt bt1 87) #false)
(check-expect (search-bt bt2 87) 'h)
(define (search-bt b n)
  (match b
    [(no-info) #false]
    [(node nu s b1 b2)
     (if (= nu n) s
         (if (contains-bt? b1 n)
             (search-bt b1 n)
             (search-bt b2 n)))]))

;;=======================
;;324

;;BT -> [List-of Number]
(check-expect (inorder bt1) '(15 24))
(check-expect (inorder bt2) '(87 15))
(define (inorder bt)
  (match bt
    [(no-info) '()]
    [(node nu sy bt1 bt2)
     (append (inorder bt1) (list nu) (inorder bt2))]))

;;========================
;;325

;; BST N -> [Symbol/no-info]
(define (search-bst bt n)
  (match bt
    [(no-info) bt]
    [(node nu sy bt1 bt2)
     (if (= nu n) sy
         (if (> n nu)
             (search-bst bt1 n)
             (search-bst bt2 n)))]))

;;=====================
;;326

;; BST Number Symbol -> BST
(define (create-bst bst n s)
  (match bst
    [(no-info) (make-node n s NONE NONE)]
    [(node num sym bs bl)
     (cond
      [(< n num) (make-node num sym (create-bst bs n s) bl)]
      [(> n num) (make-node num sym bs (create-bst bl n s))])]))

;;=============================
;;327

(define one-list '((99 o)
                    (77 l)
                    (24 i)
                    (10 h)
                    (95 g)
                    (15 d)
                    (89 c)
                    (29 b)
                    (63 a)))
;; [List-of [List Number Symbol]] -> BST
(define (create-bst-from-list l)
  (match l
    ['() NONE]
    [(cons head tail)
     (create-bst (create-bst-from-list tail) (first head) (second head))]))

;; version 2
(define (create-bst-from-list.v2 l)
    (foldr (lambda (i bst)
             (create-bst bst (first i) (second i))) NONE l))

;; version 3
(define (create-bst-from-list.v3 l)
  (foldl (lambda (i bst)
           (create-bst bst (first i) (second i))) NONE l))
