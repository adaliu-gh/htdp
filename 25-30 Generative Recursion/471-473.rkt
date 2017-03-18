;;=========================
;; 471

;; a Node is a Symbol

;; a Graph is:
;; - [List-of [List-of Node]]

(define sample-graph
  '((A B E)
    (B E F)
    (C D)
    (D)
    (E C F)
    (F D G)
    (G)))


;; Node Graph -> [List-of Node]
;; produce the immediate neighbors of node in graph
(check-expect (neighbors 'C sample-graph) '(D))
(check-expect (neighbors 'G sample-graph) '())

(define (neighbors node graph)
  (cond
   [(empty? graph) '()]
   [else (if (equal? (first (first graph)) node)
             (rest (first graph))
             (neighbors node (rest graph)))]))

;;================================
;; 472

;; Node Node Graph -> [Maybe Path]
;; finds a path from origination to destination in G
;; if there is no path, the function produces #false
(check-expect (find-path 'C 'D sample-graph) '(C D))
(check-member-of (find-path 'E 'D sample-graph)
                 '(E F D) '(E C D))
(check-expect (find-path 'C 'G sample-graph) #false)

(define (find-path origination destination G)
  (cond
   [(symbol=? origination destination) (list destination)]
   [else (local ((define next (neighbors origination G))
                 (define candidate
                   (find-path/list next destination G)))
           (cond
            [(boolean? candidate) #false]
            [else (cons origination candidate)]))]))


;; [List-of Node] Node Graph -> [Maybe Path]
;; finds a path from some node on lo-Os to D
;; if there is no path, the function produces #false
(define (find-path/list lo-Os D G)
  (cond
   [(empty? lo-Os) #false]
   [else (local ((define candidate
                   (find-path (first lo-Os) D G)))
           (cond
            [(boolean? candidate)
             (find-path/list (rest lo-Os) D G)]
            [else candidate]))]))


;; Graph -> Boolean
;; exmaine whether there is a path between any pair of nodes in the given graph

(define (test-on-all-nodes G)
  (local (;; [List-of [List-of Nodes]] -> Boolean
          (define (test-pairs pairs)
            (cond
             [(empty? pairs) #true]
             [else (local ((define first-pair (first pairs)))
                     (if (cons? (find-path (first first-pair) (second first-pair) G))
                         (test-pairs (rest pairs))
                         #false))])))
    (test-pairs (pairs (nodes G)))))


;; Graph -> [List-of Node]
;; extract all nodes from the given graph
(check-expect (nodes sample-graph) '( A B C D E F G))
(define (nodes G)
  (cond
   [(empty? G) '()]
   [else (cons (first (first G))
               (nodes (rest G)))]))


;; [X] [List-of X] -> [List-of (list X X)]
;; extracts all pairs of X from the given list
(define (pairs l)
  (local (;; X [List-of X] -> [List-of (list X X)]
          (define (first-pairs x l)
            (cond
             [(empty? l) '()]
             [else (cons (list x (first l))
                         (first-pairs x (rest l)))])))
    (cond
     [(empty? l) '()]
     [else (append (first-pairs (first l) (rest l))
                   (pairs (rest l)))])))


;;=======================================
;; 473

(define cyclic-graph
  '((A B E)
    (B E F)
    (C B D)
    (D)
    (E C F)
    (F G)
    (G)))


;; Node Node Graph -> [Maybe Path]
(define (find-path.v2 origination destination G)
  (find-path-helper.v2 origination destination origination G))

;; Node Node Node Graph -> [Maybe Path]
;; finds a path from origination to destination in G
;; if there is no path, the function produces #false
(define (find-path-helper.v2 origination destination S G)
  (cond
   [(symbol=? origination destination) (list destination)]
   [else (local ((define next (neighbors origination G))
                 (define candidate
                   (find-path-helper.v2/list  next  destination S G)))
           (cond
            [(boolean? candidate) #false]
            [else (cons origination candidate)]))]))

;; [List-of Node] Node Node Graph -> [Maybe Path]
;; finds a path from some node on lo-Os to D
;; if there is no path, the function produces #false
(define (find-path-helper.v2/list lo-Os D S G)
  (cond
   [(empty? lo-Os) #false]
   [(member? S lo-Os) #false]
   [else (local ((define candidate
                   (find-path-helper.v2 (first lo-Os) D S G)))
           (cond
            [(boolean? candidate)
             (find-path-helper.v2/list (rest lo-Os) D S G)]
            [else candidate]))]))
