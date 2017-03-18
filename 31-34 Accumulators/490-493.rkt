;;==============================
;; 490

;; [List-of Number] -> [List-of Number]
;; convert a list of relative to absolute distances
;; the first number represents the distance to the origin

(check-expect (relative->absolute '(50 40 70 30 30))
              '(50 90 160 190 220))

(define (relative->absolute l)
  (cond
   [(empty? l) '()]
   [else (local ((define rest-of-l
                   (relative->absolute (rest l)))
                 (define adjusted
                   (add-to-each (first l) rest-of-l)))
           (cons (first l) adjusted))]))

;; Number [List-of Number] -> [List-of Number]
;; add n to each number on l

(check-expect (cons 50 (add-to-each 50 '(40 110 140 170)))
              '(50 90 160 190 220))

(define (add-to-each n l)
  (map (lambda (x) (+ x n)) l))


;;=======================================
;; 493

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
