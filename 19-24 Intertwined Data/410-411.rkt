
(define-struct db [schema content])

(define school-schema
  `( ( "Name" ,string?)
     ( "Age" ,integer?)
     ( "Present" ,boolean?)))
(define school-content
  '(("Alice" 35 #true)
    ("Bob" 25 #false)
    ("Carol" 30 #true)
    ("Dave" 32 #false)))
(define school-db (make-db school-schema school-content))

(define presence-schema `( ( "Present" ,boolean?)
        ( "Description" ,string?)))
(define presence-content
  '((#true "presence")
    (#false "absence")))
(define presence-content2
  '((#true "presence")
    (#true "absence")
    (#false "presence")
    (#false "absence")))
(define presence-db (make-db presence-schema presence-content))
(define presence-db-2 (make-db presence-schema presence-content2))


;;================================
;; 410

;; Any [List-of Any] -> [List-of Any]
(define (add-at-end x l)
  (cond
   [(empty? l) (cons x '())]
   [else (cons (first l) (add-at-end x (rest l)))]))


;; Set Set -> Set
;; union two sets
(define (set-union s1 s2)
  (foldr (lambda (x y) (if (member? x y) y (cons x y))) s2 s1))


;; DB DB -> DB
(check-expect (db-union school-db (make-db school-schema '(("Ada" 21 #false)))) (make-db school-schema (add-at-end '("Ada" 21 #false) school-content)))
(define (db-union d1 d2)
  (local ((define schema (db-schema d1))
          (define content1 (db-content d1))
          (define content2 (db-content d2)))
    (make-db schema (set-union content1 content2))))


;;========================================
;; 411

;; List -> Item
;; extract the last item of a list
(define (last l)
  (cond
   [(empty? (rest l)) (first l)]
   [else (last (rest l))]))

;; DB DB -> DB
;; one row corresponds to one row
(define (join.v1 db-1 db-2)
  (local ((define schema1 (db-schema db-1))
          (define schema2 (db-schema db-2))
          (define content1 (db-content db-1))
          (define content2 (db-content db-2))

          ;; List List -> List
          (define (join-lists s1 s2)
            (append (reverse (rest (reverse s1))) (rest s2)))

          ;; Row -> Row
          (define (join-row row)
            (local (;; Any -> Row
                    (define (find-row key)
                      (foldl (lambda (row result)
                               (if (equal? key (first row))
                                   row result)) '() content2)))
            (join-lists row (find-row (last row))))))

    (make-db (join-lists schema1 schema2)
             (map join-row content1))))

;; DB -> DB
;; one row may correspond to many rows
(define (join.v2 db-1 db-2)
  (local ((define schema1 (db-schema db-1))
          (define schema2 (db-schema db-2))
          (define content1 (db-content db-1))
          (define content2 (db-content db-2))

          ;; List List -> List
          (define (join-lists s1 s2)
            (append (reverse (rest (reverse s1))) (rest s2)))

          ;; Any -> [List-of Row]
          (define (find-row key)
            (foldl (lambda (row result)
                     (if (equal? key (first row))
                         (cons row result) result)) '() content2))

          ;; Row -> [List-of Row]
          (define (join-row row)
            (map (lambda (x) (join-lists row x)) (find-row (last row)))))

    (make-db (join-lists schema1 schema2)
             (foldl (lambda (row result)
                      (append (join-row row) result)) '() content1))))
