;;==========================
;; 403

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
(define presence-db (make-db presence-schema presence-content))

;;========================
;; 404

;; [X Y] [X Y -> Boolean] [List-of X] [List-of Y] -> Boolean
(define (andmap2 f l1 l2)
  (cond
   [(empty? l1) #true]
   [else (and (f (first l1) (first l2))
              (andmap2 f (rest l1) (rest l2)))]))

;;=========================
;; 405

(define labels '("Name" "Present"))

(define projected-content
  `(("Alice" #true)
    ("Bob"   #false)
    ("Carol" #true)
    ("Dave"  #false)))
(define projected-schema
  `(("Name" ,string?) ("Present" ,boolean?)))
(define projected-db
  (make-db projected-schema projected-content))


;; Row [List-of Labels] -> Row
;; retain those cells whose name is in labels
(check-expect (row-filter '("Alice" 34 #true) '("Name" "Age" "Present")) '("Alice" #true))

(define (row-filter row names)
  (cond
   [(empty? row) '()]
   [else (local ((define filtered (row-filter (rest row) (rest names))))
           (if (member? (first names) labels)
               (cons (first row) filtered) filtered))]))

;;===============================
;; 406

;; Any [List-of Any] -> [List-of Any]
(define (add-at-end x l)
  (cond
   [(empty? l) (cons x '())]
   [else (cons (first l) (add-at-end x (rest l)))]))

;; DB [List-of String] -> DB
(check-expect
 (db-content (project.v1 school-db '("Name" "Present")))
 projected-content)
(define (project.v1 db labels)
  (local ((define schema  (db-schema db))
          (define content (db-content db))
          (define schema-labels (map first schema))

          ;; Spec -> Boolean
          ;; does this column belong to the new schema
          (define (keep? c)
            (member? (first c) labels))

          ;; Row -> Row
          ;; retains those columns whose name is in labels
          (define (row-project row)
            (foldr (lambda (x y z)
                     (if (member? x labels)
                         (cons y z) z)) '() schema-labels row))

          ;; Row [List-of Label] -> Row
          ;; retain those cells whose name is in labels
          (define (row-filter row names)
            (cond
             [(empty? row) '()]
             [else
              (if (member? (first names) labels)
                  (cons (first row)
                        (row-filter (rest row) (rest names)))
                  (row-filter (rest row) (rest names)))])))
    (make-db (filter keep? schema)
             (map row-project content))))

(define (project db labels)
  (local ((define schema (db-schema db))
          (define content (db-content db))

          (define (keep? c) (member? (first c) labels))

          (define (row-project row)
            (foldr (lambda (cell m c) (if m (cons cell c) c))
                   '() row mask))

          (define mask (map keep? schema)))
    (make-db (filter keep? schema)
             (map row-project content))))

;;===============================
;; 408

;; DB [List-of String] [Row -> Boolean] -> DB
(define (select db labels predicate)
  (project (make-db (db-schema db)
                    (map predicate (db-content db))) labels))

;;============================
;; 409

;; DB [List-of Label] -> DB
(define (reorder.v1 db labels)
  (local ((define schema (db-schema db))
          (define content (db-content db))
          (define schema-labels (map first schema))

          ;; [List-of String] -> [List-of Spec]
          (define (reorder-schema label)
            (foldr (lambda (spec x)
                     (if (equal? label (first spec))
                         spec x)) '- schema))

          ;; Row -> Row
          (define (reorder-row row)
            (local ((define (find-cell label)
                      (foldr (lambda (cell l x)
                               (if (equal? l label) cell x))
                             '- row schema-labels)))
            (map find-cell labels))))

    (make-db (map reorder-schema labels)
             (map reorder-row content))))

;; DB [List-of Number] -> DB
(define (reorder.v2 db order)
  (local ((define schema (db-schema db))
          (define content (db-content db))

          ;; Row -> Row
          (define (reorder-row row)
            (map (lambda (n) (list-ref row n)) order)))

    (make-db (reorder-row schema)
             (map reorder-row content))))

;; DB [List-of Labels] -> DB
(define (reorder.v3 db labels)
  (local ((define schema (db-schema db))
          (define content (db-content db))

          ;; String -> Number
          (define (find-order l)
            (local ((define (find-order-helper l s n)
                      (cond
                       [(empty? s) (error "WRONG")]
                       [else (if (equal? l (first (first s)))
                                 n
                                 (find-order-helper l (rest s) (add1 n)))])))
              (find-order-helper l schema 0)))

          (define order (map find-order labels))


          ;; Row -> Row
          (define (reorder-row row)
            (map (lambda (n) (list-ref row n)) order)))

    (make-db (reorder-row schema)
             (map reorder-row content))))


