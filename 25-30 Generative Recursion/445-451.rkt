;;=======================
;; 445

(define epsilon 0.00005)

;; [Number-> Number] -> [Number -> Boolean]
;; create a check-function for find-root for f
(define (create-check f)
  (lambda (root)
    (<= (abs (f root) ) 0.5)))

;; Number -> Number
(define (poly x)
  (* ( - x 2) (- x 4)))

;;===============================
;; 446-449

;; [Number -> Number] Number Number -> Number
;; determines R such that f has a root in [R,(+ R ε)]
;; assume f is continuous
;; assume (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
;; generative divide interval in half, the root is in one of the two
;; halves, pick according to assumption
(check-satisfied (find-root poly 1 3) (create-check poly))
(check-satisfied (find-root poly 3 4) (create-check poly))
(define (find-root f left right)
  (local ((define (helper left right f@left f@right)
            (cond
             [(<= (- right left) epsilon) left]
             [else
              (local ((define mid (/ (+ left right) 2))
                      (define f@mid (f mid)))
                (cond
                 [(or (<= f@left 0 f@mid) (<= f@mid 0 f@left))
                  (helper left mid f@left f@mid)]
                 [(or (<= f@mid 0 f@right) (<= f@right 0 f@mid))
                  (helper mid right f@mid f@right)]))])))
    (helper left right (f left) (f right))))


;;============================
;; 450

(define (find-root.v2  f left right)
  (local ((define (helper left right f@left f@right)
            (cond
             [(<= (- right left) epsilon) left]
             [else (local ((define mid (/ (+ left right) 2))
                           (define f@mid (f mid)))
                     (cond
                      [(<= f@left 0 f@mid)
                       (helper left mid f@left f@mid)]
                      [(<= f@mid 0 f@right)
                       (helper mid right f@mid f@right)]))])))
    (helper left right (f left) (f right))))


;;===================================
;; 451

(define-struct table [length array]) ;; (make-table Number [Number -> Number])
(define table1 (make-table 10 poly))


;; Table -> Number
(check-expect (find-linear table1) 2)
(define (find-linear table)
  (local ((define len (table-length table))
          (define table-function (table-array table))
          ;; N -> Number
          (define (table-ref i)
            (cond
             [(= i len) (error "no root")]
             [else (if (<= (abs (table-function i))
                           epsilon)
                       i
                       (table-ref (add1 i)))])))
    (table-ref 0)))

;; Table -> Number
(check-expect (find-linear table1) 2)
(define (find-binary table)
  (local ((define len (table-length table))
          (define func (table-array table))
          (define (helper left right f@left f@right)
            (cond
             [(= left right) left]
             [else (local ((define mid (floor (/ (+ left right) 2)))
                           (define f@mid (func mid)))
                     (cond
                      [(<= f@left 0 f@mid)
                       (helper left mid f@left f@mid)]
                      [(<= f@mid 0 f@right)
                       (helper mid right f@mid f@right)]))])))
    (helper 0 (sub1 len) (func 0) (func (sub1 len)))))
