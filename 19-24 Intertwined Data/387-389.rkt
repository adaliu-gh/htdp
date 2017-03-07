;;========================
;;387

;; [List-of Symbol] [List-of Number] -> [List-of (list Symbol Number)]
;; produces all possible pairs of symbols and numbers
(check-expect (cross '(a b c) '( 1 2))
              '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))
(define (cross los lon)
  (local (;; Symbol [List-of Number] -> [List-of (list Symbol Number)
          (define (cross-one s lon)
            (cond
             [(empty? lon) '()]
             [else (cons (list s (first lon))
                         (cross-one s (rest lon)))])))
    (cond
     [(empty? los) '()]
     [else (append (cross-one (first los) lon)
                   (cross (rest los) lon))])))

;;==============================
;;388

(define-struct employee [name security-number pay-rate])
(define-struct work-record [name hours])
(define one-list-work-record (list (make-work-record "Adam" 12) (make-work-record "Eve" 12)))
(define one-list-employee (list (make-employee "Adam" 012 40.34) (make-employee "Eve" 013 50.34)))

;; [List-of Employee] [List-of Work-Record] -> [List-of Number]
(check-expect (wages*.v2 one-list-employee one-list-work-record)  (list (* 12 40.34) (* 12 50.34)))
(define (wages*.v2 loe low)
  (cond
   [(empty? loe) '()]
   [else (cons (weekly-wage (first loe) low)
               (wages*.v2 (rest loe) low))]))

;; Employee [List-of Work-Record] -> Number
;; computes the weekly of an employee
(define (weekly-wage e low)
  (cond
   [(empty? low) 0]
   [else (if (equal? (employee-name e)
                     (work-record-name (first low)))
             (* (employee-pay-rate e)
                (work-record-hours (first low)))
             (weekly-wage e (rest low)))]))

;;==========================
;;389

(define-struct phone-record [name number]) ;; (make-phone-record String String)

;; [List-of String] [List-of String] -> [List-of Phone-record]
(define (zip list-of-name list-of-phone)
  (cond
   [(empty? list-of-name) '()]
   [else (cons (make-phone-record (first list-of-name)
                                  (first list-of-phone))
               (zip (rest list-of-name) (rest list-of-phone)))]))

;;=========================
;;390

(define-struct branch [left right])

;; a TOS is one of:
;; - Symbol
;; - (make-branch Tos Tos)
(define one-tos (make-branch (make-branch (make-branch 'a 'e) (make-branch 'b 'c)) (make-branch 'd 'e)))

;; a Direction is one of:
;; - 'left
;; - 'right

;; a list of Directions is also called a path
(define one-path '(left right right))

;; TOS Path -> Symbol
(check-expect (tree-pick one-tos one-path) 'c)
(check-error (tree-pick one-tos '(left)) "you have reached the end of path")
(check-error (tree-pick one-tos '(left left left left left)) "you have reached the end of tos")
(define (tree-pick tos path)
  (cond
   [(and (empty? path)
         (symbol? tos)) tos]
   [(and (empty? path)
         (branch? tos)) (error "you have reached the end of path")]
   [(symbol? tos) (error "you have reached the end of tos")]
   [(branch? tos)
    (tree-pick (if (equal? 'left (first path))
                   (branch-left tos)
                   (branch-right tos)) (rest path))]))
