;;====================
;;363

;; an Xexpr.v2 is a list:
;; - (cons Symbol XL)

;; an XL is one of:
;; - '()
;; - Xexpr.v2
;; - (cons Xexpr.v2 XL)
;; - (cons AL (cons Xexpr.v2 XL))

;; an Attribute is:
;; (cons Symbol (cons String '()))

;; an AL is one of:
;; - '()
;; - (cons Attribute AL)


;;========================
;;364

(define xexpr1 '(transition ((from "seen-e") (to "seen-f")) (l)))
(define xexpr2 '(ul (li (word) (word)) (li (word))))

;;==========================
;;365

;; 1. <server name="example.org"/>
;; 2. <carcas><board><grass/><board/><player name="sam"/><carcas/>
;; 3. <start/>

;;===========================
;;366

;; Xexpr.v2 -> Symbol
;; extract the name of the Xexpr
(check-expect (xexpr-name xexpr1) 'transition)
(check-expect (xexpr-name xexpr2) 'ul)
(define (xexpr-name xexpr)
  (first xexpr))

;; [List-of Attribute] or Xexpr.v2 -> Boolean
;; is the given value a list of attributes
(define (list-of-attributes? x)
  (cond
   [(empty? x) #true]
   [else
    (local ((define possible-attribute (first x)))
      (cons? possible-attribute))]))

;; Xexpr.v2 -> [List-of Xexpr.v2]
(check-expect (xexpr-content xexpr1) '((l)))
(check-expect (xexpr-content xexpr2) '((li (word) (word)) (li (word))))
(define (xexpr-content xexpr)
  (local ((define optional-loa+content (rest xexpr)))
    (cond
     [(empty? optional-loa+content) '()]
     [else (local ((define loa-or-x
                     (first optional-loa+content)))
             (if (list-of-attributes? loa-or-x)
                 (rest optional-loa+content)
                 optional-loa+content))])))

;;============================
;;369

(define attr1 '((name "ada") (age "21") (gender "f")))

;; AL Symbol -> String
(check-expect (find-attr attr1 'name) "ada")
(check-expect (find-attr attr1 'addr) #false)
(define (find-attr l s)
  (cond
   [(empty? l) #false]
   [else (if (equal? (first (first l)) s)
             (second (first l))
             (find-attr (rest l) s))]))
