(require 2htdp/image)

(define xexpr1 '(transition ((from "seen-e") (to "seen-f")) (l)))
(define xexpr2 '(ul (li (word) (word)) (li (word))))

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
(define (xexpr-content xexpr)
  (local ((define optional-loa+content (rest xexpr)))
    (cond
     [(empty? optional-loa+content) '()]
     [else (local ((define loa-or-x
                     (first optional-loa+content)))
             (if (list-of-attributes? loa-or-x)
                 (rest optional-loa+content)
                 optional-loa+content))])))

;; Xexpr.v2 -> [List-of Attribute]
(check-expect (xexpr-attr xexpr1) '((from "seen-e") (to "seen-f")))
(check-expect (xexpr-attr xexpr2) '())
(define (xexpr-attr x)
  (local ((define optional-loa+content (rest x)))
    (cond
     [(empty? optional-loa+content) '()]
     [else (local ((define loa-or-x
                     (first optional-loa+content)))
             (if (list-of-attributes? loa-or-x)
                 loa-or-x
                 '()))])))

;;=======================
;;370

;; an XWord is '(word ((text String)))
(define xw1 '(word ((text "hello"))))
(define xw2 '(word ((text "world"))))
(define xw3 '(word ((text "!"))))

;; Any -> Boolean
;; determine if x is a XWord
(check-expect (word? xw1) #true)
(check-expect (word? 1) #false)
(define (word? x)
  (if (cons? x)
      (equal? 'word (first x))
      #false))


;; XWord -> String
;; extract the text from the given xw
(check-expect (word-text xw1) "hello")
(define (word-text xw)
  (second (first (second xw))))

;;================================
;;371

;; an Xexpr is:
;; (cons Symbol XL)

;; an XL is one of:
;; - [List-of Xexpr]
;; - (cons [List-of Attribute] [List-of Xexpr])

;; an Attribute is one of:
;; - (cons Symbol Value)
;; - (cons 'text String)

;;=============================
;;372

(define li1 '(li (word ((text "one")))))
(define li2 '(li (word ((text "two")))))

(define BULLET (circle 1 "solid" "black"))

;; XItem.v1 -> Image
(define (render-item1 i)
  (local ((define content (xexpr-content i))
          (define element (first content))
          (define a-word (word-text element))
          (define item (text a-word 12 'black)))
    (beside/align 'center BULLET item)))

;;==============================
;; 376

(define xe1 '(ul
              (li ((name "1"))
                  (word ((text "hello"))))
              (li (ul
                   (li (word ((text "world"))))
                   (li (word ((text "hello"))))))))
(define xe2 '(ul
              (li ((name "1"))
                  (word ((text "bye"))))
              (li (ul
                   (li (word ((text "world"))))
                   (li (word ((text "bye"))))))))

;; XEnum.v2 -> Number
;; counts all 'hello's in the given xexpr
(check-expect (count-hello xe1) 2)
(define (count-hello x)
  (local ((define content (xexpr-content x))

          ;; XItem.v2 Number -> Number
          (define (add-one-item x n)
            (+ n (count-hello-item x))))
    (foldr add-one-item 0 content)))

;; XItem.v2 -> Number
(check-expect (count-hello-item '(li (word ((text "hello"))))) 1)
(define (count-hello-item x)
  (local ((define content (xexpr-content x))
          (define element (first content)))
    (cond
     [(word? element)
      (if (equal? "hello" (word-text element))
          1 0)]
     [else (count-hello element)])))

;;=============================
;;377

;; XEnum.v2 -> XEnum.v2
;; replaces all "hello" with "bye"
(check-expect (replace xe1) xe2)
(check-expect (replace xe2) xe2)
(define (replace x)
  (local ((define elements (xexpr-content x))
          (define name (xexpr-name x))
          (define attr (xexpr-attr x))
          (define replaced-elements (map replace-hello-item elements)))
    (cons name (cond
                [(empty? attr) replaced-elements]
                [else (cons attr replaced-elements)]))))

;; XItem.v2 -> XItem.v2
(define (replace-hello-item x)
  (local ((define element (first (xexpr-content x)))
          (define name (xexpr-name x))
          (define attr (xexpr-attr x))

          ;; Xexpr -> Xexpr
          (define (replace-element e)
            (cond
             [(word? e) (if (equal? "hello" (word-text e))
                            '(word ((text "bye"))) e)]
             [else (replace e)]))

          (define replaced-content (list (replace-element element))))
    (cons name (cond
                      [(empty? attr) replaced-content]
                      [else (cons attr replaced-content)]))))
