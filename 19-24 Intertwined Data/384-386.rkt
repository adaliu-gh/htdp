(require 2htdp/abstraction)
(require 2htdp/batch-io)
(require 2htdp/universe)
(require 2htdp/image)

(define PREFIX "https://www.google.com/finance?q=")
(define SUFFIX "&btnG=Search")
(define SIZE 22) ;; font size

(define-struct data [price delta])
;; a StockWorld is a structure; (make-data String String)

;; String -> StockWorld
;; retrives the stock price of co and its change every 15 minutes
(define (stock-alert co)
  (local (;; the url of search page
          (define url (string-append PREFIX co SUFFIX))

          ;; [StockWorld -> StockWorld]
          ;; retrives new data
          (define (retrieve-stock-data __w)
            (local ((define x (read-xexpr/web url)))
              (make-data (get x "price")
                         (get x "priceChange"))))

          ;; StockWorld -> Image
          (define (render-stock-data w)
            (local (; [StockWorld -> String] -> Image
                    (define (word sel col)
                      (text (sel w) SIZE col)))
              (overlay (beside (word data-price 'black)
                               (text " " SIZE 'white)
                               (word data-delta 'red))
                       (rectangle 300 35 'solid 'white)))))
    (big-bang (retrieve-stock-data 'no-use)
              [on-tick retrieve-stock-data 15]
              [to-draw render-stock-data])))

;;==========================
;;386

;; Xexpr.v3 String -> String
;; retrieves the value of the "content" attribute
;; from a 'meta element that has attribute "itemprop"
;; with value s
(check-expect
 (get '(meta ((content "+1") (itemprop "F"))) "F")
 "+1")
(check-error
 (get '(meta ((content "+1") (itemprop "F"))) "M")
 "not found")

(define (get x s)
  (local ((define result (get-xexpr x s)))
    (if (string? result)
        result
        (error "not found"))))

;; AL Symbol -> String
(define (find-attr l s)
  (cond
   [(empty? l) #false]
   [else (if (equal? (first (first l)) s)
             (second (first l))
             (find-attr (rest l) s))]))

;; [List-of Attribute] or Xexpr.v2 -> Boolean
;; is the given value a list of attributes
(define (list-of-attributes? x)
  (cond
   [(empty? x) #true]
   [else
    (local ((define possible-attribute (first x)))
      (cons? possible-attribute))]))

;; Xexpr.v3 String -> [Maybe String]
(define (get-xexpr x s)
  (local (;; Symbol [AL or Xexpr.v3] -> [Maybe String]
          (define (get-content-xexpr name al-or-xexpr)
            (if (and (equal? name 'meta)
                     (equal? s (find-attr al-or-xexpr 'itemprop)))
                (find-attr al-or-xexpr 'content)
                #false))

          ;; [AL or Xexpr.v3] XL-> [Maybe String]
          (define (get-from-rest al-or-xexpr xl)
            (if (list-of-attributes? al-or-xexpr)
                (get-xexpr-list xl s)
                (get-xexpr-list (cons al-or-xexpr xl s)))))
    (match x
      [(? symbol?) #false]
      [(? string?) #false]
      [(? number?) #false]
      [(cons symbol (cons al-or-xexpr xl))
       (local ((define potential-data (get-content-xexpr symbol al-or-xexpr)))
         (if (string? potential-data)
             potential-data
             (get-from-rest al-or-xexpr xl)))])))

;; XL String -> [Maybe String]
(define (get-xexpr-list xl s)
  (cond
   [(empty? xl) #false]
   [else (local ((define first-result (get-xexpr (first xl) s)))
           (if (string? first-result)
               first-result
               (get-xexpr-list (rest xl) s)))]))
