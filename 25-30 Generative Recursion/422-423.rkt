;;===========================
;; 422

;; [List-of Any] N -> [List-of [List-of Any]]
;; produce a list of chunks, each of which contains n items
(check-expect (list->chunks '( 1 2 3 4 5) 2) '((1 2) (3 4) (5)))
(define (list->chunks l n)
  (cond
   [(< (length l) n) (list l)]
   [(zero? n) l]
   [else (cons (take l n) (list->chunks (drop l n) n))]))


;; [List-of Any] N -> [List-of Any]
;; extract the first n items from list
(define (take l n)
  (cond
   [(empty? l) '()]
   [(zero? n) '()]
   [else (cons (first l) (take (rest l) (sub1 n)))]))


;; [List-of Any] N -> [List-of Any]
;; remove the first n items from list l
(define (drop l n)
  (cond
   [(empty? l) '()]
   [(zero? n) l]
   [else (drop (rest l) (sub1 n))]))


;;=============================
;; 423

;; String N -> [List-of String]
;; divide a string into chunks, each of which is n 1String long
(check-expect (partition "abcdefg" 3) '("abc" "def" "g"))
(define (partition s n)
  (cond
   [(< (string-length s) n) (list s)]
   [(zero? n) (list s)]
   [else (cons (substring s 0 n)
               (partition (substring  s n (string-length s)) n))]))
