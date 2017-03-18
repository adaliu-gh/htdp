;;============================
;; 453

;; a Token is one of:
;; - 1String
;; - [List-of 1String] (where all 1Strings are lower-case letters)

(define one-line '("h" "e" "l" "l" "o" ","  " " "w" "o" "r" "l" "d" "!"))
(define another-line '("/" "h" "e" "l" "l" "o" "," "\t" " " "w" "o" "r" "l" "d" "!" "\n"))

;; Line ([List-of 1String]) -> [List-of Token]
(check-expect (tokenize one-line) '(("h" "e" "l" "l" "o")
                                    ","
                                    ("w" "o" "r" "l" "d")
                                    "!"))
(check-expect (tokenize another-line) '("/"
                                        ("h" "e" "l" "l" "o")
                                    ","
                                    ("w" "o" "r" "l" "d")
                                    "!"))
(define (tokenize line)
  (cond
   [(empty? line) '()]
   [else (cons (first-token line)
               (tokenize (remove-first-token line)))]))

;; Line -> Token
(define (first-token line)
  (local (;; Line -> [List-of 1String]
          (define (first-word line)
            (cond
             [(or (empty? line)
                  (not (string-lower-case? (first line))))
              '()]
             [else (cons (first line)
                         (first-word (rest line)))])))
    (cond
     [(empty? line) '()]
     [(string-lower-case? (first line))
      (first-word line)]
     [else (first line)])))

;; Line -> Line
(define (remove-first-token line)
  (local (;; Line -> Line
          (define (remove-first-word line)
            (cond
             [(or (empty? line)
                  (not (string-lower-case? (first line))))
              line]
             [else (remove-first-word (rest line))]))

          ;; Line -> Line
          (define (strip-left-whitespaces line)
            (cond
             [(empty? line) '()]
             [(string-whitespace? (first line))
              (strip-left-whitespaces (rest line))]
             [else line])))
  (cond
   [(empty? line)'()]
   [(string-lower-case? (first line))
    (strip-left-whitespaces (remove-first-word line))]
   [else (strip-left-whitespaces (rest line))])))

;;================================
;; 454

;; a Matrix is:
;; (list [List-of Number*N]*N)

;; Number [List-of Number] -> Matrix
(check-expect (create-matrix 2 (list 1 2 3 4)) (list (list 1 2)
                                                     (list 3 4)))
(define (create-matrix n l)
  (cond
   [(empty? l) '()]
   [else (cons (take-first-n n l)
               (create-matrix n (remove-first-n n l)))]))


;; [X] Number [List-of X] -> [List-of X]
;; take the first n items from list
(define (take-first-n n l)
  (cond
   [(zero? n) '()]
   [else (cons (first l)
               (take-first-n (sub1 n) (rest l)))]))


;; [X] Number [List-of X] -> [List-of X]
;; remove the first n items from list
(define (remove-first-n n l)
  (cond
   [(zero? n) l]
   [else (remove-first-n (sub1 n) (rest l))]))
