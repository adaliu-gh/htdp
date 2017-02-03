;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 10-174) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; 1String -> String
; converts the given 1String to a 3-letter numeric String
(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10)
     (string-append "00" (code1 s))]
    [(< (string->int s) 100)
     (string-append "0" (code1 s))]))

; 1String -> String
; convert the given 1String into a String
(check-expect (code1 "z") "122")
(define (code1 c)
  (number->string (string->int c)))

; String -> String
; converts the given word into a numeric string
(define (encode-word w)
  (cond
    [(string=? w "") ""]
    [else (string-append (encode-letter (substring  w 0 1))
                         (encode-word (substring w 1 (string-length w))))]))

; Low -> Low
; encodes a list of words
(check-expect (encode-line (cons "z" (cons "z" '()))) (cons "122" (cons "122" '())))
(define (encode-line l)
  (cond
    [(empty? l) '() ]
    [else (cons (encode-word (first l))
                (encode-line (rest l)))]))

; LLS -> LLS
; encodes a list of list of strings
(define (encode-lls l)
  (cond
    [(empty? l) '()]
    [else (cons (encode-line (first l))
                (encode-lls (rest l)))]))

; String -> LLS
; encodes a file
(define (encode-file n)
  (encode-lls (read-words/line n)))
