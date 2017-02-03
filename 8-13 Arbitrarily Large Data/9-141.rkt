;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9-141) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-strings is one of:
; - '()
; - (cons String List-of-strings)
(define s1 '())
(define s2 (cons "a" (cons "b" s1)))
(define s3 (cons "ab" (cons "cd" (cons "ef" s1))))

; List-of-strings -> String
; appends all the elements of a List-of-strings into one long string
(check-expect (cat s1) "")
(check-expect (cat s2) "ab")
(check-expect (cat s3) "abcdef")
(define (cat l)
  (cond
    [(empty? l) ""]
    [(cons? l)
     (string-append (first l) (cat (rest l)))]))