;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 11-190) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Lo1s -> Lol
; produces the list of all prefixes (in the form of Lo1S)
(define (prefixes l)
  (cond
    [(empty? l) '()]
    [else
     (cons l (prefixes (cut-tail l)))]))

; Lo1S -> Lo1s
; cuts the last 1String from the given list
(check-expect (cut-tail (list 1 2 3)) (list 1 2))
(check-expect (cut-tail (list 1)) '())
(define (cut-tail l)
  (cond
    [(empty? l) '()]
    [(empty? (rest l)) '()]
    [else (cons (first l)
                (cut-tail (rest l)))]))

; Lo1S -> Lol
; produces the list of all suffixes
(define (suffixes l)
  (cond
    [(empty? l) '()]
    [else (cons l (suffixes (rest l)))]))
