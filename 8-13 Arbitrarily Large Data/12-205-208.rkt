;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12-205-208) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;-----------------------------------
; DATE DEFINITIONS
;-----------------------------------
; A BSDN is one of:
; – Boolean
; – Number
; – String
; – Date

; An Association is a list of two items:
;  (cons String (cons BSDN '()))
(define asso1 (list "time" 23))
(define asso2 (list "name" "Love Story"))
(define asso3 (list "boolean" #false))
(define asso4 (list "time" 34))
(define asso5 (list "name" "May It Be"))
(define asso6 (list "love?" #true))

; An LAssoc is one of:
; – '()
; – (cons Association LAssoc)
(define la1 (list asso1 asso2 asso3))
(define la2 (list asso4 asso5 asso6))

; An LLists is one of:
; – '()
; – (cons LAssoc LLists)
(define ll (list la1 la2))

;-----------------------------
; EXERCISE 206
;----------------------------
; String LAssoc Any -> Any
; produces the first Association whose first item is key
; or default if there is no such Association
(check-expect (find-association "name" la1 "none") asso2)
(check-expect (find-association "album" la2 "none") "none")
(define (find-association key l default)
  (cond
    [(empty? l) default]
    [else (if (string=? key (first (first l)))
              (first l)
              (find-association key (rest l) default))]))

;-----------------------------
; EXERCISE 207
;-----------------------------
; LLists -> Number
; produces the total amount of play time
(check-expect (total-time/list ll) 57)
(define (total-time/list l)
  (cond
    [(empty? l) 0]
    [else (+ (if (list? (find-association "time" (first l) #false))
                 (second (find-association "time" (first l) #false))
                 0)
             (total-time/list (rest l)))]))

;-------------------------------
; EXERCISE 208
;------------------------------
; LLists -> List-of-strings
; produces the strings that are associated with a Boolean attribute
(define (boolean-attributes l)
  (create-set (collapse (boolean-keys l)))
  )

; LLists -> List-of-strings
(define (boolean-keys l)
  (cond
    [(empty? l) '()]
    [else (cons (boolean-keys-assoc (first l))
                (boolean-keys (rest l)))]))

; Any LAssoc Any -> Any
; produces the strings that are associated with a Boolean attribute
(check-expect (boolean-keys-assoc la1) (list "boolean"))
(define (boolean-keys-assoc a)
  (cond
    [(empty? a) '()]
    [else (if (boolean? (second (first a)))
              (cons (first (first a)) (boolean-keys-assoc (rest a)))
              (boolean-keys-assoc (rest a)))]))

; List-of-lists -> List
; removes all nestings
(check-expect (collapse (list (list 1 2) (list 4 5))) (list 1 2 4 5))
(define (collapse l)
  (cond
    [(empty? l) '()]
    [else (cond
            [(empty? (first l)) (collapse (rest l))]
            [(list? (first l)) (cons (first (first l))
                                     (collapse (cons (rest (first l))
                                                     (rest l))))]
            [else (cons (first l) (collapse (rest l)))])]
    ))

; List -> List
; leaves only 1 occurrence of each item
(check-expect (create-set (list 1 1 2)) (list 1 2))
(define (create-set l)
  (cond
    [(empty? l) '()]
    [else (if (member? (first l) (rest l))
              (create-set (rest l))
              (cons (first l) (create-set (rest l))))]))
