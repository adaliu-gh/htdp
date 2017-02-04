;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 11-187) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct gp [name score])
; A GamePlayer is a structure:
;   (make-gp String Number)
; interpretation (make-gp p s) represents player p who
; scored a maximum of s points
(define g1 (make-gp "Player1" 34))
(define g2 (make-gp "Player2" 44))

; a Log (short for List-of-game-players) is one of:
; - '()
; - (cons gp Log)
(define l1 (list g1 g2))
(define l3 (list g2 g1))
(define l2 (list g1))

; Log -> Boolean
; determines if the list of game players is in a descending order
(check-expect (sort>? l1) #false)
(check-expect (sort>? l2) #true)
(check-expect (sort>? '()) #true)
(check-expect (sort>? l3) #true)
(define (sort>? l)
  (cond
    [(empty? l) #true]
    [(empty? (rest l)) #true]
    [else (and (> (gp-score (first l))
                  (gp-score (first (rest l))))
               (sort>? (rest l)))]))

; gp Log -> Log
; inserts a player p into the sorted list of players l
(check-expect (insert g2 l2) l3)
(check-expect (insert g1 '()) l2)
(define (insert p l)
  (cond
    [(empty? l) (list p)]
    [else (if (>= (gp-score p) (gp-score (first l)))
              (cons p l)
              (cons (first l)
                    (insert p (rest l))))]))

; Log -> Log
; sorts a list of game players in a descending order
(check-satisfied (sort> l1) sort>?)
(check-satisfied (sort> l2) sort>?)
(check-satisfied (sort> l3) sort>?)
(check-satisfied (sort> '()) sort>?)
(define (sort> l)
  (cond
    [(empty? l) '()]
    [else (insert (first l)
                  (sort> (rest l)))]))

