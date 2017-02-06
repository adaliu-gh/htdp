;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12-212-214) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

;----------------------
; DATA DEFINITIONS
;----------------------

; a Word is one of:
; - '() or
; - (cons 1String Word)
; interpretation a String as a list of 1String
(define de (list "d" "e"))
(define ed (list "e" "d"))
(define cat (list "c" "a" "t"))
(define act (list "a" "c" "t"))

; a List-of-words is one of:
; - '()
; - (cons Word List-of-words)
(define lw (list de ed))
(define lwcat (list cat act))

;--------------------------------
; CONSTANTS
;--------------------------------
(define DICTIONARY-LOCATION "/usr/share/dict/words") ; My OS is Fedora
(define DICTIONARY-AS-LIST (read-lines DICTIONARY-LOCATION))
;---------------------
; EXERCISE 213
;---------------------

; 1String List-of-words -> List-of-words
; inserts a letter into a list of words
(check-expect (insert-everywhere/in-all-words "d" (list '())) (list (list "d")))
(check-expect (insert-everywhere/in-all-words "d" (list (list "e"))) (list de ed))
(define (insert-everywhere/in-all-words l w)
  (cond
    [(empty? w) '()]
    [else (append (insert-everywhere/one-word l (first w))
                (insert-everywhere/in-all-words l (rest w)))]))

; 1String Word -> List-of-words
; inserts a letter into a word
(check-expect (insert-everywhere/one-word "d" '()) (list (list "d")))
(check-expect (insert-everywhere/one-word "d" (list "e")) (list de ed))
(define (insert-everywhere/one-word l w)
  (cond
    [(empty? w) (list (list l))]
    [else (cons (cons l w)
                (add-at-first (first w)
                              (insert-everywhere/one-word l (rest w))))]))

; 1String List-of-words -> List-of-words
; adds letter l at all the beginnings of words in the given w
(check-expect (add-at-first "d" (list (list "e" "r") (list "r" "e")))
              (list (list "d" "e" "r") (list "d" "r" "e")))
(define (add-at-first l w)
  (cond
    [(empty? w) '()]
    [else (cons (cons l (first w))
                (add-at-first l (rest w)))]))

;-------------------------------
; EXERCISE 214
;-------------------------------

; String -> List-of-strings
; find all words that the letters of some given word spell
(check-member-of (alternative-words "cat")
                 (list "act" "cat")
                 (list "cat" "act"))
(define (alternative-words s)
  (in-dictionary
   (words->strings (arrangements (string->word s)))))

;---------------------
; OTHER FUNCTIONS
;---------------------

; Word -> List-of-words
; creates all rearrangements of the letters in w
(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere/in-all-words (first w)
                                          (arrangements (rest w)))]))

; String -> Word
; converts s to the chosen word representation
(check-expect (string->word "cat") cat)
(define (string->word s)
  (cond
    [(zero? (string-length s)) '()]
    [else (cons (substring s 0 1)
                (string->word
                 (substring s 1 (string-length s))))]))

; Word -> String
; converts w to a string
(check-expect (word->string cat) "cat")
(define (word->string w)
  (cond
    [(empty? w) ""]
    [else (string-append (first w)
                         (word->string (rest w)))]))

; List-of-words -> List-of-strings
; converts ws to a list of strings
(check-expect (words->strings lwcat) (list "cat" "act"))
(define (words->strings ws)
  (cond
    [(empty? ws) '()]
    [else (cons (word->string (first ws))
                (words->strings (rest ws)))]))

; List-of-strings -> List-of-strings
; extracts from the given s only words that are in dictionary
(check-expect (in-dictionary (list "cat" "tac")) (list "cat"))
(define (in-dictionary s)
  (cond
    [(empty? s) '()]
    [else (if (member? (first s) DICTIONARY-AS-LIST)
              (cons (first s) (in-dictionary (rest s)))
              (in-dictionary (rest s)))]))


