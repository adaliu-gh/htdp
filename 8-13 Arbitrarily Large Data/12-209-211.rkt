;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12-209-211) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;------------------------------
; REQUIREMENTS
;-----------------------------

(require 2htdp/batch-io)

;-------------------------------
; DATA DEFINITION
;------------------------------

; a Word is one of:
; - '()
; - (cons 1String Word)
(define cat (list "c" "a" "t"))
(define act (list "a" "c" "t"))

; a List-of-words is one of:
; - '()
; - (cons Word List-of-words)
(define lwcat (list cat act))

;---------------------------------
; CONSTANTS
;---------------------------------

(define DICTIONARY-LOCATION "/usr/share/dict/words")
(define DICTIONARY-AS-LIST (read-lines DICTIONARY-LOCATION))

;--------------------------------
; EXERCISE 209
;--------------------------------

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

;-----------------------------
; EXERCISE 210
;-----------------------------

; List-of-words -> List-of-strings
; converts ws to a list of strings
(check-expect (words->strings lwcat) (list "cat" "act"))
(define (words->strings ws)
  (cond
    [(empty? ws) '()]
    [else (cons (word->string (first ws))
                (words->strings (rest ws)))]))

;---------------------------------
; EXERCISE 211
;---------------------------------

; List-of-strings -> List-of-strings
; extracts from the given s only words that are in dictionary
(check-expect (in-dictionary (list "cat" "tac")) (list "cat"))
(define (in-dictionary s)
  (cond
    [(empty? s) '()]
    [else (if (member? (first s) DICTIONARY-AS-LIST)
              (cons (first s) (in-dictionary (rest s)))
              (in-dictionary (rest s)))]))
