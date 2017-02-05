;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12-195-198) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

;-------------------------------

; a Letter is one of the following 1Strings:
; - "a"
; - ...
; - "z"
; or, equivalently, a member? of this list:
(define LETTERS (explode "abcedfghijklmnopqrstuvwxyz"))

(define-struct lc [letter count])
; a Letter-Count is a structure:
;  (make-lc Letter Number)
; interpretation where count represents how often a word starts with a letter
(define lc1 (make-lc "a" 23))

; a Lol (short for List-of-Letter-Counts) is one of:
; - '()
; - (cons Letter-Count Lol)

; a Lod (short for List-of-Dictionaries) is one of:
; - '()
; - (cons Dictionary Lod)

;-----------------------------------

(define DICTIONARY-LOCATION "/usr/share/dict/words")   ; My OS is Fedora
(define DICTIONARY-AS-LIST (read-lines DICTIONARY-LOCATION))
(define TEST-DICT (list "hello" "world" "this" "that"))

;-----------------------------
; EXERCISE 195
;-----------------------------
; Letter Dictionary -> Number
; counts how many words in d start with l
(check-expect (starts-with# "t" TEST-DICT) 2)
(check-expect (starts-with# "h" TEST-DICT) 1)
(check-expect (starts-with# "z" TEST-DICT) 0)
(define (starts-with# l d)
  (cond
    [(empty? d) 0]
    [else (+ (if (string=? (substring (first d) 0 1) l)
                 1
                 0)
             (starts-with# l (rest d)))]))

;--------------------------------
; EXERCISE 196
;-------------------------------
; Dictionary -> Lol
; given a dictionary, produces a list of Letter-Counts
(define (count-by-letter d)
  (count-dict-letters LETTERS d))

; Dictionary -> Number
; produces the sum of legitimate words in a dictionary
(check-expect (sum TEST-DICT) 4)
(define (sum d)
  (sum-lc (count-by-letter d)))

;--------------------------------
; EXERCISE 197
;--------------------------------
; Dictionary -> Letter-Count
; produces the most frequent letter-count in the given dictionary d
(define (most-frequent d)
  (most-in-lcs (count-by-letter d)))

;--------------------------------
; EXERCISE 198
;--------------------------------
; Dictionary -> Lod
; produces a list of Dictionaries, one per letter
(check-expect (words-by-first-letter TEST-DICT) (list (list "hello" )
                                                       (list "this" "that")
                                                       (list "world")))
(define (words-by-first-letter d)
  (remove-empty
   (words-dict-first-letters LETTERS d)))

; Dictionary -> Letter-Count
; produces the most frequent letter-count in the given dictionary d
(check-expect (most-frequent.v2 TEST-DICT) (most-frequent TEST-DICT))
(define (most-frequent.v2 d)
  (longest (words-by-first-letter d)))

;--------------------------------
; AUXILIARY FUNCTIONS
;--------------------------------
; Letter Dictionary -> Dictionary
; produces a new dictionary whose words start with letter l
(check-expect (dict-starts-with# "t" TEST-DICT) (list "this" "that"))
(define (dict-starts-with# l d)
  (cond
    [(empty? d) '()]
    [else (if (string=? (substring (first d) 0 1) l)
              (cons (first d) (dict-starts-with# l (rest d)))
              (dict-starts-with# l (rest d)))]))

; List-of-letters Dictionary -> Lol
(define (count-dict-letters l d)
  (cond
    [(empty? l) '()]
    [else (cons (make-lc (first l)
                         (starts-with# (first l) d))
                (count-dict-letters (rest l) d))]))

; Lol -> Number
; produces the sum of letter-counts
(define (sum-lc l)
  (cond
    [(empty? l) 0]
    [else (+ (lc-count (first l))
             (sum-lc (rest l)))]))

; Lol -> Letter-Count
; produces the biggest Letter-Count in a given Lol
(define (most-in-lcs l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (if (>= (lc-count (first l))
                  (lc-count (first (rest l))))
              (most-in-lcs (cons (first l) (rest (rest l))))
              (most-in-lcs (rest l)))]))

; List-of-letters Dictionary -> Lod
(define (words-dict-first-letters l d)
  (cond
    [(empty? l) '()]
    [else (cons (dict-starts-with# (first l) d)
                (words-dict-first-letters (rest l) d))]))

; Lod -> Lod
; remove empty dictionary from the given dictionary
(check-expect (remove-empty (list '() 23 '())) (list 23))
(define (remove-empty d)
  (cond
    [(empty? d) '()]
    [else (if (empty? (first d))
              (remove-empty (rest d))
              (cons (first d) (remove-empty (rest d))))]))

; List-of-list -> Letter-Count
; extracts the longest list and gets its Letter-Count
(define (longest l)
  (cond
    [(empty? (rest l)) (make-lc (substring (first (first l)) 0 1)
                                (length (first l)))]
    [else (if (>= (length (first l)) (length (first (rest l))))
              (longest (cons (first l) (rest (rest l))))
              (longest (rest l)))]))

;--------------------------
