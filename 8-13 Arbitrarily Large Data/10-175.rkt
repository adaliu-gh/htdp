;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 10-175) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

(define-struct wc [letters words lines])
; (make-wc Number Number Number) where letters represents the number of 1Strings
; the words represents the number of words
; the lines represents the number of line in a given file

; String -> Wc
; counts the number of letters, words, and lines in a given file
(define (count n)
  (make-wc (count-letters (read-words n))
           (length (read-words n))
           (length (read-lines n))))

; Low -> Number
; counts the number of 1Strings in a given list of words
(define (count-letters l)
  (cond
    [(empty? l) 0]
    [else (+ (string-length (first l))
             (count-letters (rest l)))]))
