;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 10-172) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; Low -> String
; converts a list of words into a string
(define (w2s l)
  (cond
    [(empty? l) ""]
    [else (string-append (first l) " " (w2s (rest l)))]))

; LLS -> String
; converts a list of lines into a string
(define (collapse l)
  (cond
    [(empty? l) ""]
    [else (string-append
           (w2s (first l)) "\n" (collapse (rest l)))]))
