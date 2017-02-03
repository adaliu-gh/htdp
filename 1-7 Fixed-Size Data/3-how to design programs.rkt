;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |3-how to design programs|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; exercise 34
; String -> String
; returns the first character of a string str
; given: "ada", expected: "a"
; given: "world", expected: "w"
(define (string-first str)
  (substring str 0 1))

; exercise 35
; String -> String
; returns the last character of a string str
; given: "ada", expected: "a"
; given: "world", expected: "d"
(define (string-last str)
  (substring str (- (string-length str) 1) (string-length str)))

; exercise 36
; Image -> Number
; returns the numbers of pixels in a given image img
; given: (circle 5 "solid" "red"), expected: 100
; given: (rectangle 20 20 "solid" "red"), expected: 400
(define (image-area img)
  (* (image-width img) (image-height img)))

; exercise 37
; String -> String
; retursn the rest of a string str with the first character removed
; given: "ada", expected: "da"
; given: "world", expected: "orld"
(define (string-rest str)
  (substring str 1 (string-length str)))

; exercise 38
; String -> String
; returns the rest of a string str with the last character removed
; given: "ada", expected: "ad"
; given: "world", expected: "worl"
(define (string-remove-last str)
  (substring str 0 (- (string-length str) 1)))