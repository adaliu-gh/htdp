;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 10-173) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

(define ARTICLES (cons "a" (cons "an" (cons "the" '()))))

; String -> String
; removes all the articles in the given file
(define (remove-article* n)
  (remove-article (read-words n)))

; Low -> String
; removes all the articles in the given list of words
(define (remove-article low)
  (cond
    [(empty? low) ""]
    [else (if (member? (first low) ARTICLES)
              (remove-article (rest low))
              (string-append (first low)
                             " " (remove-article (rest low))))]))
