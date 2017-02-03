;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 5-81) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct time [hour minute second])
; a time is a structure:
; (make-time Number Number Number)
; interpretation the time since midnight

; time -> Number
; obtains the number of seconds that have passed since midnight
; given the time
; given (make-time 0 0 0), expected 0
; given (make-time 10 10 10), expected 36,610
(define (time->seconds t)
  (+ (* 3600 (time-hour t))
     (* 60 (time-minute t))
     (time-second t)))
