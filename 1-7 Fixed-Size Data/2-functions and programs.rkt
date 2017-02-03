;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2function) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; exercise 11
(define (distance x y)
  (sqrt (+ (sqr x)
           (sqr y))))

; exercise 12
(define (cvolume length)
  (* length length length))
(define (csurface length)
  (* length length 6))

; exercise 13
(define (string-first str)
  (substring str 0 1))

;exercise 14
(define (string-last str)
  (substring str (- (string-length str) 1) (string-length str)))

;exercise 15
(define (==> sunny friday)
  (if (or (not sunny) friday)
      #true
      #false))

;exercise 16
(define (image-area image)
  (* (image-width image)
     (image-height image)))

;exercise 17
(define (image-classify image)
  (cond
    [(< (image-height image) (image-width image)) "wide"]
    [(> (image-height image) (image-width image)) "tall"]
    [else "square"]))

;exercise 18

(define (string-join x y)
  (string-append x "_" y))

; exercise 19
(define (string-insert str i)
  (string-append
   (substring str 0 i)
   "_"
   (substring str i (string-length str))))

;exercise 20
(define (string-delete str i)
  (string-append
   (substring str 0 i)
   (substring str (+ i 1) (string-length str))))



;exercise 29
(define AVR-ATTEND 120)
(define AVR-PRICE 5)
(define PRICE-OVER-ATTEND 150)
(define DYNAMIC-COST 1.5)
(define (attendees ticket-price)
  (- AVR-ATTEND
     (* (- ticket-price AVR-PRICE)
        PRICE-OVER-ATTEND)))
(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))
(define (cost ticket-price)
  (* DYNAMIC-COST
     (attendees ticket-price)))
(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))