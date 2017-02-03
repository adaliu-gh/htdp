;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9-142) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
; ImageOrFalse is one of:
; - Image
; - #false

; a List-of-iamges if one of:
; - '()
; - (cons Image List-of-image)

; List-of-images Number -> ImageOrFalse
; produces the first image on loi that is not an n by n square;
; if it cannot find such an image, it produces #false

(define (ill-sized? loi n)
  (cond
    [(empty? loi) #false]
    [(cons? loi)
     (if (and (= n (image-width (first loi)))
              (= n (image-height (first loi))))
         (ill-sized (rest loi))
         (first loi))]))