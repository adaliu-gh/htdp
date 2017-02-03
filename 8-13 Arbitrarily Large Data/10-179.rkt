;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 10-179) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; -------------------------
(define-struct editor [pre post])
; an Editor is a structure:
; (make-editor Lo1S Lo1S)

; an Lo1S is one of:
; - '()
; - (cons 1String Lo1S)

(define good (cons "g" (cons "o" (cons "o" (cons "d" '())))))
(define all (cons "a" (cons "l" (cons "l" '()))))
(define lla (cons "l" (cons "l" (cons "a" '()))))

; data-example 1:
(make-editor all good)

; data-example 2:
(make-editor lla good)

;--------------------------
(define HEIGHT 20)
(define WIDTH 200)
(define FONT-SIZE 16)
(define FONT-COLOR "black")

(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

;-------------------------
; String -> Lo1s
; converts a string s into a list of 1Strings
(check-expect (convert "a") (cons "a" '()))
(check-expect (convert "ab") (cons "a" (cons "b" '())))
(define (convert s)
  (cond
    [(string=? "" s) '()]
    [else (cons (substring s 0 1)
                (convert (substring s 1 (string-length s))))]))

; Lo1S -> Lo1S
; produces a reverse version of the given list of 1Strings
 (check-expect (rev all) lla)
(define (rev l )
  (cond
    [(empty? l) '()]
    [else (add-at-end (rev (rest l)) (first l))]))

; Lo1S 1String -> Lo1S
; creates a new list by adding s to the end of l
(check-expect (add-at-end (cons "a" (cons "l" '())) "l") all)
(define (add-at-end l s)
  (cond
    [(empty? l) (cons s '())]
    [else (cons (first l) (add-at-end (rest l) s))]))

; String String -> Editor
; creates an editor given s1 and s2
(check-expect (create-editor "a" "b") (make-editor (cons "a" '()) (cons "b" '())))
(define (create-editor s1 s2)
  (make-editor (rev (convert s1))
               (convert s2)))

; Editor 1String -> Editor
; adds a character at the end of the pre part of a given editor
(define (editor-ins ed k)
  (make-editor (cons k (editor-pre ed))
               (editor-post ed)))

; Editor -> Editor
; moves the cursor position one 1String left,
; if possible
(define (editor-lft ed)
  (cond
    [(empty? (editor-pre ed) ) ed]
    [else (make-editor (rest (editor-pre ed))
                       (cons (first (editor-pre ed))
                             (editor-post ed)))]))

; Editor -> Editor
; moves the cursor position one 1String right
; if possible
(define (editor-rgt ed)
  (cond
    [(empty? (editor-post ed) ) ed]
    [else (make-editor (cons (first (editor-post ed)) (editor-pre ed))
                       (rest (editor-post ed)))]))

; Editor -> Editor
; deletes a 1String to the left of the cursor
; if possible
(define (editor-del ed)
  (cond
    [(empty? (editor-pre ed) ) ed]
    [else (make-editor (rest (editor-pre ed))
                       (editor-post ed))]))

; Lo1S -> Image
; renders a list of 1Strings as a text image
(define (editor-text s)
  (cond
    [(empty? s) (text "" FONT-SIZE FONT-COLOR)]
    [else (beside (text (first s) FONT-SIZE FONT-COLOR)
                   (editor-text (rest s)))]))
;-----------------------

; Editor KeyEvent -> Editor
; deals with a key event, given some editor
(check-expect (editor-kh (create-editor "" "") "e")
              (create-editor "e" ""))
(check-expect (editor-kh (create-editor "cd" "fgh") "e")
              (create-editor "cde" "fgh"))
(check-expect (editor-kh (create-editor "c" "") "\b")
              (create-editor "" ""))
(check-expect (editor-kh (create-editor "" "") "\b")
              (create-editor "" ""))
(check-expect (editor-kh (create-editor "cd" "fgh") "left")
              (create-editor "c" "dfgh"))
(check-expect (editor-kh (create-editor "" "fgh") "left")
              (create-editor "" "fgh"))
(check-expect (editor-kh (create-editor "cd" "") "right")
              (create-editor "cd" ""))
(check-expect (editor-kh (create-editor "cd" "fgh") "right")
              (create-editor "cdf" "gh"))
(define (editor-kh ed k)
  (cond
    [(key=? k "left") (editor-lft ed)]
    [(key=? k "right") (editor-rgt ed)]
    [(key=? k "\b") (editor-del ed)]
    [(key=? k "\t") ed]
    [(key=? k "\r") ed]
    [(= (string-length k) 1) (editor-ins ed k)]
    [else ed]))

; Editor -> Image
(define (editor-render e)
  (place-image/align
   (beside (editor-text (rev (editor-pre e)))
            CURSOR
            (editor-text (editor-post e)))
   1 1
   "left" "top"
   MT))


; main: String -> Editor
; launches the editor given some initial string
(define (main s)
  (big-bang (create-editor s "")
            [on-key editor-kh]
            [to-draw editor-render]))
