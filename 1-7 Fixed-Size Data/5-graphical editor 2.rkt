;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |5-graphical editor 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct editor [content cursor])
; An editor is a structture:
;   (make-editor String Number)
; interpretation (make-editor s n) describes an editor
; whose visible text is s with
; the cursor n characters from the beginning

(define BACK (empty-scene 200 20))
(define CURSOR (rectangle 1 20 "solid" "red"))

;;---------------------
;; Auxiliary functions:
;;---------------------

; editor -> String
; gets the text before the cursor
(define (get-pre ed)
  (if (string=? (editor-content ed) "")
      ""
      (substring (editor-content ed) 0 (editor-cursor ed))))

; editor -> String
; gets the text after the cursor
(define (get-post ed)
   (if (string=? (editor-content ed) "")
      ""
      (substring (editor-content ed)
                 (editor-cursor ed)
                 (string-length (editor-content ed)))))

; editor -> Image
; renders the text and the cursor without background image
(define (render-text ed)
  (beside (text (get-pre ed) 11 "black")
          CURSOR
          (text (get-post ed) 11 "black")))

; String -> String
; gets the first character of String str
(define (string-first str)
  (if (> (string-length str) 0)
      (substring str 0 1)
      ""))

; String -> String
; get the last character of String str
(define (string-last str)
  (if (> (string-length str) 0)
      (substring str (- (string-length str) 1) (string-length str))
      ""))

; String -> String
; gets the new string after removing the last character of the original string str
(define (string-last-remove str)
  (if (> (string-length str) 0)
      (substring str 0 (- (string-length str) 1))
      ""))

; String -> String
; gets the new string after chopping the first character off
(define (string-rest str)
  (if (> (string-length str) 0)
      (substring str 1 (string-length str))
      ""))
; editor Number -> Number
; gets the new position of cursor by Number n when pressing "left" or "right" if
; the cursor doesn't cross the boundaries of text
(define (move-cursor ed n)
  (if (or (< (+ (editor-cursor ed) n) 0)
          (> (+ (editor-cursor ed) n) (string-length (editor-content ed))))
      (editor-cursor ed)
      (+ (editor-cursor ed) n)))
;;---------------------
;; Auxiliary functions.
;;---------------------

; editor -> Image
; renders the editor and places the cursor by editor-cursor characters from the beginning of the text
(define (render ed)
  (overlay/xy (render-text ed)
              0 0
              BACK))

; editor KeyEvent -> editor
; obtains new editor after pressing a key
; "\b" deletes the last character of the editor-pre
; "left" and "right" move the cursor to the right place (if any)
; other letter keys, such as "a" "b", adds a new character to the end of the editor-pre
; all other keys are ignored
(define (edit ed ke)
  (cond
   [(key=? ke "\b")
    (make-editor (string-append (string-last-remove (get-pre ed)) (get-post ed)) (move-cursor ed -1))]
   [(key=? ke "left")
    (make-editor (editor-content ed) (move-cursor ed -1))]
   [(key=? ke "right")
    (make-editor (editor-content ed) (move-cursor ed 1))]
   [(and (= 1 (string-length ke))
         (not (key=? ke "\t"))
         (not (key=? ke "\r"))
         (> 33 (string-length (editor-content ed))))
    (make-editor (string-append (get-pre ed) ke (get-post ed))
                 (+ (editor-cursor ed) 1))]
   [else ed]))

; String -> editor
(define (run str)
  (big-bang (make-editor str  (string-length str))
            [on-key edit]
            [to-draw render]))
