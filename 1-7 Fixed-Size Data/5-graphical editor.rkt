;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |5-graphical editor|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct editor [pre post])
; An Editor is a structure:
;    (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-appedn s t) with
; the cursor displayed between s and t

(define BACK (empty-scene 200 20))
(define CURSOR (rectangle 1 20 "solid" "red"))

;;---------------------
;; Auxiliary functions:
;;---------------------

; editor -> Image
; renders the text and the cursor without background image
(define (render-text e)
  (beside (text (editor-pre e) 11 "black")
          CURSOR
          (text (editor-post e) 11 "black")))

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

;;---------------------
;; Auxiliary functions.
;;---------------------


; editor -> Image
; renders the editor and places the cursor between editor-pre and editor-post
(define (render e)
  (overlay/xy (render-text e)
              0 0
              BACK))

; editor KeyEvent -> editor
; obtains new editor after pressing a key
; "\b" deletes the last character of the editor-pre
; "left" and "right" move the cursor to the right place (if any)
; other letter keys, such as "a" "b", adds a new character to the end of the editor-pre
; all other keys are ignored
(define (edit edi ke)
  (cond
   [(key=? ke "\b")
    (make-editor (string-last-remove (editor-pre edi)) (editor-post edi))]
   [(key=? ke "left")
    (make-editor (string-last-remove (editor-pre edi))
                 (string-append (string-last (editor-pre edi)) (editor-post edi)))]
   [(key=? ke "right")
    (make-editor (string-append (editor-pre edi) (string-first (editor-post edi)))
                 (string-rest (editor-post edi)))]
   [(and (= 1 (string-length ke))
         (not (key=? ke "\t"))
         (not (key=? ke "\r"))
         (> 33 (+ (string-length (editor-pre edi)) (string-length (editor-post edi)))))
    (make-editor (string-append (editor-pre edi) ke) (editor-post edi))]
   [else edi]))

; String -> editor
(define (run str)
  (big-bang (make-editor str "")
            [on-key edit]
            [to-draw render]))
