(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 20)
(define WIDTH 200)
(define FONT-SIZE 16)
(define FONT-COLOR "black")

(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

;; [List-of 1String] -> Image
;; render a string as an image for the editor
(define (editor-text s)
  (text (implode s) FONT-SIZE FONT-COLOR))

(define-struct editor [pre post])
;; an editor is a structure:
;; (make-editor [List-of 1String] [List-of 1String])
;; interpretation if (make-editor p s) is the state of
;; the text to the left of the cursor and s to the
;; text on the right

;;=======================
;; 509 -> 510

;; [List-of 1String] Number -> Editor
(define (split-structural ed x)
  (local ((define (split-structural/a pre post)
            (cond
             [(empty? post)
              (make-editor pre post)]
             [else (local ((define d1 (- x (image-width (editor-text pre))))
                           (define d2 (- (image-width (editor-text (cons (first post) pre))) x)))
                     (cond
                      [(<= 0 d1 d2)
                       (make-editor pre post)]
                      [(<= 0 d2 d1)
                       (make-editor (cons (first post) pre) (rest post))]
                      [else (split-structural/a (cons (first post) pre) (rest post))]))])))
    (split-structural/a '() ed)))


;;==========================
;; 510

;; Editor -> Editor
(define (main ed)
  (big-bang (create-editor ed "")
            [on-key editor-kh]
            [to-draw editor-render]
            [on-mouse split]))

;; Editor Number Number MouseEvent-> Editor
(define (split ed x y me)
  (split-structural (append (reverse (editor-pre ed))
                            (editor-post ed)) x))

;; Editor -> Image
(define (editor-render ed)
  (place-image/align
   (beside (editor-text (reverse (editor-pre ed)))
           CURSOR
           (editor-text (editor-post ed)))
   1 1 "left" "top"  MT))

;; String String -> Editor
(define (create-editor s1 s2)
  (make-editor (reverse (explode s1) ) (explode s2)))


;; Editor KeyEvent -> Editor
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
  (local ((define pre (editor-pre ed))
          (define post (editor-post ed))


          (define (editor-lft ed)
              (cond
               [(empty? pre) ed]
               [else (make-editor (rest pre) (cons (first pre) post))]))


          (define (editor-rgt ed)
            (cond
             [(empty? post) ed]
             [else (make-editor (cons (first post) pre) (rest post))]))

          (define (editor-del ed)
            (cond
             [(empty? pre) ed]
             [else (make-editor (rest pre) post)]))

          (define (editor-ins ed k)
            (make-editor (cons k pre) post)))
      (cond
       [(key=? k "left") (editor-lft ed)]
       [(key=? k "right") (editor-rgt ed)]
       [(key=? k "\b") (editor-del ed)]
       [(key=? k "\t") ed]
       [(key=? k "\r") ed]
       [(= (string-length k) 1) (editor-ins ed k)]
       [else ed])))

