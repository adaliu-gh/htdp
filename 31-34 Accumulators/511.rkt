(require 2htdp/batch-io)


;;===========================
;; 511

;; Number String String -> String?
;; rearrange a file so that the lines have a maximal width
(define (fmt w in-f out-f)
  (local (
          ;; [List-of String] -> [List-of String]
          (define (rearrange/list los)
            (cond
             [(empty? los) '()]
             [else (append (rearrange/line (first los))
                           (rearrange/list (rest los) ) )]))

          ;; String -> [List-of String]
          (define (rearrange/line string)
            (cond
             [(<= (string-length string) w)
              (list string)]
             [else (rearrange/list (list
                                 (substring string 0 w)
                                 (substring string w (string-length string))))]))

          ;; [List-of String] -> String
          (define (list->string los)
            (cond
             [(empty? los) ""]
             [else (string-append (first los) "\n"
                                  (list->string (rest los)))])))
    (write-file out-f (list->string
                       (rearrange/list (read-lines in-f)) ))))


;;=============================
;; Accumulator Version


;; Number String String -> String?
;; rearrange a file so that the lines have a maximal width
(define (fmt.v2 w in-f out-f)
  (local (
          ;; [List-of String] String -> [List-of String]
          (define (rearrange/list los a)
            (cond
             [(empty? los) a]
             [else (rearrange/list (rest los)
                                   (string-append a
                                                  (rearrange/line (first los))))]))

          ;; String -> [List-of String]
          (define (rearrange/line string)
            (cond
             [(<= (string-length string) w)
              (string-append string "\n")]
             [else (rearrange/list (list (substring string 0 w)
                                         (substring string w (string-length string))) "")])))

    (write-file out-f
                (rearrange/list (read-lines in-f) "") )))
