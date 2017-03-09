(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)

;; A HM-Word is a [List-of Letter or "_"]
;; interpretation "_" represents a letter to be guessed

(define LETTERS (explode "abcdefghijklmnopqrstABCDEFGHIJKLMNOPQRSTUVWXYZ"))

;; HM-Word N -> String
;; run a simplistic Hangman game, produce the current state
(define (play the-pick time-limit)
  (local ((define the-word  (explode the-pick))
          (define the-guess (make-list (length the-word) "_"))
                                        ; HM-Word -> HM-Word
          (define (do-nothing s) s)
                                        ; HM-Word KeyEvent -> HM-Word
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (compare-word the-word current-status ke)
                current-status)))
    (implode
     (big-bang the-guess ; HM-Word
               [to-draw render-word]
               [on-tick do-nothing 1 time-limit]
               [on-key  checked-compare]))))

;; HM-Word -> Image
(define (render-word w)
  (text (implode w) 22 "black"))

;;===========================
;;396

;; HM-Word HM-Word KeyEvent -> HM-Word
;; if ke in the-word, updates the current-status or remains unchanged
(check-expect (compare-word '("a" "b") '("_" "_") "a") '("a" "_"))
(check-expect (compare-word '("a" "b" "a") '("_" "_" "_") "a") '("a" "_" "a"))
(define (compare-word the-word current-status ke)
  (local (;; HM-Word HM-Word -> HM-Word
          (define (update-status the-word the-status)
            (cond
             [(empty? the-status) '()]
             [else (local ((define rest-updated
                             (update-status (rest the-word)
                                            (rest the-status))))
                     (if (equal? (first the-word) ke)
                         (cons ke rest-updated)
                         (cons (first the-status)
                               rest-updated)))])))
  (if (member? ke the-word)
      (update-status the-word current-status)
      current-status)))

(define LOCATION "/usr/share/dict/words") ; on OS X
(define AS-LIST (read-lines LOCATION))
(define SIZE (length AS-LIST))

