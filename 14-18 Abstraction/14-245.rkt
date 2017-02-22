;;======================
;;245

;; Function Function -> Boolean
;; determines if two functions are equal
(define (function=at-1.2-3-and-5.775? f1 f2)
  (and (= (f1 1.2) (f2 1.2))
       (= (f1 3) (f2 3))
       (= (f1 -5.775) (f2 -5.775))))
