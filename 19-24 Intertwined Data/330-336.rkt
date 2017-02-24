(require 2htdp/abstraction)

;;======================
;;330

;; a Dir.v1 (short for directory) is one of:
;; - '()
;; - (cons File.v1 Dir.v1)
;; - (cons Dir.v1 Dir.v1)

;; a File.v1 is a String

(define part1 "part1-99")
(define part2 "part2-52")
(define part3 "part3-17")
(define hang "hang-8")
(define draw "draw-2")
(define read1 "read!-10")
(define read2 "read!-19")
(define Text (list part1 part2 part3))
(define Code (list hang draw))
(define Docs (list read2))
(define Libs (list Code Docs))
(define TS (list Text read1 Libs))


;;===========================
;;331

;; Dir.v1 -> Number
;; counts how many files a d contains
(check-expect (how-many.v1 TS) 7)
(define (how-many.v1 d)
  (foldl (lambda (x y)
           (+ y (if (string? x) 1 (how-many.v1 x))))
         0 d))

;;======================
;;332

;; a Dir.v2 is a structure:
(define-struct dir [name content])
;; - (make-dir String LOFD)

;; a LOFD (short for list of files and directories) is one of:
;; - '()
;; - (cons File.v2 LOFD)
;; - (cons Dir.v2 LOFD)

;; A File.v2 is a String

(define v2.part1 "part1-99")
(define v2.part2 "part2-52")
(define v2.part3 "part3-17")
(define v2.hang "hang-8")
(define v2.draw "draw-2")
(define v2.read1 "read!-10")
(define v2.read2 "read!-19")
(define text (make-dir "text" (list v2.part1 v2.part2 v2.part3)))
(define code (make-dir "code" (list v2.hang v2.draw)))
(define docs (make-dir "docs" (list v2.read2)))
(define libs (make-dir "libs" (list code docs)))
(define ts (make-dir "ts" (list text v2.read1 libs)))


;;==========================
;;333

;; Dir.v2 -> Number
;; counts how many files a d contains
(check-expect (how-many.v2 ts) 7)
(define (how-many.v2 d)
  (foldl (lambda (x y)
           (+ y (if (string? x) 1 (how-many.v2 x))))
         0 (dir-content d)))

;;======================
;;334

;; a Dir.my is a structure:
(define-struct dir-my [name content size readability])

;;==============================
;;335

;; a File.v3 is a structure:
(define-struct file [name size content])
;; name - String; size - Number; content - String

;; a Dir.v3 is a structure:
(define-struct dir.v3 [name dirs files])
;; name - String; dirs - Dir*; files - File*

;; a Dir* is:
;; [List-of Dir.v3]

;; a File* is one of:
;; [List-of File.v3]

(define part1.v3 (make-file "part1" 99 "" ))
(define part2.v3 (make-file "part2" 52 ""))
(define part3.v3 (make-file "part3" 17 ""))
(define text.v3 (make-dir.v3 "text" '() (list part1.v3 part2.v3 part3.v3)))

(define hang.v3 (make-file "hang" 8 ""))
(define draw.v3 (make-file "draw" 2 ""))
(define code.v3 (make-dir.v3 "code" '() (list hang.v3 draw.v3)))

(define read2.v3 (make-file "read!" 19 ""))
(define docs.v3 (make-dir.v3 "docs" '() (list read2.v3)))
(define libs.v3 (make-dir.v3 "libs" (list code.v3 docs.v3) '()))

(define read1.v3 (make-file "read!" 10 ""))
(define ts.v3 (make-dir.v3 "ts" (list text.v3 libs.v3) (list read1.v3)))

;;=====================
;;336

;; Dir.v3 -> Number
;; counts how many files a d contains
(check-expect (how-many.v3 ts.v3) 7)
(define (how-many.v3 d)
  (+ (length (dir.v3-files d))
     (foldl (lambda (x y) (+ y (how-many.v3 x)))
            0 (dir.v3-dirs d))))
