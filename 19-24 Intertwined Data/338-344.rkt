(require 2htdp/abstraction)
(require htdp/dir)

;;===================
;;338

(define working-dir (create-dir "/home/ada/code/htdp/"))
(define f1 (make-file "f1" 1 ""))
(define f2 (make-file "f2" 1 ""))
(define d1 (make-dir "d1" '() (list f1 f2)))
(define d2 (make-dir "d2" (list d1) '()))

;; Dir -> Number
;; counts how many files the given d contains
(define (how-many d)
  (+ (length (dir-files d))
     (foldl (lambda (x y) (+ y (how-many x)))
            0 (dir-dirs d))))

;;=====================
;;339

;; Dir String -> Boolean
;; determines if the file f exists in the given d
(check-expect (find? working-dir "330-336.rkt") #true)
(check-expect (find? working-dir "330-336.ss") #false)
(define (find? d f)
  (or (ormap (lambda (x) (string=? f (file-name x))) (dir-files d))
      (ormap (lambda (x) (find? x f)) (dir-dirs d))))

;;=========================
;;340

;; Dir -> [List-of String]
;; lists all files and dirs in the given d
(define (ls d)
  (append (map (lambda (x) (file-name x)) (dir-files d))
          (map (lambda (x) (dir-name x)) (dir-dirs d))))

;;========================
;;341

;; Dir -> Number
;; computes the total size of a directory tree d
(define (du d)
  (+ 1 (foldl (lambda (x y) (+ y (file-size x))) 0 (dir-files d))
     (foldl (lambda (x y) (+ y (du x))) 0 (dir-dirs d))))

;;============================
;;342

;; Dir String -> Boolean
;; determines if the file is directly in the given d
(define (in-files? d f)
  (ormap (lambda (x) (string=? f (file-name x))) (dir-files d)))

;; Dir String -> [Maybe [Path]]
(define (find d f)
  (local (
          ;; [List-of Dir] -> Dir
          ;; gets the subdirectory where is the f is
          (define (get-sub l)
            (cond
             [(empty? (rest l)) (rest l) ]
             [else (if (find? (first l) f) (first l) (get-sub (rest l)))])))
    (cond
     [(in-files? d f) (list (dir-name d) f)]
     [(find? d f) (cons (dir-name d) (find (get-sub (dir-dirs d)) f))]
     [else #false])))

;;============================
;;342 challenge

;; Dir String -> [List-of Path]
(define (find-all d f)
  (local (
          ;; Dir -> [List-of Path]
          (define (all-valid-paths d)
            (append (if (in-files? d f) (list (list f)) '() )
                    (foldl (lambda (x y)
                             (append y (find-all x f)))
                           '() (dir-dirs d)))))

    (map (lambda (x) (cons (dir-name d) x)) ( all-valid-paths d))))
;;========================
;;343

;; Dir -> [List-of Path]
(define (ls-R d)
  (local (;; Dir -> [List-of Path]
          (define (all-sub-paths d)
            (append (map (lambda (f) (list (file-name f))) (dir-files d))
                    (foldl (lambda (x y)
                             (append y (ls-R x)))
                           '() (dir-dirs d)))))
    (map (lambda (x) (cons (dir-name d) x)) (all-sub-paths d))))

;; [List-of Item] -> Item
(define (last l)
  (cond
   [(empty? (rest l)) (first l)]
   [else (last (rest l))]))

;;=====================
;;344

;; Dir String -> [List-of Path]
(define (find-all.v2 d f)
  (filter (lambda (p) (equal? f (last p))) (ls-R d)))
