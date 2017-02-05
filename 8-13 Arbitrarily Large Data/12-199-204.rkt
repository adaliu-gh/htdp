;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12-199-204) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;----------------------------
; DATA DEFINITIONS
;---------------------------
(define-struct date [year month day hour minute second])
; A Date is a structure:
;   (make-date N N N N N N)
; interpretation An instance records six pieces of information:
; the date's year, month (between 1 and 12 inclusive),
; day (between 1 and 31), hour (between 0
; and 23), minute (between 0 and 59), and
; second (also between 0 and 59).
(define d1 (make-date 2004 06 16 18 08 31))
(define d2 (make-date 2004 08 17 16 39 53))

(define-struct track
 [name artist album time track# added play# played])
; A Track is a structure:
;   (make-track String String String N N Date N Date)
; interpretation An instance records in order: the track's
; title, its producing artist, to which album it belongs,
; its playing time in milliseconds, its position with the
; album, the date it was added, how often it has been
; played, and the date when it was last played
(define t1 (make-track "Wild Child" "Enya"
                       "A Day Without Rain"
                       227996 2 d1 20 d2))
(define t2 (make-track "Landslide" "Fleetwood Mac"
                       "The Dance"
                       268283 9 d1 10 d2))

; the 2htdp/itunes library documentation, part 1:
; An LTracks is one of:
; – '()
; – (cons Track LTracks)
(define ltrack (list t1 t2))

;----------------------------------
; EXERCISE 200
;----------------------------------
; LTracks -> Number
; produces the total time of all tracks in the given list
(check-expect (total-time ltrack) (+ 227996 268283))
(define (total-time l)
  (cond
    [(empty? l) 0]
    [else (+ (track-time (first l))
             (total-time (rest l)))]))

;------------------------------
; EXERCISE 201
;------------------------------
; LTracks -> List-of-strings
; produces a list of album titles of a given list of tracks
(check-expect (select-all-album-titles ltrack) (list "A Day Without Rain" "The Dance"))
(define (select-all-album-titles l)
  (cond
    [(empty? l) '()]
    [else (cons (track-album (first l))
                (select-all-album-titles (rest l)))]))

; List-of-strings -> List-of-strings
; leaves only 1 occurrence of each string
(check-expect (create-set (list 1 1 2)) (list 1 2))
(define (create-set l)
  (cond
    [(empty? l) '()]
    [else (if (member? (first l) (rest l))
              (create-set (rest l))
              (cons (first l) (create-set (rest l))))]))

; LTracks -> List-of-strings
; produces a list of distinct album titles from a given list of tracks
(define (select-all-album-titles/unique l)
  (create-set (select-all-album-titles l)))

; ------------------------------
; EXERCISE 202
;-------------------------------
; String LTracks -> LTracks
; produces a list of all the tracks that belong to album al
(define (select-album al l)
  (cond
    [(empty? l) '()]
    [else (if (string=? al (track-album (first l)))
              (cons (first l) (select-album al (rest l)))
              (select-album al (rest l)))]))

;--------------------------------
; EXERCISE 203
;------------------------------
; String Date LTracks -> LTracks
; produces a list of all the tracks that belong to album a
; and have been played after the Date d
(define (select-album-date a d l)
  (cond
    [(empty? l) '()]
    [else (if (and (string=? a (track-album (first l)))
                   (after-date? (track-played (first l)) d))
              (cons (first l) (select-album-date a d (rest l)))
              (select-album-date a d (rest l)))]))

; Date Date -> Boolean
; determines whether the second date is after the first date
(check-expect (after-date? d1 d2) #true)
(define (after-date? d1 d2)
  (cond
    [(> (date-year d2) (date-year d1)) #true]
    [(> (date-month d2) (date-month d1)) #true]
    [(> (date-day d2) (date-day d1)) #true]
    [(> (date-hour d2) (date-hour d1)) #true]
    [(> (date-minute d2) (date-minute d1)) #true]
    [(> (date-second d2) (date-second d1)) #true]
    [else #false]))

; ---------------------------------
; EXERCISE 204
; ---------------------------------
; LTracks -> List-of-LTracks
; produces one list of tracks per album
(define (select-albums l)
  (select-albums-from (select-all-album-titles/unique l)
                      l))

; List-of-strings LTracks -> List-of-LTracks
; produces one list of tracks per album
(define (select-albums-from a l)
  (cond
    [(empty? a) '()]
    [else (cons (select-album (first a) l)
                (select-albums-from (rest a) l)) ]))
