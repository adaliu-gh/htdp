(require 2htdp/universe)
(require 2htdp/image)

;; Xexpr.v2 -> [List-of Xexpr.v2]
(define (xexpr-content xexpr)
  (local ((define optional-loa+content (rest xexpr)))
    (cond
     [(empty? optional-loa+content) '()]
     [else (local ((define loa-or-x
                     (first optional-loa+content)))
             (if (list-of-attributes? loa-or-x)
                 (rest optional-loa+content)
                 optional-loa+content))])))

;; [List-of Attribute] or Xexpr.v2 -> Boolean
;; is the given value a list of attributes
(define (list-of-attributes? x)
  (cond
   [(empty? x) #true]
   [else
    (local ((define possible-attribute (first x)))
      (cons? possible-attribute))]))

;; Xexpr.v2 -> [List-of Attribute]
(define (xexpr-attr x)
  (local ((define optional-loa+content (rest x)))
    (cond
     [(empty? optional-loa+content) '()]
     [else (local ((define loa-or-x
                     (first optional-loa+content)))
             (if (list-of-attributes? loa-or-x)
                 loa-or-x
                 '()))])))

;; AL Symbol -> String
(define (find-attr l s)
  (cond
   [(empty? l) #false]
   [else (if (equal? (first (first l)) s)
             (second (first l))
             (find-attr (rest l) s))]))

;; A FSM is a [List-of 1Transition]
;; A 1Transition is a list of two items:
;;   (cons FSM-State (cons FSM-State '()))
;; A FSM-State is a String that specifies a color

;; data examples
(define fsm-traffic
  '(("red" "green") ("green" "yellow") ("yellow" "red")))

;;==========================
;;378

;; FSM FSM-State -> FSM-State
;; match the keys pressed by a player with the given FSM
(define (simulate state0 transitions)
  (big-bang state0
            [to-draw
             (lambda (current)
               (overlay (text current 15 "black")
                        (square 100 "solid" current)))]
            [on-key
             (lambda (current key-event)
               (find transitions current))]))

;;==========================
;;379

;; [X Y] [List-of [List X Y]] X -> Y
;; finds the matching Y for the given X in alist
(check-expect (find '((1 2) (2 3)) 1) 2)
(check-error (find '(("e" "er")) "not found"))
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))

;;============================
;;380

;; a 1Transition is a list of two items:
;; (cons (list FSM-State KeyEvent) (cons FSM-State '()))

;; data examples
(define fsm-2 '((("red" "g") "green") (("green" "y") "yellow") (("yellow" "r")"red")))

;; FSM FSM-State -> FSM-State
(define (simulate.v2 state0 transitions)
  (big-bang state0
            [to-draw
             (lambda (current)
               (overlay (text current 15 "black")
                        (square 100 "solid" current)))]
            [on-key
             (lambda (current key-event)
               (find  transitions (list current key-event)))]))

;;=============================
;;381

;; an XMachine is a nested list of this shape:
;; (cons 'machine
;;      (cons (list (list 'initial FSM-State))
;;           [List-of X1T]))

;; an X1T is a nested list of this shape:
;; (cons 'action
;;       (cons (list (list 'state FSM-State)
;;                   (list 'next FSM-State)) '())

;;===========================
;;382

;; a BW machine in XML
;;<machine initial="black">
    ;;<action state="black" next="white"/>
    ;;<action state="white" next="black"/>
;;<machine/>

;; a BW machine in XMachine:
(define bw-machine
  '(machine ((initial "black"))
            (action ((state "black") (next "white")))
            (action ((state "white") (next "black")))))

;;=============================
;;383

;; XM -> FSM-State
(define (simulate-xm xm)
  (simulate (xm-initial xm) (xm-transitions xm)))

;; XM -> FSM-State
;; extract the initial state from xm
(check-expect (xm-initial bw-machine) "black")
(define (xm-initial xm)
  (find-attr (xexpr-attr xm) 'initial))

;; XM -> FSM
;; extract & convert the transitions in xm
(check-expect (xm-transitions bw-machine)
              '(("black" "white") ("white" "black")))
(define (xm-transitions xm)
  (local ((define (action->transition xa)
            (list (find-attr (xexpr-attr xa) 'state)
                  (find-attr (xexpr-attr xa) 'next))))
    (map action->transition (xexpr-content xm))))
