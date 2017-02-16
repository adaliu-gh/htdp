;;==========================
; A FSM is one of:
;   – '()
;   – (cons Transition FSM)

(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)

; FSM-State is a Color.

; interpretation A FSM represents the transitions that a
; finite state machine can take from one state to another
; in reaction to key strokes

(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))

;;============================
;;226

; State State -> Boolean
(define (state=? a b)
  (string=? a b))


;;===========================
;;228

; FSM FSM-State -> FSM-State
; finds the state representing current in transition
; and retrieve the next field
(check-expect (find fsm-traffic "red") "green")
(check-expect (find fsm-traffic "green") "yellow")
(define (find transitions current)
  (cond
    [(empty? transitions) (string-append "not found: " current)]
    [else (if (state=? current (transition-current (first transitions)))
              (transition-next (first transitions))
              (find (rest transitions) current))]))

