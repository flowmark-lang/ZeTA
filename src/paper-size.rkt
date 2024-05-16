#lang racket/base
(require (only-in racket/class
                  send*
                  send))
(require (only-in racket/contract
                  define/contract
                  ->
                  cons/c))
(require (only-in racket/draw
                  current-ps-setup))
(require (only-in racket/math
                  exact-round))
(provide *paper-size*
         *default-paper-class*
         paper-class->paper-size
         mm->horizontal-dc-unit
         mm->vertical-dc-unit
         inch->horizontal-dc-unit
         inch->vertical-dc-unit
         paper-size/c)

;; fix current-ps-setup
(send* (current-ps-setup)
  (set-margin 0.0 0.0)
  (set-editor-margin 0.0 0.0)
  (set-scaling 1 1)
  (set-orientation 'portrait))

;;; paper types & their sizes.
;;; racket's libary maintains a scaling factor and if we want to have arbitrary paper size
;;; we'll have to somehow get the value of this scaling factor. we'll get it from the current
;;; setup.
(define-values (*scaling-factor-x* *scaling-factor-y*)
  ((lambda ()
     (let ((b1 (box 0.0)) (b2 (box 0.0)))
       (send (current-ps-setup) get-scaling b1 b2)
       (values (unbox b1) (unbox b2))))))

;;; we use mm as the central length unit when dealing with users.

(define/contract (inch->horizontal-dc-unit x)
  (-> real? integer?)
  (exact-round (/ (* x 72) *scaling-factor-x*)))
(define/contract (inch->vertical-dc-unit x)
  (-> real? integer?)
  (exact-round (/ (* x 72) *scaling-factor-y*)))
(define/contract (mm->horizontal-dc-unit x)
  (-> real? integer?)
  (exact-round (/ (* (/ x 25.4) 72) *scaling-factor-x*)))
(define/contract (mm->vertical-dc-unit x)
  (-> real? integer?)
  (exact-round (/ (* (/ x 25.4) 72) *scaling-factor-y*)))

(define paper-size/c
  (cons/c integer? integer?))

;;; in postscript the minimal unit is point, with 72 point = 1 inch
(define/contract (make-paper-size/mm width-mm height-mm)
  (-> real? real? paper-size/c)
  (cons (exact-round (/ (* (/ width-mm 25.4) 72) *scaling-factor-x*))
        (exact-round (/ (* (/ height-mm 25.4) 72) *scaling-factor-y*))))
(define/contract (make-paper-size/in width-in height-in)
  (-> real? real? paper-size/c)
  (cons (exact-round (/ (* width-in 72) *scaling-factor-x*))
        (exact-round (/ (* width-in 72) *scaling-factor-y*))))
(define *paper-size*
  `((4a0 . ,(make-paper-size/mm 1682 2378))
    (2a0 . ,(make-paper-size/mm 1198 1682))
    (a0 . ,(make-paper-size/mm 841 1189))
    (a1 . ,(make-paper-size/mm 594 841))
    (a2 . ,(make-paper-size/mm 420 594))
    (a3 . ,(make-paper-size/mm 297 420))
    (a4 . ,(make-paper-size/mm 210 297))
    (a5 . ,(make-paper-size/mm 148 210))
    (a6 . ,(make-paper-size/mm 105 148))
    (a7 . ,(make-paper-size/mm 74 105))
    (a8 . ,(make-paper-size/mm 52 74))
    (a9 . ,(make-paper-size/mm 37 52))
    (a10 . ,(make-paper-size/mm 26 37))
    (b0 . ,(make-paper-size/mm 1000 1414))
    (b1 . ,(make-paper-size/mm 707 1000))
    (b2 . ,(make-paper-size/mm 500 707))
    (b3 . ,(make-paper-size/mm 353 500))
    (b4 . ,(make-paper-size/mm 250 353))
    (b5 . ,(make-paper-size/mm 176 250))
    (b6 . ,(make-paper-size/mm 125 176))
    (b7 . ,(make-paper-size/mm 88 125))
    (b8 . ,(make-paper-size/mm 62 88))
    (b9 . ,(make-paper-size/mm 44 62))
    (b10 . ,(make-paper-size/mm 31 44))
    (c0 . ,(make-paper-size/mm 917 1297))
    (c1 . ,(make-paper-size/mm 648 917))
    (c2 . ,(make-paper-size/mm 458 648))
    (c3 . ,(make-paper-size/mm 324 458))
    (c4 . ,(make-paper-size/mm 229 324))
    (c5 . ,(make-paper-size/mm 162 229))
    (c6 . ,(make-paper-size/mm 114 162))
    (c7 . ,(make-paper-size/mm 81 114))
    (c8 . ,(make-paper-size/mm 57 81))
    (c9 . ,(make-paper-size/mm 40 57))
    (c10 . ,(make-paper-size/mm 28 40))
    (letter . ,(make-paper-size/in 8.5 11))
    (legal . ,(make-paper-size/in 8.5 14))
    (ledger . ,(make-paper-size/in 17 11))
    (tabloid . ,(make-paper-size/in 11 17))
    (ansi-a . ,(make-paper-size/in 8.5 11))
    (ansi-b . ,(make-paper-size/in 11 17))
    (ansi-c . ,(make-paper-size/in 17 22))
    (ansi-d . ,(make-paper-size/in 22 34))
    (ansi-e . ,(make-paper-size/in 34 44))
    (arch-a . ,(make-paper-size/in 9 12))
    (arch-b . ,(make-paper-size/in 12 18))
    (arch-c . ,(make-paper-size/in 18 24))
    (arch-d . ,(make-paper-size/in 24 36))
    (arch-e1 . ,(make-paper-size/in 30 42))
    (arch-e2 . ,(make-paper-size/in 26 38))
    (arch-e3 . ,(make-paper-size/in 27 39))
    (arch-e . ,(make-paper-size/in 36 48))
    ))
(define *default-paper-class* 'a4)
(define (paper-class->paper-size cls)
  (let ((x (assoc cls *paper-size*)))
    (if x
        (cdr x)
        (error (format "~s: ~s" "Unsupported paper class" cls)))))
