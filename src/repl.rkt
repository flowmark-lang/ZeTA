#lang racket/base
(require "flowmark.rkt")
(require "zeta.rkt")
(provide repl)

(define *prompt* "% ")
(define *meta-character* #\;)

(define (read-from-stdin)
  (let ((s (list)))
    (do ((ch (read-char (current-input-port))
             (read-char (current-input-port))))
        ((or (equal? ch *meta-character*)
             (eof-object? ch))
         (cond
           ((and (null? s) (eof-object? ch)) #f)
           (else (list->string (reverse s)))))
      (set! s (cons ch s)))))


(define (repl)
  (displayln "Welcome to Flowmark/ZeTA.")
  (define should-continue? #t)
  (register-fm-primitive
   "exit"
   (λ (p) (set! should-continue? #f)))
  (register-fm-primitive
   "error"
   (λ (p) (error "user break")))
  (define (repl-loop)
    (let ((input (begin (display *prompt*) (read-from-stdin))))
      (unless (equal? input #f)
        (evaluate-logic-document (parse-text input))
        (when should-continue?
          (repl-loop)))))
  (repl-loop))

