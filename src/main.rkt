#lang racket/base
(require "flowmark.rkt")
(require "zeta.rkt")
(require "repl.rkt")

(cond
  ((<= (vector-length (current-command-line-arguments)) 0)
   (repl))
  (else
   (process-file (vector-ref (current-command-line-arguments) 0)
                 (if (>= (vector-length (current-command-line-arguments)) 2)
                     (vector-ref (current-command-line-arguments) 1)
                     #f))))

