#lang racket/base
(require racket/class)
(require (only-in racket/draw
                  pdf-dc%
                  current-ps-setup
                  make-font
                  the-pen-list
                  font%))
(require racket/string)
(require racket/contract)
(require (only-in racket/math
                  exact-round))
(require (only-in racket/file
                  file->string))
(require (only-in racket/list
                  append*
                  dropf))
(require data/queue)
(require "util.rkt")
(require "flowmark.rkt")
(require "paper-size.rkt")
(provide process-file)

;;;;;; NOTE: all names, when not specified otherwise, are using dc-unit by default.

;;; font context. associated with each text-piece.
(struct/contract font-context ([bold boolean?]
                               [italics boolean?]
                               [size integer?]
                               [family any/c])
                 #:transparent
                 #:mutable)
(define (clone-font-context x)
  (struct-copy font-context x))

;;;;;; graphical piece types.
;;; text-piece. not to be confused for word-piece in flowmark.rkt; this is the
;;; model of a piece of text to be written on the page.
;;; the x and y field is filled by the line typesetter.
(struct/contract text-piece ([text string?]
                             [width real?]
                             [height real?]
                             [x (or/c real? #f)]
                             [y (or/c real? #f)]
                             [font (or/c (is-a?/c font%) #f)])
                 #:transparent
                 #:mutable)

;; debugging utilities
(define *dotted-pen* (send the-pen-list find-or-create-pen
                           "black" 1 'dot 'projecting))
(define *solid-pen* (send the-pen-list find-or-create-pen
                          "black" 1 'solid 'projecting))

;; empty space between words is called "glue" like TeX.
(define graphic-piece/c
  (or/c text-piece? 'glue))

(define (set-graphic-piece-x! p x)
  (cond
    ((text-piece? p) (set-text-piece-x! p x))))
(define (set-graphic-piece-y! p y)
  (cond
    ((text-piece? p) (set-text-piece-y! p y))))
(define (get-graphic-piece-x p)
  (cond
    ((text-piece? p) (text-piece-x p))))
(define (get-graphic-piece-y p)
  (cond
    ((text-piece? p) (text-piece-y p))))

;;; used during the typesetting process.
(struct/contract typeset-pointer ([y-offset real?]
                                  [current-height real?]
                                  [text-width real?]
                                  [min-glue-width real?]
                                  [max-glue-width real?]
                                  [line-height real?]
                                  [line-list (listof graphic-piece/c)]
                                  [page-piece-list (listof graphic-piece/c)]
                                  [first-line? boolean?]
                                  [first-paragraph? boolean?])
                 #:transparent
                 #:mutable)     

(struct/contract subdoc ([width real?]
                         [height (or/c real? #f)]  ;; #f means open-bottomed.
                         [left-margin real?]
                         [right-margin real?]
                         [font-family-to-face hash?]
                         [font-table hash?]
                         [font-context font-context?]
                         [params hash?]
                         [typeset-pointer typeset-pointer?]
                         [skip-when-empty boolean?])
                 #:transparent
                 #:mutable)
(struct/contract vertical-spacer ([height (or/c real? #f)])
                 ;; #f as height means a vertical spacer that expand as much as possible
                 ;; automatically, i.e. 0 when checking for vertical overflow and "maximum"
                 ;; when flushing a page.
                 #:transparent
                 #:mutable)
(struct/contract document-session ([subdoc-list hash?]
                                   [subdoc-index-list (listof (or/c symbol? vertical-spacer?))]
                                   [global-paper-size paper-size/c]
                                   [output-port output-port?]
                                   [dc (is-a?/c pdf-dc%)]
                                   [current-subdoc-index symbol?])
                 #:transparent
                 #:mutable)

;;; default font-family-to-face
(define (make-default-font-family-to-face)
  (make-hash
   #;`((roman . "Nimbus Roman No9 L")
     (default . "Nimbus Sans L")
     (modern . "Nimbus Mono L"))
   `((roman . "CMU Serif")
     (default . "CMU Sans")
     (modern . "CMU Typewriter"))))

(define (_make-font-set size family [face #f])
  (make-hash
   `((regular . ,(make-font #:size size
                            #:face face
                            #:family family
                            #:style 'normal
                            #:weight 'normal))
     (bold . ,(make-font #:size size
                         #:face face
                         #:family family
                         #:style 'normal
                         #:weight 'bold))
     (italic . ,(make-font #:size size
                           #:face face
                           #:family family
                           #:style 'italic
                           #:weight 'normal))
     (bold-italic . ,(make-font #:size size
                                #:face face
                                #:family family
                                #:style 'italic
                                #:weight 'bold)))))
(define (generate-font-table m)
  (make-hash
   (for/list ([family (in-list (list 'roman 'default 'modern))])
     (cons family
           (make-hash
            (for/list ([size (in-list (list 8 9 10 12 16 20 24 28 32 36 40 48 56 64 72 96))])
              (cons size (_make-font-set
                          size
                          family
                          (hash-ref m family)))))))))

(define (font-context->font sd ctx)
  (let* ((family (font-context-family ctx))
         (size (font-context-size ctx))
         (a (hash-ref (subdoc-font-table sd) family))
         (b (hash-ref a size)))
    (when (not b)
      (hash-set! a size (_make-font-set size family #f)))
    (let* ((b (hash-ref a size))
           (c (hash-ref b
                        (cond
                          ((and (font-context-italics ctx) (font-context-bold ctx)) 'bold-italic)
                          ((font-context-bold ctx) 'bold)
                          ((font-context-italics ctx) 'italic)
                          (else 'regular)))))
      c)))

(define (get-text-width/dc-unit doc sd str ctx)
  (let ((f (font-context->font sd ctx))
        (dc (document-session-dc doc)))
    (let-values (((w h d x) (send dc get-text-extent str f)))
      w)))

(define (get-text-height/dc-unit doc sd str ctx)
  (let ((f (font-context->font sd ctx))
        (dc (document-session-dc doc)))
    (let-values (((w h d x) (send dc get-text-extent str f)))
      h)))

(define (should-be-glue? p)
  (require-all char-blank? (string->list p)))
(define (is-newline? p)
  (or (string=? p "\n")
      (string=? p "\r\n")))

(define (subdoc-content-width/dc-unit sd)
  (let ((sd-width (subdoc-width sd))
        (left-margin (subdoc-left-margin sd))
        (right-margin (subdoc-right-margin sd)))
    (- sd-width left-margin right-margin)))

(define (minimum-glue-width doc sd)
  (get-text-width/dc-unit doc sd " " (subdoc-font-context sd)))
(define (line-height doc sd)
  (get-text-height/dc-unit doc sd " " (subdoc-font-context sd)))

(define (register-piece-at/dc-unit sd piece x y)
  (set-graphic-piece-x! piece x)
  (set-graphic-piece-y! piece y)
  (set-typeset-pointer-page-piece-list!
   (subdoc-typeset-pointer sd)
   (cons piece (typeset-pointer-page-piece-list (subdoc-typeset-pointer sd)))))

(define (draw-piece-at/dc-unit dc piece #:y-offset [y-offset 0])
  (send dc set-font (text-piece-font piece))
  (send dc draw-text (text-piece-text piece)
        (get-graphic-piece-x piece) (+ y-offset (get-graphic-piece-y piece))))

(define (subdoc-line-align sd)
  (hash-ref (subdoc-params sd) 'line-align))
(define (subdoc-paragraph-align sd)
  (hash-ref (subdoc-params sd) 'paragraph-align))

(define (write-logic-line line)
  (for ((i (in-list line)))
    (if (text-piece? i)
        #;(display i)
        (display (text-piece-text i))
        (write i))
    (display"|")))

(define (assemble-line doc sd
                       current-y-offset line line-height text-width min-glue max-glue
                       #:glue-mode [glue-mode 'justify]
                       #:left-indent [left-indent 0])
  (write-logic-line line)(displayln "")
  (case glue-mode
    ((justify)
     (let* ((real-content-width (- (subdoc-content-width/dc-unit sd) left-indent))
            (glue-count (for/sum ((i (in-list line)))
                          (if (equal? i 'glue) 1 0)))
            (current-x-offset (+ left-indent (subdoc-left-margin sd)))
            (glue-width (/ (- real-content-width text-width)
                           glue-count))
            (glue-placed #f))
       (for ((tp (in-list line)))
         (if (equal? tp 'glue)
             (if glue-placed
                 (void)
                 (set! current-x-offset (+ current-x-offset glue-width)))
             (begin (when glue-placed (set! glue-placed #f))
                    (register-piece-at/dc-unit sd tp
                                               current-x-offset
                                               (- (+ current-y-offset line-height)
                                                  (text-piece-height tp)))
                    (set! current-x-offset (+ current-x-offset (text-piece-width tp))))))))
  ((left-aligned)
   (let ((current-x-offset (+ left-indent (subdoc-left-margin sd)))
         (glue-placed #f))
     (for ((tp (in-list line)))
       (if (equal? tp 'glue)
           (if glue-placed
               (void)
               (set! current-x-offset (+ current-x-offset (minimum-glue-width doc sd))))
           (begin
             (when glue-placed (set! glue-placed #f))
             (register-piece-at/dc-unit sd tp
                                        current-x-offset 
                                        (- (+ current-y-offset line-height)
                                           (text-piece-height tp)))
             (set! current-x-offset (+ current-x-offset (text-piece-width tp))))))))
  ((right-aligned)
   (let ((current-x-offset (+ left-indent
                              (- (subdoc-width sd)
                                 (subdoc-right-margin sd)
                                 text-width
                                 min-glue)))
         (glue-placed #f))
     (for ((tp (in-list line)))
       (if (equal? tp 'glue)
           (if glue-placed
               (void)
               (set! current-x-offset (+ current-x-offset (minimum-glue-width doc sd))))
           (begin
             (when glue-placed (set! glue-placed #f))
             (register-piece-at/dc-unit sd tp
                                        current-x-offset 
                                        (- (+ current-y-offset line-height)
                                           (text-piece-height tp)))
             (set! current-x-offset (+ current-x-offset (text-piece-width tp))))))))
  ((center)
   (let ((current-x-offset (+ left-indent
                              (/ (- (subdoc-width sd)
                                    (+ min-glue text-width)) 2)))
         (glue-placed #f))
     (for ((tp (in-list line)))
       (if (equal? tp 'glue)
           (if glue-placed
               (void)
               (set! current-x-offset (+ current-x-offset (minimum-glue-width doc sd))))
           (begin
             (when glue-placed (set! glue-placed #f))
             (register-piece-at/dc-unit sd tp
                                        current-x-offset 
                                        (- (+ current-y-offset line-height)
                                           (text-piece-height tp)))
             (set! current-x-offset (+ current-x-offset (text-piece-width tp))))))))))

;; token analysis from flowmark.rkt does not handle empty lines specially so
;; we have this to combine the "newline-emptyline-newline" sequence into a
;; single 'empty-line piece.
(define (combined-source s)
  (define q (make-queue))
  (define last-out (void))
  (define (combiner p)
    (cond
      ((or (equal? p #f) (eof-object? p))
       (enqueue! q p))
      ((equal? p 'skip)
       (void))
      ((equal? p 'paragraph-break)
       (enqueue! q p))
      ((command-piece? p)
       (for ((i (in-list (evaluate-command-piece p))))
         (enqueue! q i)))
      ((freeform-piece? p)
       (for ((i (in-list (evaluate-freeform-piece p))))
         (enqueue! q i)))
      (else
       (enqueue! q p))))
  (λ (cmd)
    (case cmd
      ((is-empty) (equal? last-out #f))
      ((get)
       (cond ((not (queue-empty? q))
              (let ((r (dequeue! q)))
                (set! last-out r)
                r))
             ((equal? last-out #f)
              #f)
             (else
              (do ()
                  ((or (not (queue-empty? q))
                       (equal? last-out #f))
                   (if (equal? last-out #f)
                       #f
                       (let ((r (dequeue! q)))
                         (set! last-out r)
                         r)))
                (combiner (next-piece s)))))))))

(define (get-subdoc-y-offset sd)
  (typeset-pointer-y-offset (subdoc-typeset-pointer sd)))
(define (set-subdoc-y-offset! sd x)
  (set-typeset-pointer-y-offset! (subdoc-typeset-pointer sd) x))
(define (get-subdoc-current-height sd)
  (typeset-pointer-current-height (subdoc-typeset-pointer sd)))
(define (set-subdoc-current-height! sd x)
  (set-typeset-pointer-current-height! (subdoc-typeset-pointer sd) x))
(define (get-subdoc-text-width sd)
  (typeset-pointer-text-width (subdoc-typeset-pointer sd)))
(define (set-subdoc-text-width! sd x)
  (set-typeset-pointer-text-width! (subdoc-typeset-pointer sd) x))
(define (get-subdoc-min-glue-width sd)
  (typeset-pointer-min-glue-width (subdoc-typeset-pointer sd)))
(define (set-subdoc-min-glue-width! sd x)
  (set-typeset-pointer-min-glue-width! (subdoc-typeset-pointer sd) x))
;; NOTE: max-glue-width is not currently used but it's intended to be used to warn user of
;;       hbox underfill in the future.
(define (get-subdoc-max-glue-width sd)
  (typeset-pointer-max-glue-width (subdoc-typeset-pointer sd)))
(define (set-subdoc-max-glue-width! sd x)
  (set-typeset-pointer-max-glue-width! (subdoc-typeset-pointer sd) x))
(define (get-subdoc-line-height sd)
  (typeset-pointer-line-height (subdoc-typeset-pointer sd)))
(define (set-subdoc-line-height! sd x)
  (set-typeset-pointer-line-height! (subdoc-typeset-pointer sd) x))
(define (get-subdoc-line-list sd)
  (typeset-pointer-line-list (subdoc-typeset-pointer sd)))
(define (push-word-to-line! doc sd wp)
  (let* ((ctx (clone-font-context (subdoc-font-context sd)))
         (font (font-context->font sd ctx))
         (str (word-piece-word wp))
         (word-w (get-text-width/dc-unit doc sd str ctx))
         (word-h (get-text-height/dc-unit doc sd str ctx)))
    (set-typeset-pointer-line-list!
     (subdoc-typeset-pointer sd)
     (cons (text-piece str word-w word-h #f #f font)
           (get-subdoc-line-list sd)))
    (set-subdoc-text-width! sd (+ (get-subdoc-text-width sd) word-w))
    (set-subdoc-min-glue-width! sd (+ (get-subdoc-min-glue-width sd) (minimum-glue-width doc sd)))
    (set-subdoc-max-glue-width! sd (+ (get-subdoc-max-glue-width sd) (* 2 (minimum-glue-width doc sd))))
    (let ((old-line-height (get-subdoc-line-height sd)))
      (set-subdoc-line-height! sd (max old-line-height word-h))
      (set-subdoc-current-height! sd (+ (- (get-subdoc-current-height sd) old-line-height)
                                        (max old-line-height word-h))))))
(define (push-glue-to-line! sd)
  (set-typeset-pointer-line-list!
   (subdoc-typeset-pointer sd)
   (cons 'glue (get-subdoc-line-list sd))))
(define (clear-subdoc-line-list! sd)
  (set-typeset-pointer-line-list! (subdoc-typeset-pointer sd) (list)))
(define (get-subdoc-first-line-left-indent sd)
  (hash-ref (subdoc-params sd) 'first-line-left-indent (get-subdoc-left-indent sd)))
(define (get-subdoc-left-indent sd)
  (hash-ref (subdoc-params sd) 'left-indent 0))
(define (get-subdoc-paragraph-skip sd)
  (hash-ref (subdoc-params sd) 'paragraph-skip 0))
(define (get-subdoc-paragraph-align sd)
  (hash-ref (subdoc-params sd) 'paragraph-align 'left-aligned))
(define (get-subdoc-line-align sd)
  (hash-ref (subdoc-params sd) 'line-align 'justify))
(define (_preprocess-line line)
  (define (_preprocess-line_ l x glue-taken)
    (cond
      ((null? l) x)
      ((equal? (car l) 'glue)
       (if glue-taken
           (_preprocess-line_ (cdr l) x glue-taken)
           (_preprocess-line_ (cdr l) (cons (car l) x) #t)))
      (else
       (_preprocess-line_ (cdr l) (cons (car l) x) #f))))
  (_preprocess-line_ line (list) #f))
(define (flush-line! doc sd glue-mode [first-line? #f])
  (assemble-line doc sd
                 (get-subdoc-y-offset sd)
                 (_preprocess-line (dropf (get-subdoc-line-list sd) (λ (x) (equal? x 'glue))))
                 (get-subdoc-line-height sd)
                 (get-subdoc-text-width sd)
                 (get-subdoc-min-glue-width sd)
                 (get-subdoc-max-glue-width sd)
                 #:glue-mode glue-mode
                 #:left-indent (if first-line?
                                   (get-subdoc-first-line-left-indent sd)
                                   (get-subdoc-left-indent sd)))
  (set-subdoc-text-width! sd 0)
  (set-subdoc-min-glue-width! sd 0)  ;; NOTE: subdoc is vertical composition.
  (set-subdoc-max-glue-width! sd 0)
  (clear-subdoc-line-list! sd)
  (set-subdoc-y-offset! sd (+ (get-subdoc-y-offset sd) (get-subdoc-line-height sd)))
  (set-subdoc-line-height! sd 0))
(define (horizontal-overflow sd [with 0])
  (> (+ (get-subdoc-text-width sd) (get-subdoc-min-glue-width sd) with)
               (subdoc-content-width/dc-unit sd)))
(define (vertical-overflow doc [with 0])
  (let ((page-height (cdr (document-session-global-paper-size doc))))
    (> (+ (for/sum ((i (document-session-subdoc-index-list doc)))
             (if (vertical-spacer? i)
                 (if (equal? (vertical-spacer-height i) #f)
                     0
                     (vertical-spacer-height i))
                 (let ((sd (get-subdoc-by-id doc i)))
                   (if (equal? (subdoc-height sd) #f)
                       (typeset-pointer-current-height (subdoc-typeset-pointer sd))
                       (subdoc-height sd)))))
           with)
        page-height)))
(define (get-subdoc-by-id doc i)
  (hash-ref (document-session-subdoc-list doc) i))
(define (clear-subdoc-page-piece-list! sd)
  (set-typeset-pointer-page-piece-list! (subdoc-typeset-pointer sd) (list)))
(define (flush-page! doc)
  (let ((dc (document-session-dc doc))
        (current-y-offset 0)
        (sdlist (document-session-subdoc-list doc))
        (p (send (document-session-dc doc) get-pen))
        (total-height (cdr (document-session-global-paper-size doc)))
        (occupied-height 0)
        (expanding-spacer-count 0)
        (expanding-spacer-height 0))
    (for ((sdi (in-list (document-session-subdoc-index-list doc))))
      (if (vertical-spacer? sdi)
          (if (equal? (vertical-spacer-height sdi) #f)
              (begin (set! expanding-spacer-count (+ expanding-spacer-count 1))
                     0)
              (set! occupied-height (+ occupied-height (vertical-spacer-height sdi))))
          (set! occupied-height (+ occupied-height
                                   (let ((sd (get-subdoc-by-id doc sdi)))
                                     (if (equal? (subdoc-height sd) #f)
                                         (typeset-pointer-current-height
                                          (subdoc-typeset-pointer sd))
                                         (subdoc-height sd)))))))
    (set! expanding-spacer-height
          (if (<= expanding-spacer-count 0)
              0
              (/ (- total-height occupied-height) expanding-spacer-count)))
    (for ((sdi (in-list (document-session-subdoc-index-list doc))))
      (if (vertical-spacer? sdi)
          (set! current-y-offset (+ current-y-offset
                                    (if (equal? (vertical-spacer-height sdi) #f)
                                        expanding-spacer-height
                                        (vertical-spacer-height sdi))))
          (let ((sd (get-subdoc-by-id doc sdi)))
            (send dc set-pen *dotted-pen*)
            (send (document-session-dc doc) draw-rectangle
                  (subdoc-left-margin sd) current-y-offset
                  (subdoc-content-width/dc-unit sd)
                  (if (not (equal? (subdoc-height sd) #f))
                      (subdoc-height sd)
                      (typeset-pointer-current-height
                       (subdoc-typeset-pointer sd))))
            (send dc set-pen p)
            (for ((i (in-list (typeset-pointer-page-piece-list (subdoc-typeset-pointer sd)))))
              (draw-piece-at/dc-unit dc i #:y-offset current-y-offset))
            (set! current-y-offset
                  (+ current-y-offset
                     (if (equal? (subdoc-height sd) #f)
                         (typeset-pointer-current-height (subdoc-typeset-pointer sd))
                         (subdoc-height sd)))))))))

(define (next-page doc)
  (flush-page! doc)
  (send* (document-session-dc doc)
    (end-page)
    (start-page))
  (let ((sdlist (document-session-subdoc-list doc)))
    (for ((sdi (in-list (document-session-subdoc-index-list doc))))
      (unless (vertical-spacer? sdi)
        (let ((sd (get-subdoc-by-id doc sdi)))
          (set-subdoc-current-height! sd 0)
          (set-subdoc-y-offset! sd 0)
          (clear-subdoc-page-piece-list! sd))))))

(define (is-subdoc-at-first-paragraph? sd)
  (typeset-pointer-first-paragraph? (subdoc-typeset-pointer sd)))
(define (is-subdoc-at-first-line? sd)
  (typeset-pointer-first-line? (subdoc-typeset-pointer sd)))
(define (set-subdoc-at-first-paragraph! sd x)
  (set-typeset-pointer-first-paragraph?! (subdoc-typeset-pointer sd) x))
(define (set-subdoc-at-first-line! sd x)
  (set-typeset-pointer-first-line?! (subdoc-typeset-pointer sd) x))

(define (new-paragraph doc sd)
  (when (vertical-overflow doc) (next-page doc))
  (set-subdoc-y-offset! sd (+ (get-subdoc-y-offset sd)
                              (get-subdoc-line-height sd)
                              (get-subdoc-paragraph-skip sd)))
  (set-subdoc-current-height! sd (+ (get-subdoc-current-height sd)
                                    (get-subdoc-line-height sd)
                                    (get-subdoc-paragraph-skip sd)))
  (when (is-subdoc-at-first-paragraph? sd)
    (set-subdoc-at-first-paragraph! sd #f))
  (set-subdoc-at-first-line! sd #t))
(define (line-left-indent sd)
  (if (is-subdoc-at-first-paragraph? sd)
      (get-subdoc-left-indent sd)
      (if (is-subdoc-at-first-line? sd)
          (get-subdoc-first-line-left-indent sd)
          (get-subdoc-left-indent sd))))
(define (should-line-indent? sd)
  (if (is-subdoc-at-first-paragraph? sd)
      #f
      (is-subdoc-at-first-line? sd)))
(define (process-piece doc sd p)
  (cond
    ((equal? p 'paragraph-break)
     (when (vertical-overflow doc) (next-page doc))
     (flush-line! doc sd
                  (get-subdoc-paragraph-align sd)
                  (should-line-indent? sd))
     (new-paragraph doc sd)
     (clear-subdoc-line-list! sd))
    ((or (equal? p #f) (eof-object? p))   ;; typeset the last line.
     (when (vertical-overflow doc) (next-page doc))
     (flush-line! doc sd
                  (get-subdoc-paragraph-align sd)
                  (should-line-indent? sd)))
    ((should-be-glue? (word-piece-word p))
     ;; (when (vertical-overflow doc) (next-page doc))
     (push-glue-to-line! sd))
    (else
     (let ((w (get-text-width/dc-unit doc sd (word-piece-word p) (subdoc-font-context sd)))
           (h (get-text-height/dc-unit doc sd (word-piece-word p) (subdoc-font-context sd))))
       (when (vertical-overflow doc h) (next-page doc))
       (when (horizontal-overflow sd (+ w (line-left-indent sd)))
         (flush-line! doc sd
                      (get-subdoc-line-align sd)
                      (should-line-indent? sd))
         (when (is-subdoc-at-first-line? sd)
           (set-subdoc-at-first-line! sd #f)))
       (push-word-to-line! doc sd p)))))
(define (run-source doc s)
  (do () ((s 'is-empty))
    (let ((p (s 'get)))
      (process-piece doc (get-current-subdoc doc) p))))

(define (make-session input-file-path [output-file-path #f])
  (let ((real-output-path (if (not output-file-path)
                              (string-append input-file-path ".pdf")
                              output-file-path)))
    (let* ((tp (typeset-pointer 0 0 0 0 0 1 (list) (list) #t #t))
           (size (paper-class->paper-size *default-paper-class*))
           (ctx (font-context #f #f 12 'roman))
           (font-family-to-face (make-default-font-family-to-face))
           (font-table (generate-font-table font-family-to-face))
           (params (make-hash
                    `((line-align . justify)
                      (paragraph-align . left-aligned)
                      (left-indent . 0)
                      (first-line-left-indent . 36)
                      (font-size . 12)
                      (footnote-font-size . 9))))
           (header (subdoc (car size) #f
                           (mm->vertical-dc-unit 25.4)
                           (mm->vertical-dc-unit 25.4)
                           font-family-to-face
                           font-table
                           (font-context #f #f 10 'roman)
                           (make-hash
                            `((line-align . left-aligned)
                              (paragraph-align . left-aligned)
                              (left-indent . 0)
                              (first-line-left-indent . 36)
                              (font-size . 10)
                              (footnote-font-size . 9)))
                           (typeset-pointer 0 0 0 0 0 0 (list) (list) #t #t)
                           #f))
           (main (subdoc (car size) #f
                         (mm->horizontal-dc-unit 25.4)
                         (mm->horizontal-dc-unit 25.4)
                         font-family-to-face
                         font-table
                         ctx
                         params
                         tp
                         #f))
           (footnote (subdoc (car size) #f
                             (mm->horizontal-dc-unit 25.4)
                             (mm->horizontal-dc-unit 25.4)
                             font-family-to-face
                             font-table
                             (font-context #f #f 9 'roman)
                             params
                             (typeset-pointer 0 0 0 0 0 0 (list) (list) #t #t)
                             #t))
           (footer (subdoc (car size) (inch->vertical-dc-unit 0.25)
                           (mm->vertical-dc-unit 25.4)
                           (mm->vertical-dc-unit 25.4)
                           font-family-to-face
                           font-table
                           (font-context #f #f 10 'roman)
                           (make-hash
                            `((line-align . left-aligned)
                              (paragraph-align . left-aligned)
                              (left-indent . 0)
                              (first-line-left-indent . 36)
                              (font-size . 10)
                              (footnote-font-size . 9)))
                           (typeset-pointer 0 0 0 0 0 0 (list) (list) #t #t)
                           #f))
           (output-file (begin (when (file-exists? real-output-path)
                                 (delete-file real-output-path))
                               (open-output-file real-output-path)))
           (dc (new pdf-dc%
                  (interactive #f)
                  (width (car size))
                  (height (cdr size))
                  (as-eps #f)
                  (output output-file))))
      (document-session (make-hash `((header . ,header)
                                     (main . ,main)
                                     (footnote . ,footnote)
                                     (footer . ,footer)))
                        (list (vertical-spacer (inch->vertical-dc-unit 1))
                              'header
                              (vertical-spacer (inch->vertical-dc-unit 0.25))
                              'main
                              (vertical-spacer #f)
                              ;; (vertical-spacer (inch->vertical-dc-unit 0.25))
                              'footnote
                              ;; (vertical-spacer (inch->vertical-dc-unit 0.25))
                              ;; 'footer
                              (vertical-spacer (inch->vertical-dc-unit 1.5)))
                        size
                        output-file
                        dc
                        'main))))

(define *current-session* (void))
(define (process-file file-path [target-path #f])
  (let ((real-target-path (if (not target-path)
                              (string-append file-path ".pdf")
                              target-path))
        (source-input-port (open-input-file file-path)))
    (let ((s (make-session file-path target-path)))
      (set! *current-session* s)
      (send* (document-session-dc s)
        (start-doc file-path)
        (start-page))
      (run-source s
                  (combined-source (source-port source-input-port file-path 0 0)))
      (for ((sdi (document-session-subdoc-index-list s)))
        (when (not (vertical-spacer? sdi))
          (let ((sd (get-subdoc-by-id s sdi)))
            (let ((tp (subdoc-typeset-pointer sd)))
              (when (not (null?  (typeset-pointer-line-list tp)))
                (flush-line! s sd (subdoc-paragraph-align sd) (should-line-indent? sd)))))))
      (flush-page! s)
      #;(for ((sd (in-list (document-session-subdoc-list s))))
        (let ((ppl (typeset-pointer-page-piece-list (subdoc-typeset-pointer sd)))
              (dc (document-session-dc s)))
          (when (>= (length ppl) 0)
            (for ((i (in-list ppl)))
              (draw-piece-at/dc-unit dc i)))))
      (close-input-port source-input-port)
      (send* (document-session-dc s)
        (end-page)
        (end-doc))
      (close-output-port (document-session-output-port s)))))

(define (get-current-subdoc doc)
  (hash-ref (document-session-subdoc-list doc)
            (document-session-current-subdoc-index doc)))
(define (*current-subdoc*) (get-current-subdoc *current-session*))

(register-fm-primitive
 "bold"
 (λ (p)
   (set-font-context-bold! (subdoc-font-context (*current-subdoc*)) #t)
   (append (append* (command-piece-args p)) (list (command-piece "toggle.bold" (list) -1 -1)))))

(register-fm-primitive
 "it"
 (λ (p)
   (set-font-context-italics! (subdoc-font-context (*current-subdoc*)) #t)
   (append (append* (command-piece-args p)) (list (command-piece "toggle.italics" (list) -1 -1)))))

(register-fm-primitive
 "toggle.bold"
 (λ (p)
   (set-font-context-bold! (subdoc-font-context (*current-subdoc*))
                           (not (font-context-bold (subdoc-font-context (*current-subdoc*)))))))

(register-fm-primitive
 "toggle.italics"
 (λ (p)
   (set-font-context-italics! (subdoc-font-context (*current-subdoc*))
                           (not (font-context-italics (subdoc-font-context (*current-subdoc*)))))))

(register-fm-primitive
 "set.font_size"
 (λ (p)
   (set-font-context-size! (subdoc-font-context (*current-subdoc*))
                           (string->number (string-trim (evaluate-logic-document-to-string
                                                         (command-arg-ref p 0)))))))

(register-fm-primitive
 "flush.line"
 (λ (p)
   (flush-line! *current-session* (*current-subdoc*)
                (get-subdoc-paragraph-align (*current-subdoc*)))))
(register-fm-freeform-primitive "$" "\\flush.line")

(register-fm-primitive
 "new.page"
 (λ (p)
   (flush-line! *current-session* (*current-subdoc*)
                (get-subdoc-paragraph-align (*current-subdoc*)))
   (next-page *current-session*)))

(register-fm-primitive
 "vskip"
 (λ (p)
   (let ((sd (*current-subdoc*))
         (h (command-arg-as-number p 0)))
     (set-subdoc-y-offset! sd (+ (get-subdoc-y-offset sd) h))
     (set-subdoc-current-height! sd (+ (get-subdoc-current-height sd) h)))))

(register-fm-primitive
 "set.first_line_left_indent"
 (λ (p)
   (hash-set! (subdoc-params (*current-subdoc*)) 'first-line-left-indent
              (command-arg-as-number p 0))))
(register-fm-primitive
 "set.left_indent"
 (λ (p)
   (hash-set! (subdoc-params (*current-subdoc*)) 'left-indent
              (command-arg-as-number p 0))))
(register-fm-primitive
 "set.current_subdoc"
 (λ (p)
   (set-document-session-current-subdoc-index!
    *current-session*
    (string->symbol (string-trim (evaluate-logic-document-to-string (command-arg-ref p 0)))))))


