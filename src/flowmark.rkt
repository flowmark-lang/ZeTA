#lang typed/racket/base
(require racket/string)
(require (only-in racket/list
                  append*))
(provide (all-defined-out))

(struct word-piece ([word : String])
  #:transparent
  #:mutable
  #:type-name WordPiece)
(struct command-piece ([head : String]
                       [args : (Listof LogicDocument)]
                       [line : Integer]
                       [col : Integer])
  #:transparent
  #:mutable
  #:type-name CommandPiece)
(struct freeform-piece ([pattern : String]
                        [line : Integer]
                        [col : Integer])
  #:transparent
  #:mutable
  #:type-name FreeformPiece)
(define-type LogicPiece (U WordPiece CommandPiece FreeformPiece 'skip 'paragraph-break))
(define-type LogicDocument (Listof LogicPiece))

(struct source-string ([source : String]
                       [origin : String]
                       [pointer : Integer]
                       [source-length : Integer]
                       [line : Integer]
                       [col : Integer])
  #:transparent
  #:mutable
  #:type-name SourceString)
(struct source-port ([source : Input-Port]
                     [origin : String]
                     [line : Integer]
                     [col : Integer])
  #:transparent
  #:mutable
  #:type-name SourcePort)
(define-type Source (U SourceString SourcePort))

(: string->source-string (-> String SourceString))
(define (string->source-string s)
  (source-string s "" 0 (string-length s) 0 0))

(: set-source-line! (-> Source Integer Void))
(define (set-source-line! s i)
  (cond ((source-string? s) (set-source-string-line! s i))
        ((source-port? s) (set-source-port-line! s i))))

(: set-source-col! (-> Source Integer Void))
(define (set-source-col! s i)
  (cond ((source-string? s) (set-source-string-col! s i))
        ((source-port? s) (set-source-port-col! s i))))

(: source-line (-> Source Integer))
(define (source-line s)
  (cond ((source-string? s) (source-string-line s))
        ((source-port? s) (source-port-line s))))

(: source-col (-> Source Integer))
(define (source-col s)
  (cond ((source-string? s) (source-string-col s))
        ((source-port? s) (source-port-col s))))

(struct fm-error ([line : Integer]
                  [col : Integer]
                  [reason : String])
  #:transparent
  #:type-name FMError)
(define error-list : (Listof FMError) (list))
(: register-error (-> Integer Integer String Void))
(define (register-error line col reason)
  (set! error-list (cons (fm-error line col reason) error-list)))
(: report-error (-> Void))
(define (report-error)
  (for ((i (in-list (reverse error-list))))
    (displayln (format "(~a, ~a) : ~a"
                       (fm-error-line i) (fm-error-col i) (fm-error-reason i))
               (current-error-port))))
                       

(: source-peek-char (-> Source (U Char False)))
(define (source-peek-char s)
  (cond
    ((source-string? s)
     (if (>= (source-string-pointer s) (source-string-source-length s))
         #f
         (string-ref (source-string-source s) (source-string-pointer s))))
    ((source-port? s)
     (let ((ch (peek-char (source-port-source s))))
       (if (eof-object? ch)
           #f
           ch)))))

(: source-read-char (-> Source (U Char False)))
(define (source-read-char s)
  (cond
    ((source-string? s)
     (if (>= (source-string-pointer s) (source-string-source-length s))
         #f
         (let ((ch (string-ref (source-string-source s) (source-string-pointer s))))
           (set-source-string-pointer! s (+ (source-string-pointer s) 1))
           ch)))
    ((source-port? s)
     (let ((ch (read-char (source-port-source s))))
       (if (eof-object? ch)
           #f
           ch)))))

(: source-dispose-char (-> Source Void))
(define (source-dispose-char s)
  (let ((c (source-read-char s)))
    (when (not (equal? c #f))
      (update-source-location-by-char! s c)))
  (void))

(: update-source-location-by-char! (-> Source Char Void))
(define (update-source-location-by-char! s ch)
  (case ch
    ((#\newline) (set-source-line! s (+ (source-line s) 1))
                 (set-source-col! s 0))
    (else (set-source-col! s (+ (source-col s) 1)))))

(: source-take-while! (-> Source (-> Char Boolean) String))
(define (source-take-while! s f)
  (let ((r : (Listof Char) (list)))
    (do : String
      ((ch (source-peek-char s) (source-peek-char s)))
      ((or (equal? ch #f) (not (f ch)))
       (list->string (reverse r)))
      (set! r (cons ch r))
      (source-dispose-char s))))

(: valid-command-first-char (-> (U Char False) Boolean))
(define (valid-command-first-char ch)
  (if (equal? ch #f)
      #f
      (or (and (char>=? ch #\a) (char<=? ch #\z))
          (and (char>=? ch #\A) (char<=? ch #\Z))
          (char=? ch #\.)
          (char=? ch #\_))))
(: valid-command-char (-> Char Boolean))
(define (valid-command-char ch)
  (or (valid-command-first-char ch)
      (and (char>=? ch #\0) (char<=? ch #\9))))

(: *quote-start* Char)
(define *quote-start* #\{)
(: *quote-end* Char)
(define *quote-end* #\})

(: next-quoted-pieces (-> Source (Listof LogicPiece)))
(define (next-quoted-pieces s)
  (let ((r : (Listof LogicPiece) (list)))
    (do : Void
      ((p (next-piece s) (next-piece s)))
      ((let ((ch (source-peek-char s)))
         (or (equal? ch #f) (char=? ch *quote-end*)))
       (unless (equal? p #f)
         (set! r (cons p r))))
      (unless (not p)
        (set! r (cons p r))))
    (when (not (equal? (source-peek-char s) #f))
      (source-dispose-char s))
    (reverse r)))

(: next-arg-pieces (->* (Source) ((Listof Char)) (Values Boolean LogicDocument)))
(define (next-arg-pieces s [limiting-char (list)])
  (let ((r : (Listof LogicPiece) (list)))
    (do : Void
      ((p (next-piece s limiting-char) (next-piece s limiting-char)))
      ((or (equal? p #f)
           (let ((ch (source-peek-char s)))
             (or (equal? ch #f) (char=? ch #\,))))
       (unless (equal? p #f)
         (set! r (cons p r))))
      (unless (not p)
        (set! r (cons p r))))
    (values (let ((ch (source-peek-char s)))
              (and ch (char=? #\, ch)))
            (reverse r))))

(: next-arglist-pieces (-> Source (Listof LogicDocument)))
(define (next-arglist-pieces s)
  (let ((r : (Listof LogicDocument) (list))
        (should-end : Boolean #f))
    (do : Void
      ()
      (should-end)
      (let-values (((se arg) (next-arg-pieces s (list #\( #\)))))
        (set! should-end (not se))
        (set! r (cons arg r))
        (when (and should-end (not (equal? (source-peek-char s) #\))))
          (register-error (source-line s) (source-col s)
                          "Invalid call syntax; right parenthesis required"))
        (source-dispose-char s)))
    (reverse r)))

(: next-piece (->* (Source) ((Listof Char)) (U LogicPiece False)))
(define (next-piece s [limiting-char (list)])
  (let ((line (source-line s))
        (col (source-col s))
        (ch (source-peek-char s)))
    (case ch
      ((#\{)
       (begin
         (source-dispose-char s)
         (let* ((cnt : Integer 0)
                (prev : Integer 0)
                (r (source-take-while! s (λ (ch)
                                          (case ch
                                            ((#\{)
                                             (set! cnt (+ cnt 1 prev))
                                             (set! prev 0))
                                            ((#\})
                                             (set! cnt (- cnt 1 prev))
                                             (set! prev 0))
                                            ((#\@)
                                             (set! prev -1))
                                            (else
                                             (set! prev 0)))
                                           (>= cnt 0)))))
           (let ((ch (source-peek-char s)))
             (when (and ch (char=? ch #\}))
               (source-dispose-char s)))
           (word-piece r))))
      ((#\\)
       (source-dispose-char s)
       (let ((ch (source-peek-char s)))
         (cond
           ((equal? ch #f) #f)
           ((or (equal? ch #\space)
                (equal? ch #\tab)
                (equal? ch #\newline)
                (equal? ch #\return))
            (let ((r (source-take-while! s (λ (ch) (or (char=? ch #\space)
                                                       (char=? ch #\tab)
                                                       (char=? ch #\newline)
                                                       (char=? ch #\return))))))
              'skip))
           ((and (not (equal? ch #f)) (not (valid-command-first-char ch)))
            (register-error (source-line s) (source-col s) "Invalid command")
            #f)
           (else
            (let* ((r (source-take-while! s valid-command-char))
                   (ch (source-peek-char s))
                   (arglist (case ch
                              ((#\()
                               (source-dispose-char s)
                               (next-arglist-pieces s))
                              (else
                               (list)))))
              (command-piece r arglist line col))))))
      ((#\space #\tab)
       (let ((r (source-take-while! s (λ (ch) (or (char=? ch #\space)
                                                  (char=? ch #\tab))))))
         (word-piece r)))
      ((#\@)
       (source-dispose-char s)
       (let ((ch (source-peek-char s)))
         (if (equal? ch #f)
             (begin (register-error (source-line s) (source-col s) "Invalid escape sequence") #f)
             (begin (source-dispose-char s)
                    (word-piece (make-string 1 ch))))))
      ((#\newline)
       (source-dispose-char s)
       (let ((r (source-take-while! s (λ (ch) (or (char=? ch #\space)
                                                  (char=? ch #\tab)
                                                  (char=? ch #\return)
                                                  (char=? ch #\newline))))))
         (if (string-contains? r "\n")
             'paragraph-break
             (word-piece r))))
      ((#\()
       (source-dispose-char s)
       (word-piece (make-string 1 #\()))
      ((#\,)
       (source-dispose-char s)
       (word-piece (make-string 1 #\,)))
      ((#\# #\~ #\` #\$ #\% #\^ #\& #\_)
       (let ((r (source-take-while! s (λ (ch) (if (member ch (list #\# #\~ #\` #\$ #\% #\^ #\& #\_)) #t #f)))))
         (freeform-piece r line col)))
      ((#\})
       (source-dispose-char s)
       (word-piece (make-string 1 #\})))
      (else
       (let ((r (source-take-while! s (λ (ch) (if (or (member ch (list #\space
                                                                       #\tab
                                                                       #\newline
                                                                       #\\
                                                                       #\@
                                                                       #\,
                                                                       #\# #\~ #\` #\$ #\%
                                                                       #\^ #\& #\_
                                                                       *quote-start*
                                                                       *quote-end*))
                                                      (member ch limiting-char))
                                                  #f #t)))))
         (if (<= (string-length r) 0)
             #f
             (word-piece r)))))))

(define-type LogicPieceHandler (-> (U LogicPiece False) Void))

(: run-source-with-handler (-> Source LogicPieceHandler Void))
(define (run-source-with-handler s h)
  (do : Void
    ((p (next-piece s) (next-piece s)))
    ((equal? p #f) (h p))
    (h p)))

(: run-text-with-handler (-> String LogicPieceHandler Void))
(define (run-text-with-handler str h)
  (run-source-with-handler (string->source-string str) h))

(: run-logic-pieces-with-handler (-> (Listof LogicPiece) LogicPieceHandler Void))
(define (run-logic-pieces-with-handler l h)
  (map h l)
  (void))

(: parse-text (-> String LogicDocument))
(define (parse-text s)
  (let ((r : LogicDocument (list)))
    (run-text-with-handler s (λ (p) (when (not (equal? p #f)) (set! r (cons p r)))))
    (reverse r)))

(struct fm-macro-text-piece ([text : String])
  #:transparent
  #:mutable
  #:type-name FMMacroTextPiece)
(define-type FMMacroPiece (U FMMacroTextPiece Integer))
(struct fm-macro ([piece-list : (Listof FMMacroPiece)])
  #:transparent
  #:mutable
  #:type-name FMMacro)
(define-type FMPrimitiveFType (-> CommandPiece (U Void String LogicDocument)))
(struct fm-primitive ([f : FMPrimitiveFType])
  #:transparent
  #:mutable
  #:type-name FMPrimitive)
(define-type FMCallable (U FMMacro FMPrimitive))

(define fm-env : (HashTable String FMCallable) (make-hash))
(define fm-freeform-env : (HashTable String String) (make-hash))

(: register-fm-primitive (-> String FMPrimitiveFType Void))
(define (register-fm-primitive name p)
  (hash-set! fm-env name (fm-primitive p)))

(: register-fm-freeform-primitive (-> String String Void))
(define (register-fm-freeform-primitive name p)
  (hash-set! fm-freeform-env name p))

(: evaluate-command-piece (-> CommandPiece LogicDocument))
(define (evaluate-command-piece p)
  (let ((r (hash-ref fm-env (command-piece-head p) #f)))
    (cond
      ((not r)
       (register-error (command-piece-line p) (command-piece-col p)
                         (format "Cannot find command: ~a" (command-piece-head p)))
       (for ((i : LogicDocument (in-list (command-piece-args p))))
         (for ((j : LogicPiece (in-list i)))
           (when (command-piece? j)
             (evaluate-command-piece j))))
       (list))
      ((fm-primitive? r)
       (let ((r ((fm-primitive-f r) p)))
         (cond ((void? r) (list))
               ((string? r) (list (word-piece r)))
               (else r))))
      ((fm-macro? r)
       (append* (ann (for/list ([i : FMMacroPiece (in-list (fm-macro-piece-list r))])
         (cond
           ((fm-macro-text-piece? i)
            (list (word-piece (fm-macro-text-piece-text i))))
           ((integer? i)
            (list-ref (command-piece-args p) (- i 1))))) (Listof LogicDocument)))))))

(: evaluate-freeform-piece (-> FreeformPiece LogicDocument))
(define (evaluate-freeform-piece p)
  (let ((r (hash-ref fm-freeform-env (freeform-piece-pattern p) #f)))
    (cond
      ((not r)
       (register-error (freeform-piece-line p) (freeform-piece-col p)
                         (format "Cannot find freeform macro pattern: ~a"
                                 (freeform-piece-pattern p)))
       (list))
      (else
       (evaluate-logic-document (parse-text r))))))

(: evaluate-logic-piece (-> LogicPiece LogicDocument))
(define (evaluate-logic-piece x)
  (cond ((command-piece? x) (evaluate-command-piece x))
        ((word-piece? x) (list x))
        ((freeform-piece? x) (evaluate-freeform-piece x))
        ((equal? x 'skip) (list))
        ((equal? x 'paragraph-break) (list 'paragraph-break))))

(: evaluate-logic-piece-to-string (-> LogicPiece String))
(define (evaluate-logic-piece-to-string x)
  (let ((v (evaluate-logic-piece x)))
    (string-join (map (λ (p) (cond ((word-piece? p) (word-piece-word p))
                                   (else (error "Invalid value"))))
                      v)
                 "")))

(: evaluate-logic-document (-> LogicDocument LogicDocument))
(define (evaluate-logic-document ld)
  (append* (ann (for/list ([i : LogicPiece (in-list ld)])
                  (evaluate-logic-piece i))
                (Listof LogicDocument))))

(: evaluate-logic-document-to-string (-> LogicDocument String))
(define (evaluate-logic-document-to-string ld)
  (string-join (ann (for/list ([i : LogicPiece (in-list ld)])
                      (evaluate-logic-piece-to-string i))
                    (Listof String))
               ""))

(: evaluate-text-to-string (-> String String))
(define (evaluate-text-to-string s)
  (evaluate-logic-document-to-string (parse-text s)))

(: prim/arity-guard (-> (-> Integer Boolean) FMPrimitiveFType FMPrimitiveFType))
(define (prim/arity-guard ig f)
  (λ (p)
    (if (not (ig (length (command-piece-args p))))
        (begin (register-error (command-piece-line p) (command-piece-col p)
                                 "Arity mismatch")
               (void))
        (f p))))

(: command-arg-ref (->* (CommandPiece Integer) (LogicDocument) LogicDocument))
(define (command-arg-ref p i [fallback '()])
  (: command-arg-ref_ (-> (Listof LogicDocument) Integer LogicDocument))
  (define (command-arg-ref_ k i)
    (cond
      ((null? k) fallback)
      ((= i 0) (car k))
      (else (command-arg-ref_ (cdr k) (- i 1)))))
  (command-arg-ref_ (command-piece-args p) i))

(register-fm-primitive
 "def"
 (prim/arity-guard
  (λ (a) (>= a 1))
  (λ (p)
    (let ((name (string-trim (evaluate-logic-document-to-string (command-arg-ref p 0))))
          (body (evaluate-logic-document-to-string (command-arg-ref p 1))))
      (hash-set! fm-env name (fm-macro (list (fm-macro-text-piece body))))))))

(register-fm-primitive
 "call"
 (prim/arity-guard
  (λ (a) (>= a 1))
  (λ (p)
    (let* ((name (string-trim (evaluate-logic-document-to-string (command-arg-ref p 0))))
           (macro (hash-ref fm-env name #f)))
      (if (not macro)
          (register-error (command-piece-line p) (command-piece-col p)
                                 (format "Unknown definition: ~a" name))
          (cond
            ((fm-primitive? macro) ((fm-primitive-f macro) (command-piece
                                                            name
                                                            (cdr (command-piece-args p))
                                                            (command-piece-line p)
                                                            (command-piece-col p))))
            ((fm-macro? macro)
             (list
              (word-piece
               (evaluate-text-to-string
                (string-join
                 (ann
                  (for/list ((macro-p : FMMacroPiece (in-list (fm-macro-piece-list macro))))
                    (cond
                      ((fm-macro-text-piece? macro-p)
                       (fm-macro-text-piece-text macro-p))
                      ((integer? macro-p)
                       (evaluate-logic-document-to-string (command-arg-ref p macro-p)))))
                  (Listof String))
                 "")))))))))))

(: command-arg-as-number (-> CommandPiece Integer Number))
(define (command-arg-as-number p i)
  (or
   (string->number (string-trim (evaluate-logic-document-to-string
                                 (command-arg-ref p i))))
   0))
(: string-as-command-arg (-> String LogicDocument))
(define (string-as-command-arg s)
  (list (word-piece s)))
