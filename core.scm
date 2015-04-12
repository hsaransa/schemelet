; This is the first file that is fed to schemelet. Initially there is no
; macro-expander or anything, so this may look a bit clumsy, as most of the
; syntax sugar is done by the macro-expander.
;
; The input is annotated with the position that it was parsed from.
; TODO: EXPLAIN!

(define macro-expander
  (lambda (x)
    (define value      car)
    (define pos        cdr)
    (define make-value cons)

    (define un-annotate
      (lambda (x)
        (if (pair? (value x))
          (cons
            (un-annotate (car (value x)))
            (un-annotate (cdr (value x))))
          (value x))))

    (define not (lambda (x) (if x #f #t)))

    (define list (lambda x x))

    (define append
      (lambda (x y)
        (if (pair? x)
          (cons
            (car x)
            (append (cdr x) y))
          y)))

    (define length
      (lambda (x)
        (if (pair? x)
          (add2 1 (length (cdr x)))
          0)))

    (define value-sym?
      (lambda (x y)
        (eq? (value x) y)))

    (define value-list?
      (lambda (x)
        (if (null? (value x))
          #t
          (if (pair? (value x))
            (value-list? (cdr (value x)))
            #f))))

    (define jollo-expander
      (lambda (x)
        (if (value-sym? (car (value x)) 'jollo)
          (cons
            'pallo
            (pos x))
          x)))

    (define is-in-list?
      (lambda (x l)
        (if (pair? l)
          (if (eq? (car l) x)
            #t
            (is-in-list? x (cdr l)))
          #f)))

    (define match-pattern-raw
      (lambda (p x)
        (if (pair? p)
          (if (pair? (value x))
            (append
              (match-pattern-raw (car p) (car (value x)))
              (match-pattern-raw (cdr p) (cdr (value x))))
            (list #f))
          (if (eq? p 'MATCH)
            (list (cons 'MATCH x))
            (if (eq? p (value x))
              '()
              (list #f))))))

    (define unpack-match
      (lambda (x)
        (if (pair? x)
          (cons (cdr (car x))
                (unpack-match (cdr x)))
          x)))

    (define match-pattern
      (lambda (p x)
        (define match (match-pattern-raw p x))
        (if (not (is-in-list? #f match))
          (unpack-match match)
          #f)))

    (define define-expander
      (lambda (x)
        (define match (match-pattern '(define (MATCH . MATCH) . MATCH) x))
        ;(display (cons 'DEFINE-MUKKI match))
        (if match
          (make-value
            (cons
              (make-value
                'define
                (pos x))
              (make-value
                (cons
                  (car match)
                  (make-value
                    (cons
                      (make-value
                        (cons
                          (make-value
                            'lambda
                            (pos x))
                          (make-value
                            (cons
                              (car (cdr match))
                              (car (cdr (cdr match))))
                            (pos x)))
                        (pos x))
                      (make-value
                        '()
                        (pos x)))
                    (pos x)))
                (pos x)))
            (pos x))
          x)))

    (define and-expander
      (lambda (x)
        (define match0 (match-pattern '(and) x))
        (define match1 (match-pattern '(and MATCH . MATCH) x))
        (if match0
          (make-value
            #t
            (pos x))
          (if match1
            (make-value
              (cons
                (make-value
                  'if
                  (pos x))
                (make-value
                  (cons
                    (car match1)
                    (make-value
                      (cons
                        (make-value
                          (cons
                            (make-value
                              'and
                              (pos x))
                            (car (cdr match1)))
                          (pos x))
                        (make-value
                          (cons
                            (make-value
                              #f
                              (pos x))
                            (make-value
                              '()
                              (pos x)))
                          (pos x)))
                      (pos x)))
                  (pos x)))
              (pos x))
            x)
          x)))

    (define rec-expander
      (lambda (x)
        (if (pair? (value x))
          (if (pair? (value (car (value x))))
            (cons
              (cons
                (expand (car (value x)))
                (rec-expander (cdr (value x))))
              (pos x))
            (cons
              (cons
                (rec-expander (car (value x)))
                (rec-expander (cdr (value x))))
              (pos x)))
          x)))

    ; KORJAA YLHAALTA

    (define expanders
      (list jollo-expander and-expander define-expander rec-expander))

    (define iterate-expanders
      (lambda (x e)
        (if (pair? e)
          (iterate-expanders ((car e) x) (cdr e))
          x)))

    (define expand
      (lambda (x)
        ;(display (cons 'JEPa-EXPANDAAN (un-annotate x)))
        (iterate-expanders x expanders)))

    (define display-all
      (lambda (x)
        (if (pair? x)
          (begin
            (if (car x)
              (display (un-annotate (cdr (car x))))
              (display #f))
            (display-all (cdr x)))
          '())))

;    (display 'TEELEIPA)
;    (display-all (match-pattern '(define (PICK . PICK) . PICK) x))
;    (display (is-in-list? #f (match-pattern '(define (PICK . PICK) . PICK) x)))
;    (display 'END)

;    (display (list 'VUOHI x))
;    (display (cons 'VALMISTA-TULI-UN-ANNOTOITUNA (un-annotate (expand x))))
;    (error 'paska 'mato)

    (expand x)))
    
; Return value
'macro-expander-initialized
