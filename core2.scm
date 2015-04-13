; List utilities.

(define (append x y)
  (if (pair? x)
    (cons (car x) (append (cdr x) y))
    y))

(define (reverse x)
  (if (pair? x)
    (append (reverse (cdr x)) (cons (car x) '()))
    '()))

(define (list? x)
  (if (pair? x)
    (list? (cdr x))
    (null? x)))

(define (list . x) x)

; Numeric.

(define (sum-list x)
  (if (null? x)
    0
    (if (pair? x)
      (add2 (car x) (sum-list (cdr x)))
      (error 'bad-argument-type 'expecting-number))))

(define (sub-list v x)
  (if (null? x)
    v
    (if (pair? x)
      (sub-list (sub2 v (car x)) (cdr x))
      (error 'bad-argument-type 'expecting-number))))

(define (mul-list x)
  (if (null? x)
    1
    (if (pair? x)
      (mul2 (car x) (mul-list (cdr x)))
      (error 'bad-argument-type 'expecting-number))))

(define +
  (lambda x (sum-list x)))

(define (- . x)
  (if (null? x)
    (error 'bad-argument-count 'minus-expects-arguments)
    (if (pair? (cdr x))
      (sub-list (car x) (cdr x))
      (sub2 0 (car x)))))

(define *
  (lambda x (mul-list x)))

(define (display2 x)
  (display x))

(define (not x) (if x #f #t))

; Ports.

(define (call-with-output-file filename proc)
  (let ((p (open-output-file filename)))
    (let ((v (proc p)))
      (close-output-port p)
      v)))

(define (write-string s p)
  (define (write-string-range i n s p)
    (if (< i n)
      (begin
        (write-char (string-ref s i) p)
        (write-string-range (+ i 1) n s p))))
  (write-string-range 0 (string-length s) s p))

'standard-library-initialized
