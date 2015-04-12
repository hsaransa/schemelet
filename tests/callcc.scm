
(define (f break)
  (display "foo")
  (break 33)
  (display "baz"))

(call-with-current-continuation f)
