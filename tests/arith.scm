
(define (assert x) (if (not x) (display "failed") '()))

(assert (= (+) 0))
(assert (= (+ 1) 1))
(assert (= (+ 2 3) 5))

(assert (= (*) 1))
(assert (= (* 2) 2))
(assert (= (* 2 3) 6))
