
(define (poksor) (list 30 31))

(display `(1 2 ,(poksor) 3 4))

(display `(1 2 ,@(poksor) 3 4))
