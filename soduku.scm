(define (fac n)
	(if (zero? n)
	1
	(* n (fac (- n 1)))
	)
)

(write (fac 4))
(newline)
(write "jack is awesome")
(newline)
