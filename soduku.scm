(define (fac n)
	(if (zero? n)
	write(n)
	(newline)
	1
	(* n (fac (- n 1)))
	)
)

(write fac (4))
(newline)
