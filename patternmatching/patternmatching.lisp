(defun buscar (v l)
	(if (null l) 'no_asoc
		(if (equal (car l) v)
			(cadr l)
			(buscar v (cdr l))
		)	
	)
)

(defun verificar (v asoc e l)
	(if (equal asoc 'no_asoc)
		(append l (list v e))
		(if (equal asoc e) l 'nomatch)
	)
)

(defun esvar (v)
	(if (atom v) nil
		(if (and (equal (length v) 2) (equal (car v) '?)) t nil  ) 
	)
)

(defun patternmatching (p e &optional (l nil))
	(if (equal l 'nomatch) 'nomatch
		(if (esvar p) (verificar (cadr p) (buscar (cadr p) l) e l )
			(if (atom p)
				(if (equal p e) l 'nomatch)
				(patternmatching (cdr p) (cdr e) (patternmatching (car p) (car e) l))
			)
		)
	)
)

(patternmatching '( juan (? x) es (muy (? x))) '(juan genio es (muy genio))) ;(x genio)
(patternmatching '((? x) es un (? y)) '(juan es un genio)) ;(x juan y genio)
(patternmatching '((? x) es un (? x)) '(juan es un genio)) ;nomatch
(patternmatching '((? x) es un (? y) (? y) es dora) '(juan es un genio genio es dora)) ; (x juan y genio)
(patternmatching '((? x) es un (? y) (? y) es dora) '(juan es un genio luis es dora)) ; nomatch
