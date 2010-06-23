;interprete de lisp en lisp

(defun esoperador (op exp)
	(equal (car exp) op)
)

(defun buscar (exp amb)
	exp
)

(defun procesar_atomo (exp amb)
	(cond
		((equal exp t) t)
		((numberp exp) exp)
		(t (buscar exp amb))
	)
)

(defun procesar_and (exp amb)
	(if (evaluar (cadr exp) amb) 
			(evaluar (caddr exp) amb)
			nil
	)
)

(defun procesar_or (exp amb)
	(if (evaluar (cadr exp) amb)
			t
			(evaluar (caddr exp) amb)
	)
)

(defun procesar_if (exp amb)
	(if (evaluar (cadr exp) amb)
		(evaluar (caddr exp) amb)
		(evaluar (cadddr exp) amb)
	)
)

(defun evaluar (exp amb)
	(if (null exp) nil
		(if (atom exp) (procesar_atomo exp amb)
			(cond
				((esoperador 'quote exp) (cadr exp))
				((esoperador 'and exp) (procesar_and exp amb))
				((esoperador 'or exp) (procesar_or exp amb))
				((esoperador 'if exp) (procesar_if exp amb))
				(t nil)
			)
		)
	)
)


;expresion nula
(evaluar nil nil)
;quote
(evaluar '(quote ( a b c)) nil)
;atomo
(evaluar 'atomo nil)
;and
(evaluar '(and t t) nil)
;or
(evaluar '(or t t) nil)
(evaluar '(or nil t) nil)
(evaluar '(or t nil) nil)
(evaluar '(or nil nil) nil)
;if
(evaluar '(if nil 'verdadero 'falso) nil)
(evaluar '(if t 'verdadero 'falso) nil)

