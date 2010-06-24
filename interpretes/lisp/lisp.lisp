;interprete de lisp en lisp

(defun esoperador (op expr)
	(equal (car expr) op)
)

(defun extenderamb (lp la amb)
	(if (null lp) amb
		(cons (list (car lp) (car la)) (extenderamb (cdr lp) (cdr la) amb))
	)
)

(defun buscaramb (expr amb)
	(if (null amb) 'ELEMENTO_NO_DEFINIDO
		(if (equal (caar amb) expr) (cadar amb)
			(buscaramb expr (cdr amb))
		)
	)
)

(defun procesar_atomo (expr amb)
	(cond
		((equal expr t) t)
		((numberp expr) expr)
		(t (buscaramb expr amb))
	)
)

(defun procesar_and (expr amb)
	(if (evaluar (cadr expr) amb) 
			(evaluar (caddr expr) amb)
			nil
	)
)

(defun procesar_or (expr amb)
	(if (evaluar (cadr expr) amb)
			t
			(evaluar (caddr expr) amb)
	)
)

(defun procesar_if (expr amb)
	(if (evaluar (cadr expr) amb)
		(evaluar (caddr expr) amb)
		(evaluar (cadddr expr) amb)
	)
)

(defun aplicar (f args amb)
	(if (atom f)
		(cond
			((equal f 'car)(caar args))
			((equal f 'cdr)(cdar args))
			((equal f 'list)(args))
			((equal f 'cons)(cons (car args) (cadr args)))
			((equal f 'not)(not (car args)))
			((equal f '+)(+ (car args) (cadr args)))
			((equal f '-)(- (car args) (cadr args)))
			((equal f '*)(* (car args) (cadr args)))
			((equal f '<)(< (car args) (cadr args)))
			((equal f '>)(> (car args) (cadr args)))
			((equal f '>=)(>= (car args) (cadr args)))
			((equal f '>=)(>= (car args) (cadr args)))
			((equal f '=)(= (car args) (cadr args)))
			;((equal f 'mapcar) ())
			(t 'error)
		)
		(cond
			((equal (car f) 'lambda) (evaluar (caddr f) (extenderamb (cadr f) args amb)) );LAMBDAS
			(T 'EXPRESION_NO_SOPORTADA)
		)
	)
)

(defun evaluarlista (l amb)
	(if (null l) nil
		(cons (evaluar (car l) amb) (evaluarlista (cdr l) amb))
	)
)

(defun evaluar (expr amb)
	(if (null expr) nil
		(if (atom expr) (procesar_atomo expr amb)
			(cond
				((esoperador 'quote expr) (cadr expr))
				((esoperador 'and expr) (procesar_and expr amb))
				((esoperador 'or expr) (procesar_or expr amb))
				((esoperador 'if expr) (procesar_if expr amb))
				((esoperador 'lambda expr) expr)
				(t (aplicar (car expr) (evaluarlista (cdr expr) amb) amb))
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
;car
(evaluar '(car (quote (1 2 3))) nil)
;cdr
(evaluar '(cdr (quote (1 2 3))) nil)
;cons

(evaluar '(cons 1 (quote (2 3 4))) nil)
;otras func
;(evaluar '((if t 'car 'cdr ) (quote (1 2 3)) ) nil)
;(evaluar '((if nil 'car 'cdr ) (quote (1 2 3)) ) nil)
;lambda 
;(trace aplicar)
(evaluar '(lambda (x) (+ x 1)) nil)
(evaluar '((lambda (x) (not x)) nil) nil)
;(evaluar 'pp nil)

