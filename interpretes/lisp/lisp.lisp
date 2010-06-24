;interprete de lisp en lisp

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

(defun procesar_cond (condlist amb)
	(if (null condlist) nil
		(if (caar condlist) (evaluar (cadar condlist) amb)
			(procesar_cond (cdr condlist) amb)
		)
	)
)

(defun evaluarlista (l amb)
	(if (null l) nil
		(cons (evaluar (car l) amb) (evaluarlista (cdr l) amb))
	)
)

(defun aplicar (f args amb)
	(if (atom f)
		(cond
			((equal f 'atom)(atom (car args)))
			((equal f 'listp)(listp (car args)))
			((equal f 'symbolp)(symbolp (car args)))
			((equal f 'numberp)(numberp (car args)))
			((equal f 'null)(null (car args)))
			((equal f 'length)(length (car args)))
			((equal f 'car)(caar args))
			((equal f 'cdr)(cdar args))
			((equal f 'list) args)
			((equal f 'cons)(cons (car args) (cadr args)))
			((equal f 'not)(not (car args)))
			((equal f '+)(+ (car args) (cadr args)))
			((equal f '-)(- (car args) (cadr args)))
			((equal f '*)(* (car args) (cadr args)))
			((equal f '/)(/ (car args) (cadr args)))
			((equal f 'expt)(expt (car args) (cadr args)))
			((equal f 'rem)(rem (car args) (cadr args)))
			((equal f 'sqrt)(sqrt (car args) (cadr args)))
			((equal f '<)(< (car args) (cadr args)))
			((equal f '>)(> (car args) (cadr args)))
			((equal f '>=)(>= (car args) (cadr args)))
			((equal f '>=)(>= (car args) (cadr args)))
			((equal f '=)(= (car args) (cadr args)))
			((equal f 'eq)(eq (car args) (cadr args)))
			((equal f 'equal)(equal (car args) (cadr args)))
			((equal f 'mapcar) (mapcar (car args) (cadr args)))
			((equal f 'apply) (apply (car args) (cadr args)))
			((equal f 'funcall) (apply (car args) (cdr args)))
			(t 'error)
		)
		(cond
			((equal (car f) 'lambda) (evaluar (caddr f) (extenderamb (cadr f) args amb)) );LAMBDAS
			(T 'EXPRESION_NO_SOPORTADA)
		)
	)
)

(defun evaluar (expr amb)
	(if (null expr) nil
		(if (atom expr) (procesar_atomo expr amb)
			(cond
				((equal (car expr) 'quote) (cadr expr))
				((equal (car expr) 'and ) (procesar_and expr amb))
				((equal (car expr) 'or ) (procesar_or expr amb))
				((equal (car expr) 'if ) (procesar_if expr amb))
				((equal (car expr) 'cond ) (procesar_cond (cdr expr) amb))
				((equal (car expr) 'lambda ) expr)
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
(evaluar '(if nil 'verdadero 'falso) nil)
(evaluar '(if t 'verdadero 'falso) nil)
(evaluar '(car (quote (1 2 3))) nil)
(evaluar '(cdr (quote (1 2 3))) nil)
(evaluar '(cons 1 (quote (2 3 4))) nil)
(evaluar '(lambda (x) (+ x 1)) nil)
(evaluar '((lambda (x) (not x)) nil) nil)
(evaluar '(cond (nil 'cond1) (nil 'cond2) ) nil)
(evaluar '(mapcar 'atom (quote ((1) 2 a))) nil)
(evaluar '(apply 'cons (quote (a (1 2))) ) nil)
(evaluar '(funcall 'cons 1 (quote (a b)) ) nil)
(evaluar '(atom 1) nil)
(evaluar '(listp (quote (1 2 3)) ) nil)
(evaluar '(atom (quote (1 2 3)) ) nil)
(evaluar '(listp 1 ) nil)
(evaluar '(length (quote (1 2 3)) ) nil)
(evaluar '(null nil ) nil)
(evaluar '(list 1 2 3 ) nil)

;(print (evaluar (read ) nil))

