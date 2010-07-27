;interprete de lisp en lisp

(defun extenderamb (lp la amb)
	(if (null lp) amb
		(cons (list (car lp) (car la)) (extenderamb (cdr lp) (cdr la) amb))
	)
)

(defun buscaramb (expr amb)
	(if (null amb) 'NO_EXISTE
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
			(t
				(if (equal (buscaramb f amb) 'NO_EXISTE)
					'error
					(aplicar (buscaramb f amb) args amb) 
				)
			)
		)
		(cond
			((equal (car f) 'lambda) (evaluar (caddr f) (extenderamb (cadr f) args amb)) );LAMBDAS
			(T (aplicar (evaluar f amb) args amb ))
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

; funcion para comparar el resultado de la evaluacion del interprete del tp
; contra la evaluacion que realiza el interprete real mediante la funcion eval
(defun testexpr (expr)
	(if (equal (eval expr) (evaluar expr nil)) T
		(list expr (eval expr) (evaluar expr nil))
	)
)

; tests
(testexpr nil)
(testexpr '(quote ( a b c)) )
(testexpr '(and t t) )
(testexpr '(or t t))
(testexpr '(or nil t))
(testexpr '(or t nil))
(testexpr '(or nil nil))
(testexpr '(if nil 'verdadero 'falso) )
(testexpr '(if t 'verdadero 'falso) )
(testexpr '(car (quote (1 2 3))) )
(testexpr '(cdr (quote (1 2 3))) )
(testexpr '(cons 1 (quote (2 3 4))) )
(testexpr '((lambda (x) (+ x 1)) 1) )
(testexpr '(cond (nil 'cond1) (nil 'cond2) ) )
(testexpr '(mapcar 'atom (quote ((1) 2 a))) )
(testexpr '(apply 'cons (quote (a (1 2))) ))
(testexpr '(funcall 'cons 1 (quote (a b)) ))
(testexpr '(atom 1))
(testexpr '(listp (quote (1 2 3)) ))
(testexpr '(atom (quote (1 2 3)) ))
(testexpr '(listp 1 ))
(testexpr '(length (quote (1 2 3))))
(testexpr '(null nil ))
(testexpr '(list 1 2 3 ))

;ejemplos de corridas varios
(evaluar '(quote (a b c)) nil) ;(a b c)
(evaluar '(list 1 b 2 d 3) '((b a) (d b) )) ;(a b c d e)
(evaluar '(lambda (x) (+ x 1) ) nil) ;(lambda (x) (+ x 1))
(evaluar '(l1 10) '((l1 (lambda (x) (+ x 1))))) ;11
(evaluar '(+ (+ a 1) b) '((a 1) (b 2)) ) ;4
(evaluar '(l1 l2) '((l1 (lambda (x) x )) (l2 (lambda (y) z))) ) ;(lambda (y) z)
(evaluar '(x 1) nil) ;error
(evaluar '((noexiste 2) 1) nil) ;error
(evaluar '((if (null t) '+ '-) 2 2) nil) ;0
