
(defun agregarmem ( vars memoria)
	nil
)

(defun asociar (var valor memoria)
	(if (null memoria) (list var valor)
		(if (equal (caar memoria) var) (cons (list var valor) (cdr memoria))
			(cons (car memoria) (asociar var valor (cdr memoria)))
		)
	)
)

(defun run (prg ent &optional (salida nil))
	(if (null prg) nil
		(if (eq (caar prg) 'int) (run (cdr prg) ent (agregarmem (cdar prg) mem))
			(if (eq (caar prg) 'main) (ejecutar (cadar prg) ent mem) 'ERROR)	
		)
	)
)

(defun esfuncion (prg f)
	(equal (caar prg) f)
)

(defun ejecutar (prg ent mem &optional (salida nil))
	(if (null prg) salida
		(cond
			( (esfuncion prg 'scanf) (ejecutar (cdr prg)(cdr ent) (asociar (cadar prg)(car ent) mem) salida))
			( (esfuncion prg 'printf) (ejecutar (cdr prg) ent mem (cons (eval (cadar prg) mem)) ) )
			(t nil)	
		)
	)
)


