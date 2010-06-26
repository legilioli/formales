
(defun agregarmem ( vars memoria)
	nil
)

(defun evaluar (prg mem)
	prg
)

(defun esasignacion (expr)
	nil
)

(defun asignacion (expr memoria)
	nil
)

(defun asociar (var valor memoria)
	(if (null memoria) (list var valor)
		(if (equal (caar memoria) var) (cons (list var valor) (cdr memoria))
			(cons (car memoria) (asociar var valor (cdr memoria)))
		)
	)
)

(defun run (prg ent &optional (mem nil))
	(if (null prg) nil
		(if (eq (caar prg) 'int) (run (cdr prg) ent (agregarmem (cdar prg) mem))
			(if (eq (caar prg) 'main) (ejecutar (cadar prg) ent mem) 'ERROR)	
		)
	)
)

(defun esfuncion (prg f)
	(equal (caar prg) f)
)

(defun procesar_if (prg ent mem salida)
	(if (not (equal (evaluar (cadar prg) mem) 0)) 
		(ejecutar (append (list (nth 2 (car prg))) (cdr prg)) ent mem salida); ejecuto if
		(if (equal (length (car prg)) 5) (ejecutar (append (list (nth 4 (car prg))) (cdr prg)) ent mem salida)
						(ejecutar (cdr prg) ent mem salida)
		) ;ejecuto else
	)
)

(defun procesar_while (prg ent mem salida)
	(if (not (equal (evaluar (nth 2 (car prg))) 0))
		(ejecutar (append (list (nth 2 (car prg)) ) prg) ent mem salida)
		(ejecutar (cdr prg) ent mem salida)
	)
)

(defun ejecutar (prg ent mem &optional (salida nil))
	(if (null prg) salida
		(cond
			( (esfuncion prg 'scanf) (ejecutar (cdr prg)(cdr ent) (asociar (cadar prg)(car ent) mem) salida))
			( (esfuncion prg 'printf) (ejecutar (cdr prg) ent mem (cons (evaluar (cadar prg) mem) salida) ) )
			( (esasignacion (car prg)) (ejecutar (cdr prg) ent (asignacion (car prg) mem) salida ) )
			( (esfuncion prg 'if) (procesar_if prg ent mem salida ))
			( (esfuncion prg 'while)(procesar_while prg ent mem salida )) 
			(t (list 'syntax_error  prg ))	
		)
	)
)

(setq code '(
	(int j=10)
	(main( 
		(printf "hola_moncho")
		(if 0 (printf "cond_true") else (printf "cond_false"))	
		(if 0 (printf "cond_true") else (printf "cond_false"))	
		)
	)
))

;(trace ejecutar)
;(trace evaluar)
(run code nil)
;(run '((int i j)(main () )) nil)
