
(defun agregarmem (var mem)
	(if (null var) nil
		(if (equal (length var) 1)
			(asociar (car var) 0  mem)
			(if (equal (nth 1 var) '=)
				(asociar (nth 0 var) (nth 2 var)(agregarmem (cdddr var) mem))
				(asociar (car var) 0 (agregarmem (cdr var) mem))
			)
		)	
	)
)

(defun evaluar (prg mem)
	(if (atom prg) 
		(if (esvar prg mem) (buscarvar prg mem)	
			prg
		)
		nil
	)
)

(defun buscarvar (var mem)
	(if (null mem) nil
		(if (equal (caar mem) var) (cadar mem)
		(buscarvar var (cdr mem))
		) 
	)
)

(defun pertenece (x l)
	(if (null l) nil
		(if (equals x (car l)) t
			(pertenece x (cdr l))
		)
	)
)

(defun esvar (var mem)
	(if (null mem) nil
		(if (equal (caar mem) var) t
			(esvar var (cdr mem))
		)
	)
)

(defun esasignacion (expr memoria)
	(if (esvar (car expr) memoria) t
		(or (equal (car expr) '++) (equal (car expr) '--))
	)
)

(defun asignacion (expr memoria)
	(if (esvar (car expr) memoria) 
		(cond
			( (equal (nth 1 expr) '=) (asociar (car expr) (evaluar (nth 2 expr) memoria) memoria))
			( (equal (nth 1 expr) '++) (asignacion (list (car expr) '= (car expr) '+ 1 ) memoria))
			( (equal (nth 1 expr) '--) (asignacion (list (car expr) '= (car expr) '- 1 ) memoria))
			( t (asignacion (list (car l) '= (car l) (nth 1 expr) (nth 3 expr)) memoria))
		)
		(asignacion (reverse expr) memoria)
	)
)

;(defun asignacion (expr mem)
;	nil
;)

(defun asociar (var valor memoria)
	(if (null memoria) (cons (list var valor) memoria)
		(if (equal (caar memoria) var) (cons (list var valor) (cdr memoria))
			(cons (car memoria) (asociar var valor (cdr memoria)))
		)
	)
)

(defun run (prg ent &optional (mem nil))
	(if (null prg) nil
		(if (eq (caar prg) 'int) (run (cdr prg) ent (print (agregarmem (cdar prg) mem)))
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
			( (esasignacion (car prg) mem) (ejecutar (cdr prg) ent (asignacion (car prg) mem) salida ) )
			( (esfuncion prg 'if) (procesar_if prg ent mem salida ))
			( (esfuncion prg 'while)(procesar_while prg ent mem salida )) 
			(t (list 'syntax_error  prg ))	
		)
	)
)

(setq code '(
	(int n j = 10 i k)
	(main(
		(i = 7)
		(i ++)
 		(++ i)
		(-- i)
		(i --)
		(printf "hola_moncho")
		(if 1 (printf "cond_true") else (printf "cond_false"))
		(printf (i+1))
		)
	)
))
;(trace run)
;(trace ejecutar)
(trace evaluar)
(trace esvar)
(trace asignacion)
;(trace agregarmem)
;(trace asociar)
(run code nil)
;(run '((int i j)(main () )) nil)
