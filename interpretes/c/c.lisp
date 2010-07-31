
(defun inf_a_pref (expr ops vars)
	(if (null expr) (componer_expresion ops vars)
		(if (esoperador (car expr))
			(if (> (length ops) 0)
				(if (< (prioridad_op (car expr)) (prioridad_op (car ops)))
					(inf_a_pref (cdr expr) (cons (car expr)(cdr ops)) 
								(cons (list (car ops) (cadr vars) (car vars)) (cddr vars) )
					)
					(inf_a_pref (cdr expr) (cons (car expr) ops) vars)
				)
				(inf_a_pref (cdr expr) (cons (car expr) ops) vars)
			)
			(if (atom (car expr))
				(inf_a_pref (cdr expr) ops (cons (car expr) vars))
				(inf_a_pref (cdr expr) ops (cons (car (inf_a_pref (car expr) nil nil)) vars))
			)
		)	
	)
)

(defun componer_expresion (ops vars)
	(if (null ops) vars
		(componer_expresion (cdr ops) (cons (list (car ops) (cadr vars) (car vars)) (cddr vars) ))
	)
)

(defun esoperador (el)
	(if (null el) nil
		(cond 
			((equal el '+) t)
			((equal el '*) t)
			((equal el '-) t)
			((equal el '/) t)
			((equal el '<) t)
			((equal el '>) t)
			((equal el '<=) t)
			((equal el '>=) t)
			( t nil)
		)
	)
)

(defun prioridad_op (op)
	(if (null op) nil
		(cond
			((equal op '+) 2)
			((equal op '*) 3)
			((equal op '-) 2)
			((equal op '/) 3)
			((equal op '<) 1)
			((equal op '>) 1)
			((equal op '<=) 1)
			((equal op '>=) 1)
			( t 0)
		)
	)
)

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

(defun reemplazarvars (prg mem)
	(if (null prg) nil
		(if (esvar (car prg) mem) 
			(cons (buscarvar (car prg) mem) (reemplazarvars (cdr prg) mem))
			(cons (car prg) (reemplazarvars (cdr prg) mem)) 
		)
	)
)

(defun evaluar_lisp (prg mem)
	(if (atom prg) 
		(if (esvar prg mem) (buscarvar prg mem)	
			prg
		)
		(if (null (eval (car (inf_a_pref (reemplazarvars prg mem) nil nil)))) 0
		 	(if (equal t (eval (car (inf_a_pref (reemplazarvars prg mem) nil nil)))) 1
				(eval (car (inf_a_pref (reemplazarvars prg mem) nil nil)))
			)
		)	
	)
)

(defun filtrar_nil_t (resultado)
	(cond
		((null resultado) 0)
		((equal resultado t) 1)
		(t resultado)
	)
)

(defun evaluar (prg mem)
	(filtrar_nil_t (evaluar_lisp prg mem))
)

(defun buscarvar (var mem)
	(if (null mem) nil
		(if (equal (caar mem) var) (cadar mem)
		(buscarvar var (cdr mem))
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
			( (equal (nth 1 expr) '=) (asociar (car expr) (evaluar (cddr expr) memoria) memoria))
			( (equal (nth 1 expr) '++) (asignacion (list (car expr) '= (car expr) '+ 1 ) memoria))
			( (equal (nth 1 expr) '--) (asignacion (list (car expr) '= (car expr) '- 1 ) memoria))
			( t (asignacion (list (car l) '= (car l) (nth 1 expr) (nth 3 expr)) memoria))
		)
		(asignacion (reverse expr) memoria)
	)
)


(defun asociar (var valor memoria)
	(if (null memoria) (cons (list var valor) memoria)
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
		(ejecutar (append (nth 2 (car prg)) (cdr prg)) ent mem salida); ejecuto if
		(if (equal (length (car prg)) 5) (ejecutar (append (nth 4 (car prg)) (cdr prg)) ent mem salida)
						(ejecutar (cdr prg) ent mem salida)
		) ;ejecuto else
	)
)

(defun procesar_while (prg ent mem salida)
	(if (not (equal (evaluar (nth 1 (car prg)) mem) 0))
		(ejecutar (append (nth 2 (car prg) ) prg) ent mem salida)
		(ejecutar (cdr prg) ent mem salida)
	)
)

(defun procesar_do (prg ent mem salida) 
	(ejecutar (append (nth 1 (car prg)) 
					  (list ( list 'while  (nth 3 (car prg)) (nth 1 (car prg)) ))
					  (cdr prg)) ent mem salida)
)

(defun procesar_for (prg ent mem salida)
 	(ejecutar (append (list (nth 1 (car prg)) (list 'while (nth 2 (car prg))  (append (nth 4 (car prg)) (list (nth 3 (car prg))) ) ) )   (cdr prg))  ent mem salida)
)

(defun ejecutar (prg ent mem &optional (salida nil))
	(if (null prg) salida
		(cond
			( (esfuncion prg 'scanf) (ejecutar (cdr prg)(cdr ent) (asociar (cadar prg)(car ent) mem) salida))
			( (esfuncion prg 'printf) (ejecutar (cdr prg) ent mem (cons (evaluar (cadar prg) mem) salida) ) )
			( (esasignacion (car prg) mem) (ejecutar (cdr prg) ent (asignacion (car prg) mem) salida ) )
			( (esfuncion prg 'if) (procesar_if prg ent mem salida ))
			( (esfuncion prg 'while)(procesar_while prg ent mem salida )) 
			( (esfuncion prg 'do) (procesar_do prg ent mem salida))
			( (esfuncion prg 'for) (procesar_for prg ent mem salida))
			(t (list 'syntax_error  prg ))	
		)
	)
)

(setq code '(
	(int n j = 10 i k)
	(main(
		(i = 1)
		(while (i <= 10)
			(
			 (i ++)
			; (printf "decrementando")
			)
		)
		(-- i)
		(i --)
		(printf "hola_moncho")
		(if 1 ((printf "cond_true")) else ((printf "cond_false")))
		(printf (i + 1))
		(scanf n)
		(printf n)
		(k = ((1 + 2) * ( 3 + 1)))
		(printf k)
		;(do (printf "hola") until (0) )
		(k = 3)
		(do ((k --)(printf k)) while (k >= 1 ))
		(for (i = 1) (i <= 3 ) ( i ++ )
			(
				(printf i)
				(printf i)
			)
		)
		)
	)
))
;(trace run)
(trace ejecutar)
;(trace evaluar)
;(trace esvar)
;(trace asignacion)
;(trace agregarmem)
;(trace asociar)
;(trace evaluar)
;(trace reemplazarvars)
;(trace inf_a_pref)
;(trace procesar_do)
(run code '(hola_mundo 20) )
;(run '((int i j)(main () )) nil)


;(evaluar (car (inf_a_pref  '(< 2 (1 + 3)) nil nil)) nil )
