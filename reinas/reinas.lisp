
(defun interseccion (x y) 
	(if (null x) nil
		(if (pertenece (car x) y)
			(cons (car x) (interseccion (cdr x) y))
			(interseccion (cdr x) y)
		) 
	)
)


(defun pertenece (x l)
	(if (null l) nil
		(if (equal x (car l)) t (pertenece x (cdr l)))
	)
)

(defun eliminar ( x l)
	(if (null l) nil
		(if (equal x (car l)) (eliminar x (cdr l))
			(cons (car l) (eliminar x (cdr l)))
		)
	)
)

(defun diferencia( a b)
	(if (null b) a
		(if (pertenece (car b) a) 
			(diferencia (eliminar (car b) a) (cdr b))
			(diferencia a (cdr b))
		) 
	)
)
(defun iota (n &optional(seq nil) )
	(if (equal n 0) seq
			(iota (- n 1) (cons n seq ))
	)
)

(defun distribuir (x l)
	(if (null l) nil
		(cons (list x (car l)) (distribuir x (cdr l)))
	)
)

(defun tableronm (n m )
	(if (equal n 0) nil 
		(append (tableronm (- n 1) m) (list ( distribuir n  (iota m))) )
	)
)

(defun tablero (n)
	(tableronm n n)
)


(defun retrocederhistorial (historial)
	(if (null historial) nil
		(if (equal (length (car historial)) 1) (retrocederhistorial (cdr historial)) 
				( cons (cdr (car historial))  (cdr historial))
		)
	)
)

(defun retrocedertablero (tablero  historial n)
	(if (null historial) tablero
		(retrocedertablero (removerinvalidos (caar historial) tablero n) (cdr historial) n)
	)
)

(defun eliminarposicion (l pos)
	(if (null l) nil
		(if (equal (car l) pos) (eliminarposicion (cdr l) pos)
			(cons (car l) (eliminarposicion (cdr l) pos))
		)
	)
)

(defun eliminarposiciones (tablero posiciones)
	(if (null tablero) nil
		(cons (diferencia (car tablero) posiciones) (eliminarposiciones (cdr tablero) posiciones ))
	)
)



(defun elementosdiagonald (pos n &optional (l nil))
		(if (or (equal (car pos) n) (equal (cadr pos) n )) l
			(elementosdiagonald (list (+ (car pos) 1) (+ (cadr pos) 1)) n (cons (list (+ (car pos) 1) (+ (cadr pos) 1)) l))
		)
)

(defun elementosdiagonali (pos n &optional (l nil))
		(if (or (equal (car pos) n) (equal (cadr pos) 0 )) l
			(elementosdiagonali (list (+ (car pos) 1) (- (cadr pos) 1)) n (cons (list (+ (car pos) 1) (- (cadr pos) 1)) l))
		)
)



(defun eliminarcolumna (tablero col)
	(if (null tablero) nil
		(cons (eliminarposicion (car tablero) (list (caaar tablero ) col)) 
				(eliminarcolumna (cdr tablero) col))
	)
)

(defun eliminarvacios(l)
	(if (null l) nil
		(if (null (car l)) (eliminarvacios (cdr l))
			(cons (car l) (eliminarvacios (cdr l)))
		)
	)
)

(defun removerinvalidos (pos tablero n)
(eliminarvacios	(eliminarposiciones
		(eliminarposiciones (eliminarcolumna (cdr tablero) (cadr pos)) 
					(elementosdiagonald pos n)
		)
		(elementosdiagonali pos n)
	))
)



(defun reinas (n tablero historial)
	(if (equal (length historial) n) historial
		(if (null tablero)
				(reinas n (retrocedertablero (tablero n)  (retrocederhistorial historial) n) (retrocederhistorial historial))
				(reinas n (removerinvalidos (caar tablero) tablero n) (cons (car tablero) historial))
		)
	)
)

;(tablero 4)

;(trace eliminardiagonald)
;(eliminarcolumna (tablero 4) 2)
;(removerinvalidos '(2 2 ) (tablero 4) )
;(elementosdiagonald '(1 1) 4)
;(elementosdiagonali '(2 3) 4)
;(eliminarposiciones (tablero 4) (elementosdiagonald '(1 1) 4))
;(removerinvalidos '(1 1) (tablero 4))
;(trace elementosdiagonald)
;;(trace elementosdiagonali)
;(trace reinas)
(mapcar 'car (reinas 19 (tablero 19) nil))
;(trace retrocederhistorial)
;(retrocedertablero (tablero 4) (retrocederhistorial '(((4 2))((2 3)(2 4))((1 1)(1 3)(1 3)(1 4)))) 4)
