
(setq calles '((1 (Paseo Colon))(2 Independencia)(3 EEUU)(4 Balcarce)(5 Chile)
			   (6 Venezuela)(7 Mexico)(8 Defensa)(9 Belgrano)))

(setq esquinas '((a(1 2)) (b(1 5)) (c(1 7)) (d(1 6)) (e(1 9)) (f(4 2)) 
				 (g(2 8)) (h(8 5)) (i(8 7)) (j(8 6)) (k(8 9)) (l(9 4))))

(setq nodos '((a (b f)) (b (a c)) (c (b d)) (d (c e j)) (e (d)) (f(g))
			   (g(h)) (h (b i)) (i(c j)) (j(k)) (k(l)) (l(e))))


(defun pertenece (x l)
	(if (null l) nil
		(if (equal x (car l)) t (pertenece x (cdr l)))
	)
)

(defun interseccion (x y) 
	(if (null x) nil
		(if (pertenece (car x) y)
			(cons (car x) (interseccion (cdr x) y))
			(interseccion (cdr x) y)
		) 
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

(defun vecinos (x g) 
	(if (null g) nil
		(if (equal (caar g) x) (cadar g)
			(vecinos x (cdr g))
		)
	)
)

(defun distribuir (camino vecinos)
	(if (null vecinos) nil
		(cons (cons (car vecinos) camino) (distribuir camino (cdr vecinos)))
	)
)

(defun gps( i f &optional (caminos (list (list i)))i)

	(if (null caminos) nil
		(if (equal f (caar caminos))
			(cons (car caminos) (gps i f (cdr caminos)))
			(gps i f (append (distribuir (car caminos) (diferencia (vecinos (caar caminos) nodos) (car caminos) ) )
						(cdr caminos)))
		)
	) 
)

(defun caminomin (i f)
	(reverse (car (obtenerminmaxlong (gps i f) '< 9999 nil)))
)

(defun caminomax (i f)
	(reverser (car (obtenerminmaxlong (gps i f) '> -1 nil)))
)

(defun obtenerminmaxlong (l f min minimos)
	(if (null l) minimos
		(if (equal (length (car l)) min)
			 (obtenerminmaxlong (cdr l) f min (cons (car l) minimos))
			 (obtenerminmaxlong (cdr l) f ( if (funcall f (length (car l)) min) (length (car l)) min)
				(if (funcall f (length (car l)) min) (list (car l))  minimos ))
		)
	)
)

(defun buscarcalle (id calles)
	(if (null calles) nil
		(if (equal (caar calles) id) (cadar calles)
			(buscarcalle id (cdr calles))
		)
	)
)


(defun traducirrutas (caminos esquinas calles)
	(if (null caminos) nil
		(cons (traducircamino (car caminos) esquinas calles) (traducirrutas (cdr caminos) esquinas calles))
	)
)

(defun getcallesesquina (idesquina esquinas)
	(if (null esquinas) nil
		(if (equal (caar esquinas) idesquina) (cadar esquinas)
			(getcallesesquina idesquina (cdr esquinas))
		)
	)
)

(defun traducircamino (camino esquinas calles)
	(if (equal (length camino) 1) nil
		(cons  (buscarcalle (car (interseccion (getcallesesquina (car camino) esquinas) (getcallesesquina (cadr camino) esquinas))) calles)
			  (traducircamino (cdr camino) esquinas calles))
	)
)


(defun comprimir (recorrido resumen actual)
	(if (null recorrido) (append resumen (list actual))
		(if (null actual) (comprimir (cdr recorrido) resumen (list (car recorrido) 1))
			(if (equal (car recorrido) (car actual)) (comprimir (cdr recorrido) resumen (list (car actual) (+ (cadr actual) 1)))
			(comprimir (cdr recorrido) (append resumen (list actual)) (list (car recorrido) 1))
			)
		)
	)
)


(defun describir (rec desc calleactual)
	(if (null rec)  (append desc (list '(hasta llegar a destino.)) )
		(if (null calleactual) (describir (cdr rec) (append desc (list '(avance por) (caar rec) (cadar rec) '(cuadras))) (caar rec))
			(describir (cdr rec) (append desc (list '(y gire en) (caar rec) '(y avance) (cadar rec) '(cuadras)) ) (caar rec))
		)
	)
)

(defun comollegar (a b)
	(describir (comprimir (car (traducirrutas (list (caminomin a b)) esquinas calles))  nil nil) nil nil)
)

;(trace vecinos);(trace gps)
;(gps 'a 'd )
;(caminomin 'a 'd)
;(caminomax 'a 'd)
;(buscarcalle '(3) calles)
;(getcallesesquina 'a esquinas)
;(getcallesesquina 'b esquinas)
;(interseccion (getcallesesquina 'a esquinas) (getcallesesquina 'b esquinas))
;(obtenerminmaxlong '((a b c)(a e c)) '< 9999 nil) 
;(traducircamino (caminomax 'a 'd) esquinas calles)
;(trace buscarcalle)
;(trace traducircamino(trace traducircamino))
;(trace describir)
;(describircamino '(a a b) nil nil ) 
;(traducirrutas (caminomax 'a 'd) esquinas calles)
;(describir (comprimir (car (traducirrutas (caminomax 'a 'd) esquinas calles))  nil nil) nil nil)
(comollegar 'k 'i)
