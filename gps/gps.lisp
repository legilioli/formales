
(setq calles '((1 (Paseo Colon))(2 Independencia)(3 EEUU)(4 Balcarce)(5 Chile)
			   (6 Venezuela)(7 Mexico)(8 Defensa)(9 Belgrano)))

(setq nodos '((a (b f)) (b (a c)) (c (b d)) (d (c e j)) (e (d)) (f(g))
			   (g(h)) (h (b i)) (i(c j)) (j(k)) (k(l)) (l(e))))

(setq grafo '((a (e c b ))(b (c d))(c (b))(d())(e(d))))
;(setq grafo '((a (b c)) (d (e))))

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
			(gps i f (append (distribuir (car caminos) (diferencia (vecinos (caar caminos) grafo) (car caminos) ) )
						(cdr caminos)))
		)
	) 
)

(defun caminomin (i f)
	(reverse (obtenerminmaxlong (gps i f) '< 9999 nil))
)

(defun caminomax (i f)
	(reverse (obtenerminmaxlong (gps i f) '> -1 nil))
)

(defun obtenerminmaxlong (l f min minimos)
	(if (null l) minimos
		(if (equal (length (car l)) min)
			 (obtenerminmaxlong (cdr l) f min (cons (car l) minimos))
			 (obtenerminmaxlong (cdr l) f ( if (funcall f (length (car l)) min) (length (car l)) min)
				(if (funcall f (length (car l)) min) (car l) (list minimos)))
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

(defun traducircamino (camino calles)
	(if (null camino) nil
		(cons (buscarcalle (car camino) calles)
			  (traducircamino (cdr camino) calles)
		)
	)
)

(defun traducirrutas (caminos calles)
	(if (null caminos) nil
		(cons (traducircamino (car caminos) calles) (traducirrutas (cdr caminos) calles))
	)
)

;(trace vecinos);(trace gps)
(gps 'a 'd )
(caminomin 'a 'd)
(caminomax 'a 'd)
(buscarcalle 'a nodos)
(traducircamino '(b d a) nodos)
(traducirrutas (caminomin 'a 'd) nodos)
