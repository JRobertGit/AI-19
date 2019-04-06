;; José Roberto Torres Mancilla

;; Paquete #2 de ejercicios (estructuras algorítmicas)

;; 1) [sin usar ELT ni POSITION] Defina una función ElemInPos que reciba tres argumentos: elem,
;; lista y pos. La función debe devolver T si elem está en la posición pos de lista, NIL si no
;; lo está.
;; Answer... assuming position starts in 1
(defun elemInPos (elem list pos)
    (dotimes (i (1- pos) (eql elem (first list)))
        (setq list (rest list))
    )
)
;; Test
(print (elemInPos 4 '() 2))             ;; => NIL
(print (elemInPos 1 '(1 4 3 2) 1))      ;; => T
(print (elemInPos 4 '(1 2 3 4) 2))      ;; => NIL
(print (elemInPos T '(1 T 3 2) 2))      ;; => T
(print (elemInPos '() '(1 T 3 NIL) 4))  ;; => T
(print "================================================")

;; 2) Escriba la función Inicio-en que recibe como argumentos una lista y un elemento cualquiera. La
;; función debe entregar como respuesta una copia de la lista original pero comenzando con la
;; primera ocurrencia del elemento dado en la lista original.
;; Answer:
(defun startsWith (list elem)
    (let ((found? NIL)
          (result '()))
        (dolist (e list result)
            (when (equal e elem) (setq found? T))
            (if found? (setq result (append result (list e))))
        )
    )
)
;; Test:
(print (startsWith '(1 2 3 4 5) 4))           ;; => (4 5) 
(print (startsWith '(a b c d r) 'd))          ;; => (D R)
(print (startsWith '(1 2 T 4 5) T))           ;; => (T 4 5) 
(print (startsWith '((c) (a) (f) 4 5) '(f)))  ;; => ((F) 4 5)  
(print (startsWith '(1 2 3 4 NIL) NIL))       ;; => (NIL) 
(print (startsWith '(1 2 3 "a" 5) "a"))       ;; => ("a" 5) 
(print "================================================")

;; 3) Modifique la función del ejercicio anterior para que se llame Termina-en y entregue como
;; respuesta una copia de la lista original pero que termina en la última ocurrencia del elemento
;; dado
;; Answer:
(defun endsWith (list elem)
    (let ((result '())
          (last NIL))
        (dolist (e (reverse list) (append (reverse result) (list last)))
            (if (or (not (equal e elem)) (not (null last)))
                (setq result (append result (list e)))
                (setq last e)
            )
        )
    )
)
;; Test:
(print (endsWith '(1 2 4 3 4 5) 4))             ;; => (1 2 4 3 5 4) 
(print (endsWith '(a d b c d r) 'd))            ;; => (A D B C R D)
(print (endsWith '(1 T T 2 T 4 5) T))           ;; => (1 T T 2 4 5 T) 
(print (endsWith '((f) (c) (a) 4 (f) 4 5) '(f)));; => ((F) (C) (A) 4 4 5 (F))  
(print (endsWith '(() 1 2 3 4 ()) '(a)))        ;; => (NIL 1 2 3 4 NIL NIL) 
(print (endsWith '("a" 1 2 3 "a" 5) "a"))       ;; => ((NIL 1 2 3 4 NIL NIL)) 
(print "================================================")

;; 4) Construya una función Primer-impar que reciba como argumento una lista y como respuesta
;; entregue otra lista conteniendo el primer elemento de la lista original que sea un número impar
;; y la posición (índice) donde se encuentra. Observe que los elementos de la lista pueden ser de
;; cualquier tipo de datos.
;;Answer:
(defun firstOdd (list)
    (do ((copy list (rest copy))
         (index 0 (1+ index)))
         ((or (and (integerp (first copy)) (not (zerop (mod (first copy) 2))))
          (= index (length list)))
          (if (not (= index (length list)))
            (list (first copy) index)))
    )
)
;; Test
(print (firstOdd '(2 4 8 7 6)))         ;; => (7 3)
(print (firstOdd '(2 4 8 10 6)))        ;; => NIL
(print (firstOdd '(T 4 "a" NIL 3 6)))   ;; => (3 4)
(print (firstOdd '(20 4 a 19 ())))      ;; => (19 3)
(print (firstOdd '((2 7) 5 b 6)))       ;; => (5 1)
(print "================================================")

;; 5) Modifique la función del inciso anterior para que entregue en la lista de respuesta el último
;; elemento de la lista que sea un número real mayor o igual que cero y el número de veces que
;; dicho elemento se repite en toda la lista.
;; Answer:
(defun lastReal (list)
    (let ((real NIL) (count 0))
         (dolist (i (reverse list) (if (not (null real)) (list real count)))
            (if (and (null real) (realp i))
                (setq real i)
            )
            (if (and (not (null real)) (eql i real))
                (setq count (1+ count))
            )
         )
    )
)
;; Test:
(print (lastReal '(T T T 3 T T 6 T T T 6 T)))               ;; => (6 2)
(print (lastReal '(T T T NIL T T NIL T T T NIL T)))         ;; => NIL
(print (lastReal '(5.1 T 5 a () "a" 2/3 0.1 3.2 5.1 7 5.1)));; => (5.1 3)
(print (lastReal '(a s r T g h v s q 5 y u j)))             ;; => (5 1)
(print "================================================")

;; 6) Escriba la función Conteo que recibe como argumento una lista cualquiera y, como respuesta,
;; entregue una celda de construcción cuya primera parte contiene el conteo de elementos
;; numéricos de la lista original y cuya segunda parte contiene el conteo de sublistas contenidas en
;; la lista original.
;; Answer:
(defun counting (list)
    (let ((numbers 0) (sublists 0))
         (dolist (i list (cons numbers sublists))
            (if (numberp i) (setq numbers (1+ numbers)))
            (if (listp i) (setq sublists (1+ sublists)))
         )
    )
)
;; Test:
(print (counting '(1 2 3 4 (a) (b c))))     ;; => (4 . 2)
(print (counting '(T T a a T T)))           ;; => (0 . 0)
(print (counting '(NIL NIL NIL)))           ;; => (0 . 3)
(print (counting '(1 2 3 4 (a) (b) (c))))   ;; => (4 . 3)
(print (counting '(1 2 3 4)))               ;; => (4 . 0)
(print "================================================")

;; 7) Defina una función Aplana que reciba como argumento una lista con elementos anidados a
;; cualquier nivel de profundidad y, como respuesta, entregue una lista conteniendo los mismos
;; elementos pero todos ellos al nivel principal de profundidad.
;; Answer:
(defun flatten (list)
    (let ((result '()))
        (do ((current (pop list) (pop list)))
             ((and (null current) (null list)) result)
             (if (listp current)
                (setq list (append current list))
                (setq result (append result (list current)))))))
;; Tests:
(print (flatten '(1 b c (a (b) c))))        ;; => (1 B C A B C)
(print (flatten '((4) () T (c) (a (b) c)))) ;; => (4 T C A B C)
(print (flatten '(0 (b) 1.2 (4 (T) 3/2))))  ;; => (0 B 1.2 4 T 3/2)
(print (flatten '(2 T NIL (((h)) (y) bc)))) ;; => (2 T H Y BC)
(print "================================================")

;; 8) Escriba la función Diagonal que recibe como argumento una lista conteniendo m sub-listas de
;; m elementos cada una de ellas y que representa una matriz de m x m elementos. Como
;; respuesta, esta función debe devolver una lista conteniendo los elementos en la diagonal principal
;; de dicha matriz. Observe que los elementos de la matriz pueden ser de cualquier tipo de datos, no
;; forzosamente numéricos.
;; Answer:
(defun diagonal (list)
    (loop for i from 0 to (1- (length list)) collect (nth i (nth i list)))
)
;; Test:
(print (diagonal '((1 0) (0 1))))                       ;; => (1 1)
(print (diagonal '((1 0 0) (0 1 0) (0 0 1))))           ;; => (1 1 1)
(print (diagonal '((4 0 6) (1 79 6) (7 8 0.1))))        ;; => (4 79 0.1)
(print (diagonal '((T NIL a) (9.7 "a" p) (3/4 T ()))))  ;; => (T "a" NIL)
(print (diagonal '()))                                  ;; => NIL
(print (diagonal '((T))))                               ;; => (T)
(print "================================================")

;; 9) Construya una función que reciba como argumento una lista cualquiera y, como respuesta,
;; entregue una lista, con el mismo número de elementos de primer nivel, pero que contiene un
;; símbolo A si el elemento en la posición correspondiente es un átomo, un símbolo L si el
;; elemento correspondiente es una lista y un símbolo N si el elemento en la posición
;; correspondiente es una lista vacía.
;; Answer
(defun listTypes (list)
    (let ((result '()))
        (dolist (i list result)
            (cond ((null i) (setq result (append result (list 'N))))
                  ((atom i) (setq result (append result (list 'A))))
                  ((listp i) (setq result (append result (list 'L))))
            )
        )
    )
)
;; Test:
(print (listTypes '(1 T NIL '() "a" ())))   ;; => (A A N L A N)
(print (listTypes '(T T NIL NIL '() "a")))  ;; => (A A N N L A)
(print (listTypes '(z NIL b a "a" '())))    ;; => (A N A A A L)
(print (listTypes '((a) (n) NIL (a) "a" T)));; => (L L N L A A)
(print "================================================")


;; 10) Defina la función Suma-numérica que recibe como argumento una lista cualquiera (no
;; anidada), y como respuesta entrega la suma de exclusivamente aquellos elementos de la lista que
;; son numéricos.
;; Answer
(defun numberSum (list)
    (let ((sum 0))
        (dolist (i list sum)
            (when (numberp i) (setq sum (+ sum i)))
        )
    )
)
;; Test:
(print (numberSum '(1 2 3 4 5)));           ;; => 15
(print (numberSum '(a b c d e)));           ;; => 0
(print (numberSum '(1/2 2.3 3 T ())));      ;; => 5.8
(print (numberSum '(1 "a" 3.3 NIL '())));   ;; => 4.3
(print (numberSum '()))                     ;; => 0
(print "================================================")

;; 11) Escriba una función Filtra-vocales que reciba como argumento una lista (con elementos de
;; cualquier tipo y anidada a cualquier nivel de profundidad) y, como respuesta entregue una copia
;; de la lista argumento en la cual se han removido las letras vocales (tanto mayúsculas como
;; minúsculas).
;; Answer: A simbol is not a vowel
(defun vowelsFilter (list)
    (let ((result '()))
        (do ((current (pop list) (pop list)))
             ((and (null current) (null list)) result)
             (if (listp current)
                (setq list (append current list))
                (when (not (or (equal current #\a)
								(equal current #\e)
								(equal current #\i)
								(equal current #\o)
								(equal current #\u)
								(equal current #\A)
								(equal current #\E)
								(equal current #\I)
								(equal current #\O)
								(equal current #\U)
								(equal current "a")
								(equal current "e")
								(equal current "i")
								(equal current "o")
								(equal current "u")
								(equal current "A")
								(equal current "E")
								(equal current "I")
								(equal current "O")
								(equal current "U"))) 
                        (setq result (append result (list current))))))))
;; Tests:
(print (vowelsFilter '(1 "a" c (a (#\a) #\c))))      ;; => (1 C A #\c)
(print (vowelsFilter '((4) (#\c) T ("b") (a (b) c))));; => (4 #\c T "b" A B C)
(print (vowelsFilter '(0 #\a (b) 1.2 (4 ("u") 3/2))));; => (0 B 1.2 4 3/2)
(print (vowelsFilter '(2 T "e" ((("o")) ("i") bc)))) ;; => (2 T BC)
(print "================================================")

;; 12) Construya una función Filtra-múltiplos que reciba como argumentos una lista y un número
;; entero. Como respuesta debe entregar una copia de la lista argumento en la cual se han removido
;; todos los múltiplos del entero recibido.
;; Answer:
(defun multiplesFilter (list n)
    (let ((result '()))
        (dolist (i list result)
            (when (or (not (numberp i)) (not (zerop (mod i n))))
                (setq result (append result (list i)))
            )
        )
    )
)
;; Tests:
(print (multiplesFilter '(1 2 3 4 5 6 7 8) 2))      ;; => (1 3 5 7)
(print (multiplesFilter '(a v b 3 T 5 6 NIL 8) 3))  ;; => (A V B T 5 NIL 8)
(print (multiplesFilter '("a" 2 "b" 4 '() 6 7 8) 2));; => ("a" "b" NIL 7)
(print (multiplesFilter '() 2))                     ;; => NIL
(print "================================================")

;; 13) Defina la función Celdas que recibe como argumento una lista (con elementos de cualquier tipo
;; y anidada a cualquier nivel de profundidad) y, como respuesta entrega el número de celdas de
;; construcción que contiene la representación interna de la lista argumento.
;; Answer;
;; Each element on the list counts as 1 construction cell
(defun cellNum (list)
    (let ((count 0))
        (do ((current (pop list) (pop list)))
             ((and (null current) (null list)) count)
             (when (listp current) (setq list (append current list)))
             (setq count (1+ count))
        )
    )
)
;; Tests:
(print (cellNum '(a b (a b) (a) d e (b))))              ;; => 11
(print (cellNum '(a b a b a d e b)))                    ;; => 8
(print (cellNum '(NIL T (a b) ("a") 4 5 (b (((NIL)))))));; => 14
(print (cellNum '(a b (a b) (a) ((())) T (b))))         ;; => 13
(print "================================================")

;; 14) Construya una función Implica con aridad indeterminada, que implemente el operador lógico de
;; la implicación.
;; Answer:
(defun implies (&rest list)
    (let ((implies? T))
        (dolist (i list implies?)
            (setq implies? (or (not implies?) i))
        )
    )
)
;; Tests:
(print (implies T T NIL))       ;; NIL
(print (implies T NIL NIL))     ;; T
(print (implies T T T))         ;; T
(print (implies NIL T T))       ;; T
(print (implies NIL NIL NIL))   ;; NIL
(print "================================================")

;; 15) Escriba una función Mult que recibe como argumento dos listas, conteniendo sub-listas
;; numéricas, representando matrices. La función debe regresar la multiplicación de las dos
;; matrices si es que éstas son compatibles, en caso de no serlo debe regresar NIL.
;; Answer:
(defun matrixMult (list1 list2)
	(let ((m1 (length list1))
	     (n1 (length (first list1)))
	     (m2 (length list2))
	     (n2 (length (first list2))))
	     ( if (= n1 m2)
	        (loop for row1 from 0 to (1- m1)
	            collect (loop for col2 from 0 to (1- n2)
	                collect (loop for col1 from 0 to (1- n1) with n1 = 0 with n2 = 0
	                    do (setq n1 (nth col1 (nth row1 list1)))
	                do (setq n2 (nth col2 (nth col1 list2))) sum (* n1 n2)))
	        )
		)
	)
)
;; Tests:	 
(print (matrixMult '((1 2)(3 4)) '((1 2)(3 4))))                        ;; => ((7 10) (15 22))
(print (matrixMult '((3 2)(1 6)) '((1 2)(3 4))))                        ;; => ((9 14) (19 26))
(print (matrixMult '((10 12)(9 7)) '((1 2)(3 4))))                      ;; => ((46 68) (30 46))
(print (matrixMult '((1 5 2)(3 5 4)(1 7 2)) '((3 5 4)(1 7 2)(3 7 4))))  ;; => ((14 54 22) (26 78 38) (16 68 26))
(print "================================================")