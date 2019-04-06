;; José Roberto Torres Mancilla

;; Paquete #3 de ejercicios (estructuras algorítmicas)

;; 1) [sin usar ELT ni POSITION] Defina una función ElemInPos que reciba tres argumentos: elem,
;; lista y pos. La función debe devolver T si elem está en la posición pos de lista, NIL si no
;; lo está.
;; Answer... assuming position starts in 1
(defun elemInPos (elem list pos)
    (cond ((null list) NIL)
          ((eql 1 pos) (eql elem (first list)))
          (T (elemInPos elem (rest list) (1- pos)))))
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
	(cond ((null list) NIL)
	      ((equal elem (first list)) list)
	      (T (startsWith (rest list) elem))))
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
(defun endsWithAux (list elem) 
	(cond ((null list) NIL)
	      ((equal elem (first list)) list)
	      (T (endsWithAux (rest list) elem))))
	      
(defun endsWith (list elem)
    (reverse (endsWithAux (reverse list) elem))
)
;; Test:
(print (endsWith '(1 2 4 3 4 5) 4))             ;; => (1 2 4 3 4)
(print (endsWith '(a d b c d r) 'd))            ;; => (A D B C D)
(print (endsWith '(1 T T 2 T 4 5) T))           ;; => (1 T T 2 T)
(print (endsWith '((f) (c) (a) 4 (f) 4 5) '(f)));; => ((F) (C) (A) 4 (F))
(print (endsWith '(() 1 2 3 4 ()) '(a)))        ;; => NIL
(print (endsWith '("a" 1 2 3 "a" 5) "a"))       ;; => ("a" 1 2 3 "a")
(print "================================================")

;; 4) Construya una función Primer-impar que reciba como argumento una lista y como respuesta
;; entregue otra lista conteniendo el primer elemento de la lista original que sea un número impar
;; y la posición (índice) donde se encuentra. Observe que los elementos de la lista pueden ser de
;; cualquier tipo de datos.
;;Answer:
(defun firstOddAux (list index)
    (let ((current (first list)))
        (cond ((null list) NIL)
              ((and (numberp current) (not (zerop (mod current 2)))) (list current index))
              (T (firstOddAux (rest list) (1+ index))))))

(defun firstOdd (list)
    (firstOddAux list 0))
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
    (cond ((null list) NIL)
          (T (let ((current (first list))
                  (next (lastReal (rest list))))
                (if (null next)
                    (if (and (realp current) (<= 0 current))
                        (list current 1)
                        NIL)
                    (if (eql current (first next))
                        (list current (1+ (second next)))
                        next))))))
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
    (cond ((null list) (cons 0 0))
        (T (let ((current (first list))
                (next (counting (rest list))))
                (cond ((numberp current) (cons (1+ (first next)) (rest next)))
                      ((listp current) (cons (first next) (1+ (rest next))))
                      (T next))))))
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
    (let ((current (first list))
         (left (rest list)))
        (cond ((null list) NIL)
            ((listp current) (append (flatten current) (flatten left)))
            (T (append (list current) (flatten left))))))
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
(defun diagonalAux (list index)
    (let ((current (nth index (first list)))
         (left (rest list)))
        (cond ((null list) NIL)
            ((null left) (list current))
            (T (cons current (diagonalAux left (1+ index)))))))

(defun diagonal (list)
    (diagonalAux list 0))
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
    (let ((current (first list)))
        (cond ((null list) NIL)
            ((null current) (cons 'N (listTypes (rest list))))
            ((atom current) (cons 'A (listTypes (rest list))))
            ((listp current) (cons 'L (listTypes (rest list)))))))
;; Test:
(print (listTypes '(1 T NIL '() "a" ())))   ;; => (A A N L A N)
(print (listTypes '(T T NIL NIL '() "a")))  ;; => (A A N N L A)
(print (listTypes '(z NIL b a "a" '())))    ;; => (A N A A A L)
(print (listTypes '((a) (n) NIL (a) "a" T)));; => (L L N L A A)
(print (listTypes '()))                     ;; => NIL
(print "================================================")

;; 10) Defina la función Suma-numérica que recibe como argumento una lista cualquiera (no
;; anidada), y como respuesta entrega la suma de exclusivamente aquellos elementos de la lista que
;; son numéricos.
;; Answer
(defun numberSum (list)
    (let ((current (first list)))
        (cond ((null list) 0)
              ((numberp current) (+ current (numberSum (rest list))))
              (T (numberSum (rest list))))))
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
(defun vocal? (current)
    (or (equal current #\a)
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

(defun vowelsFilter (list)
    (let ((current (first list))
         (left (rest list)))
        (cond ((null list) NIL)
            ((listp current) (append (vowelsFilter current) (vowelsFilter left)))
            ((not (vocal? current)) (append (list current) (vowelsFilter left)))
            (T (vowelsFilter left)))))
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
    (let ((current (first list)))
        (cond ((null list) NIL)
              ((or (not (numberp current)) (not (zerop (mod current n))))
                (cons current (multiplesFilter (rest list) n)))
              (T (multiplesFilter (rest list) n)))))
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
    (cond ((null list) 0)
          ((listp (first list)) (+ 1 (cellNum (first list)) (cellNum (rest list))))
          (T (+ 1 (cellNum (rest list))))))
;; Tests:
(print (cellNum '(a b (a b) (a) d e (b))))              ;; => 11
(print (cellNum '(a b a b a d e b)))                    ;; => 8
(print (cellNum '(NIL T (a b) ("a") 4 5 (b (((NIL)))))));; => 15
(print (cellNum '(a b (a b) (a) ((())) T (b))))         ;; => 13
(print "================================================")


;; 14) Construya una función Implica con aridad indeterminada, que implemente el operador lógico de
;; la implicación.
;; Answer:
(defun impliesAux (val list)
    (cond ((null list) NIL)
        ((null (rest list)) (or (not val) (first list)))
        (T (impliesAux (or (not val) (first list)) (rest list)))))

(defun implies (&rest list)
    (impliesAux T list))
;; Tests:
(print (implies T T NIL))       ;; NIL
(print (implies T NIL NIL))     ;; T
(print (implies T T T))         ;; T
(print (implies NIL T T))       ;; T
(print (implies NIL NIL NIL))   ;; NIL
(print (implies NIL))           ;; NIL
(print (implies T))             ;; T
(print (implies))               ;; NIL
(print "================================================")

;; 15) Escriba una función Mult que recibe como argumento dos listas, conteniendo sub-listas
;; numéricas, representando matrices. La función debe regresar la multiplicación de las dos
;; matrices si es que éstas son compatibles, en caso de no serlo debe regresar NIL.
;; Answer:
(defun transpose (matrix)
  (apply #'mapcar #'list matrix))
  
(defun sum (list)
    (cond ((null list) 0)
    (T (+ (first list) (sum (rest list))))))  

(defun multiply (matrix)
    (sum (apply #'mapcar #'* matrix)))

(defun multiplyVectors (list1 list2)
    (multiply (append list1 list2)))

(defun multiplyMatrixAux (matrix1 matrix2)
    (cond ((and (= 1 (length matrix1)) (= 1 (length matrix2)))
            (multiplyVectors matrix1 matrix2))
          ((and (= 1 (length matrix1)) (< 1 (length matrix2)))
            (cons (multiplyMatrixAux matrix1 (list (first matrix2))) (multiplyMatrixAux matrix1 (rest matrix2))))
          ((< 1 (length matrix1))
            (list (multiplyMatrixAux (list (first matrix1)) matrix2) (multiplyMatrixAux (rest matrix1) matrix2)))))

(defun multiplyMatrix (matrix1 matrix2)
    (let ((matrix2t (transpose matrix2)))
        (if (= (length matrix1) (length matrix2t)) (multiplyMatrixAux matrix1 matrix2t) NIL)))
(print (multiplyMatrix '((1 2)(3 4)) '((1 2)(3 4))))                        ;; => ((7 10) (15 22))
(print (multiplyMatrix '((3 2)(1 6)) '((1 2)(3 4))))                        ;; => ((9 14) (19 26))
(print (multiplyMatrix '((10 12)(9 7)) '((1 2)(3 4))))                      ;; => ((46 68) (30 46))
(print (multiplyMatrix '((1 5 2)(3 5 4)(1 7 2)) '((3 5 4)(1 7 2)(3 7 4))))  ;; => ((14 54 22) (26 78 38) (16 68 26))
(print "================================================")

;; 16) Defina una función recursiva Find que reciba dos argumentos: elem y lista.
;; La función debe devolver NIL si elem no es un elemento de lista, de lo contrario,
;; deberá devolver la sublista que comienza con la primera instancia de elem.
;; Answer:
(defun myFind (list elem) 
	(cond ((null list) NIL)
	      ((equal elem (first list)) list)
	      (T (myFind (rest list) elem))))
;; Test:
(print (myFind '(1 2 3 4 5) 4))           ;; => (4 5) 
(print (myFind '(a b c d r) 'd))          ;; => (D R)
(print (myFind '(1 2 T 4 5) T))           ;; => (T 4 5) 
(print (myFind '((c) (a) (f) 4 5) '(f)))  ;; => ((F) 4 5)  
(print (myFind '(1 2 3 4 NIL) NIL))       ;; => (NIL) 
(print (myFind '(1 2 3 "a" 5) "a"))       ;; => ("a" 5) 
(print "================================================")

;; 17) Defina una función recursiva Cambia que reciba como argumento una lista y dos
;; elementos elem1, elem2. Como respuesta, la función debe entregar otra lista
;; parecida a la original, pero donde todas las ocurrecias de elem1 se sustituyeron por elem2.
;; Answer:
(defun change (list elem1 elem2)
    (let ((current (first list)))
        (cond ((null list ) NIL)
            ((equal current elem1) (cons elem2 (change (rest list) elem1 elem2)))
            (T (cons current (change (rest list) elem1 elem2))))))
;; Tests:
(print (change '(1 1 1 1 1 1 1) 1 2))               ;; => (2 2 2 2 2 2 2)
(print (change '() 1 2))                            ;; => NIL
(print (change '(1 1 1) 2 3))                       ;; => (1 1 1)
(print (change '(1 2 2 2 4 4 4 2 2 2 1 1 1) 2 3))   ;; => (1 3 3 3 4 4 4 3 3 3 1 1 1)

;; 18) En el URL http://www.cliki.net/fibonacci se presentan diversas implementaciones
;; para los números de Fibonacci. Implemente TODAS las opciones que ahí se presentan
;; y compare su desempeño com time para el argumento 50.

;; 18.1)
;; Naive recursive computation of the nth element of the Fibonacci sequence
(defun fib (n)
    (if (< n 2) n
        (+ (fib (1- n)) (fib (- n 2)))))
(time (fib 50))
;; Real time: 5,08 sec.
;; Run time: 5.08 sec.
;; Space: 5 Mb

;; 18.2)
;; Tail-recursive computation of the nth element of the Fibonacci sequence
(defun fib (n)
    (labels ((fib-aux (n f1 f2)
        (if (zerop n) f1
            (fib-aux (1- n) f2 (+ f1 f2)))))
            (fib-aux n 0 1)))
(time (fib 50))
;; Real time: 3.03E-4 sec.
;; Run time: 0.001 sec.
;; Space: 600 Bytes

;; 18.3)
;; loop-based iterative computation of the nth element of the Fibonacci sequence
(defun fib (n)
  (loop for f1 = 0 then f2
        and f2 = 1 then (+ f1 f2)
        repeat n finally (return f1)))
(time (fib 50))
;; Real time: 7.8E-5 sec.
;; Run time: 7.4E-5 sec.
;; Space: 0 Bytes

;; 18.4)
;; do-based iterative computation of the nth element of the Fibonacci sequence
(defun fib (n)
  (do ((i n (1- i))
      (f1 0 f2)
      (f2 1 (+ f1 f2)))
      ((= i 0) f1)))
(time (fib 50))
;; Real time: 7.6E-5 sec.
;; Run time: 0.0 sec.
;; Space: 0 Bytes

;; 18.5)
;; CPS computation of the nth element of the Fibonacci sequence
(defun fib (n)
  (labels ((fib-aux (n k)
	     (if (zerop n)
		 (funcall k 0 1)
		 (fib-aux (1- n) (lambda (x y)
				   (funcall k y (+ x y)))))))
    (fib-aux n #'(lambda (a b) a))))
(time (fib 50))
;; Real time: 2.31E-4 sec.
;; Run time: 0.0 sec.
;; Space: 27008 Bytes

;; 18.6)
(defun fib (n)
    (labels ((fib2 (n)
        (cond ((= n 0)
            (values 1 0))
            (T (multiple-value-bind (val prev-val)
                (fib2 (- n 1))
                (values (+ val prev-val)
                val))))))
     (nth-value 0 (fib2 n))))
(time (fib 50))
;; Real time: 1.02E-4 sec.
;; Run time: 0.0 sec.
;; Space: 488 Bytes

;; 18.7)
;; Successive squaring method from SICP
(defun fib (n)
    (labels ((fib-aux (a b p q count)
        (cond ((= count 0) b)
            ((evenp count)
            (fib-aux a b (+ (* p p) (* q q)) (+ (* q q) (* 2 p q)) (/ count 2)))
            (T (fib-aux (+ (* b q) (* a q) (* a p)) (+ (* b p) (* a q)) p q (- count 1))))))
    (fib-aux 1 0 0 1 n)))
(time (fib 50))
;; Real time: 3.6E-5 sec.
;; Run time: 0.0 sec.
;; Space: 720 Bytes

;; 18.8)
(defun fib (n)
    (if (< n 2) n
        (if (oddp n)
            (let ((k (/ (1+ n) 2)))
                (+ (expt (fib k) 2) (expt (fib (1- k)) 2)))
            (let* ((k (/ n 2)) (fk (fib k)))
                (* (+ (* 2 (fib (1- k))) fk) fk)))))
(time (fib 10))
;; Real time: 4.3E-5 sec.
;; Run time: 4.0E-5 sec.
;; Space: 0 Bytes

;; 18.9)
; Taken from Winston's Lisp, 3rd edition, this is a tail-recursive version, w/o an auxiliary function
(defun fib (n &optional (i 1) (previous-month 0) (this-month 1))
    (if (<= n i)
        this-month
        (fib n (+ 1 i) this-month (+ this-month previous-month))))
(time (fib 50))
;; Real time: 6.8E-5 sec.
;; Run time: 0.0 sec.
;; Space: 0 Bytes

;; 18.10)
;;;Original code by Arnold Schoenhage, 
;;;translated to Scheme by Bradley J. Lucier (2004), 
;;;and adapted to Common Lisp by Nicolas Neuss.
(defun fast-fib-pair (n)
    "Returns f_n f_{n+1}."
    (case n
        ((0) (values 0 1))
        ((1) (values 1 1))
        (T (let ((m (floor n 2)))
                (multiple-value-bind (f_m f_m+1)
                (fast-fib-pair m)
            (let ((f_m^2   (* f_m f_m))
                (f_m+1^2 (* f_m+1 f_m+1)))
                (if (evenp n)
                    (values (- (* 2 f_m+1^2) (* 3 f_m^2) (if (oddp m) -2 2)) (+ f_m^2 f_m+1^2))
                    (values (+ f_m^2 f_m+1^2) (- (* 3 f_m+1^2) (* 2 f_m^2) (if (oddp m) -2 2))))))))))
(times (fib 50))
;; Real time: 0.002773 sec.
;; Run time: 0.002774 sec.
;; Space: 61568 Bytes

;; 18.11)
;; Fibonacci - Binet's Formula
(defun fib (n)
    (* (/ 1 (sqrt 5))
    (- (expt (/ (+ 1 (sqrt 5)) 2) n)
    (expt (/ (- 1 (sqrt 5)) 2) n))))
(times (fib 50))
;; Real time: 3.0E-5 sec.
;; Run time: 0.0 sec.
;; Space: 0 Bytes

;; 18.12)
(defun fib (n)
    (/ (- (expt (/ (+ 1 (sqrt 5)) 2) n)
    (expt (/ (- 1 (sqrt 5)) 2) n))
    (sqrt 5)))
(times (fib 50))
;; Real time: 4.6E-5 sec.
;; Run time: 0.0 sec.
;; Space: 0 Bytes

;; 19) Defina una función recursive Mapea que opere exactamente igual que la función
;; mapcar de Common Lisp
;; Answer
(defun mapTo (function list)
    (cond ((null list) NIL)
        (T (cons (apply function (list (first list))) (mapTo function (rest list))))
    )
)
;; Tests:
(print (mapTo #'1+ '(1 2 3) ))                          ;; => (2 3 4)
(print (mapTo #'list '(1 2 3) ))                        ;; => ((1) (2) (3))
(print (mapTo #'(lambda (x) (cons x (1- x))) '(1 2 3) ));; => ((1 . 0) (2 . 1) (3 . 2))
(print (mapTo #'(lambda (x) (* 2 x)) '(1 2 3) ))        ;; => (2 4 6)

;; 20) Defina una función Aplana que reciba como argumento una lista con elementos anidados a
;; cualquier nivel de profundidad y, como respuesta, entregue una lista conteniendo los mismos
;; elementos pero todos ellos al nivel principal de profundidad.
;; Answer:
(defun flatten (list)
    (let ((current (first list))
         (left (rest list)))
        (cond ((null list) NIL)
            ((listp current) (append (flatten current) (flatten left)))
            (T (append (list current) (flatten left))))))
;; Tests:
(print (flatten '(1 b c (a (b) c))))        ;; => (1 B C A B C)
(print (flatten '((4) () T (c) (a (b) c)))) ;; => (4 T C A B C)
(print (flatten '(0 (b) 1.2 (4 (T) 3/2))))  ;; => (0 B 1.2 4 T 3/2)
(print (flatten '(2 T NIL (((h)) (y) bc)))) ;; => (2 T H Y BC)
(print "================================================")

;; 21) Defina una función recursiva Elimina que reciba como argumentos una lista y un 
;; número real n. La función debe entregar como resultado una copia de la lista original,
;; en la cual se hayan eliminado todos los elementos que no sean numéricos, así como
;; todos aquellos elementos numéricos que sean menores o iguales que n.
;; Answer:
(defun myDelete (list n)
    (let ((current (first list)))
        (cond ((null list) NIL)
            ((and (numberp current) (< n current)) (cons current (myDelete (rest list) n)))
            (T (myDelete (rest list) n))
        )))
;; Tests:
(print (myDelete '(1 1 1 1 1) 2))               ;; => NIL
(print (myDelete '(a b T 1 10 60 3.2) 5))       ;; => (10 60)
(print (myDelete '(5.2 40 T () NIL T a "b") 4)) ;; => (5.2 40)
(print (myDelete '() 1))                        ;; => NIL

;; 22) Defina una función recursiva PegaYCambia que reciba como argumento dos listas
;; lista1 lista2 y dos elementos elem1, elem2. Como respuesta, la función debe
;;entregar una lista donde concatene las dos listas originales, pero substituyendo todas
;; las ocurrencias (en ambas listas) de elem1 por elem2.
;; Answer:
(defun pasteAndChange (list1 list2 elem1 elem2)
    (let ((current (first list1))
         (left (rest list1)))
         (cond ((and (null list1) (null list2)) NIL)
            ((null list1) (pasteAndChange list2 NIL elem1 elem2))
            ((equal current elem1) (cons elem2 (pasteAndChange left list2 elem1 elem2)))
            (T (cons current (pasteAndChange left list2 elem1 elem2)))
        )
    )
)
;; Tests:
(print (pasteAndChange '() '() 1 2))                                ;; => NIL
(print (pasteAndChange '(NIL) '(NIL 1) 1 2))                        ;; => (NIL NIL 2)
(print (pasteAndChange '(1 2 3 4 1 1 1) '(2 3 4 1) 1 2))            ;; => (2 2 3 4 2 2 2 2 3 4 2)
(print (pasteAndChange '(NIL NIL () () T) '(T T T NIL) NIL 'true))  ;; => (TRUE TRUE TRUE TRUE T T T T TRUE)

;; 23) Defina una función QSort que reciba como argumento único una lista e
;; implemente con ellos el algoritmo de ordenamiento Quick Sort, ignorando por
;; completo aquellos elementos de la lista que no sean numéricos. La respuesta
;; de la función debe ser una lista con los elementos numéricos de la original ordenados
;; de forma ascendente.
;; Answer:
(defun filter (list)
    (cond ((null list) NIL)
        ((numberp (first list)) (cons (first list) (filter (rest list))))
        (T (filter (rest list)))
    )
)

(defun QSortAux (list)
    (if (<= (length list) 1)
        list
        (let ((pivot (first list)))
            (append	(QSortAux (remove-if-not #'(lambda (x) (< x pivot)) list))
            (remove-if-not #'(lambda (x) (= x pivot)) list)
            (QSortAux (remove-if-not #'(lambda (x) (> x pivot)) list))))
    )
)

(defun QSort (list)
    (QSortAux (filter list))
)
;; Test:
(print (QSort '()))                         ;; => NIL
(print (QSort '(3 4 1 a 5 T 6 "b")))        ;; => (1 3 4 5 6)
(print (QSort '(4 4 1 a 5 T 6.9 NIL 1 8)))  ;; => (1 1 4 4 5 6.9 8)
(print (QSort '(9 4 1 a 5 T 6.7 "v" NIL 8)));; => (1 4 5 6.7 8 9)
(print (QSort '(9 10 1 a 5 T 0 () 1)))      ;; => (0 1 1 5 9 10)

