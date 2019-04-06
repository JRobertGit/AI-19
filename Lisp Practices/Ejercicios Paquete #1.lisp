;; José Roberto Torres Mancilla

;; Paquete #1:
;; 1) Construya una sola expresión LISP para calcular lo que se indica en cada uno de los siguientes incisos:
    (print "===============================================")
    ;; a) El quinto elemento de la lista (((1 2) 3) 4 (5 (6)) A (B C) D (E (F G))), sin usar la funci ón FIFTH.
    ;; Answer:
    (nth 4 '(((1 2) 3) 4 (5 (6)) A (B C) D (E (F G))))
    ;; Test:
    (print (nth 4 '(((1 2) 3) 4 (5 (6)) A (B C) D (E (F G))))) ;; Should be (B C)
    (print "================================================")
    
    ;; b) El número de segundos que tiene el año bisiesto 2004.
    ;; Answer:
    (* 366 24 60 60)
    ;; Test:
    (print (* 366 24 60 60)) ;; Should be 31622400 seconds
    (print "================================================")
        
    ;; c) Si el valor numérico asociado a la variable x es diferente de cero y además
    ;; menor o igual que el valor asociado a la variable y.
    ;; Answer: (and (not (= x 0)) (<= x y))
    ;; Test:
    (defun ltnz(x y)
        (and (not (= x 0)) (<= x y))
    )
    (print (ltnz 1 3)) ;; Should be T
    (print (ltnz 3 3)) ;; Should be T
    (print (ltnz 0 3)) ;; Should be NIL
    (print (ltnz 4 3)) ;; Should be NIL
    (print "================================================")
    
    ;; d) Una lista con las dos soluciones reales de la ecuación
    ;; Answer:
    (list (/ (- -7 (sqrt (- (expt 7 2) (* 4 2 5)))) (* 2 2)) (/ (+ -7 (sqrt (- (expt 7 2) (* 4 2 5)))) (* 2 2)))
    ;; Test:
    (print (list (/ (- -7 (sqrt (- (expt 7 2) (* 4 2 5)))) (* 2 2)) (/ (+ -7 (sqrt (- (expt 7 2) (* 4 2 5)))) (* 2 2)))) ;; Should be (-5/2 -1)
    (print "================================================")

;; 2) Escriba, en notación prefija y evalúe las siguientes expresiones aritméticas:
    ;; a) 2(4)+(6-8) = 8-2 = 6
    ;; Answer:
    (+ (* 2 4) (- 6 8))
    ;; Test:
    (print (+ (* 2 4) (- 6 8))) ;; Should be 6
    (print "================================================")
    
    ;; b) 5+(-3+4) / 6+2/5 = 6 / 32/5 = 30/32 = 15/16 
    ;; Answer:
    (/ (+ 5 (+ -3 4)) (+ 6 (/ 2 5)))
    ;; Test:
    (print (/ (+ 5 (+ -3 4)) (+ 6 (/ 2 5)))) ;; Should be 15/16
    (print "================================================")
    
    ;; c) ((-(-4 -3/8) + 1.4502) / (-1^(3-5)^(1/3)))^1/2
    ;; Answer:
    (sqrt (/ (- 1.4502 (- -4 (/ 3 8))) (expt -1 (expt (- 3 5) (/ 1 3)))))
    ;; Test:
    (print (sqrt (/ (- 1.4502 (- -4 (/ 3 8))) (expt -1 (expt (- 3 5) (/ 1 3)))))) ;; Shoudl be #C(7.3559437 -11.196841)
    (print "================================================")
    ;; d) ((65.402/(-1)^1/2)^1/5 / (0.17))^1/7
    ;; Answer
    (expt (/ (expt (/ 65.402 (sqrt -1)) (/ 1 5)) 0.17) (/ 1 7))
    ;; Test;
    (print (expt (/ (expt (/ 65.402 (sqrt -1)) (/ 1 5)) 0.17) (/ 1 7))) ;; Should be #C(1.4500146 -0.065120235)
    (print "================================================")
    
;; 3) Indique el resultado de evaluar cada una de las siguientes expresiones:
    ;; a) (cdar '((one two) three four)))
    (print (cdar '((one two) three four))) ;; Returns: (TWO)
    (print "================================================")
    
    ;; b) (append (cons '(eva lisa) '(karl sven)) '(eva lisa) '(karl sven))
    (print (append (cons '(eva lisa) '(karl sven)) '(eva lisa) '(karl sven))) ;; Returns: ((EVA LISA) KARL SVEN EVA LISA KARL SVEN)
    (print "================================================")
    
    ;; c) (subst 'gitan 'birgitta '(eva birgitta lisa birgitta karin))
    (print (subst 'gitan 'birgitta '(eva birgitta lisa birgitta karin))) ;; Returns: (EVA GITAN LISA GITAN KARIN)
    (print "================================================")
    
    ;; d) (remove 'sven '(eva sven lisa sven anna))
    (print (remove 'sven '(eva sven lisa sven anna))) ;; Returns: (EVA LISA ANNA)
    (print "================================================")
    
    ;; e) (butlast '(karl adam nilsson gregg alisson vilma) 3)
    (print (butlast '(karl adam nilsson gregg alisson vilma) 3)) ;; Returns: (KARL ADAM NILSSON)
    (print "================================================")
    
    ;; f) (nth 2 '(a b c d e))
    (print (nth 2 '(a b c d e))) ;; Returns: C
    (print "================================================")
    
    ;; g) (nthcdr 2 '(a b c d e))
    (print (nthcdr 2 '(a b c d e))) ;; Returns: (C D E)
    (print "================================================")
    
    ;; h) (intersection '(a b c) '(x b z c))
    (print (intersection '(a b c) '(x b z c))) ;; Returns: (B C)
    (print "================================================")
    
    ;; i) (cdadar '(((((1 2 3) z) y) (x 4)) 7 8 (a b c (5 (6 7 8)))))
    (print (cdadar '(((((1 2 3) z) y) (x 4)) 7 8 (a b c (5 (6 7 8)))))) ;; Returns: (4)
    (print "================================================")

;; 4) Defina una función Recombina que reciba como argumento una lista de la forma ((A . x)
;; (B . y) (C . z)), donde A, B y C son átomos simbólicos, mientras que x, y y z son
;; números. Como respuesta, la función debe entregar otra lista con la siguiente estructura:
;; (((x y) . A) ((y z) . C) ((z y x) . B))
;; Answer:
(defun recombine (list)
    (list 
        (cons (list (rest (first list)) (rest (second list))) (first (first list)))
        (cons (list (rest (second list)) (rest (third list))) (first (third list)))
        (cons (list (rest (third list)) (rest (second list)) (rest (first list))) (first (second list)))
    )
)
;; Test:
(print (recombine '((A . x) (B . y) (C . z)))) ;; Shuld resturn: (((X Y) . A) ((Y Z) . C) ((Z Y X) . B)) 
(print "================================================")

;; 5) Defina un predicado RealNoCero? que reciba un argumento N y responda si su
;; argumento es o no un número real diferente de cero.
;; Answer;
(defun non-zeroReal? (n)
    (and (realp n) (not (zerop n)))
)
;; Test:
(print (non-zeroReal? 1))       ;; Should return T
(print (non-zeroReal? 0))       ;; Should return NIL
(print (non-zeroReal? 0.0))     ;; Should return NIL
(print (non-zeroReal? (sqrt 2)));; Should return T
(print (non-zeroReal? T))       ;; Should return NIL
(print (non-zeroReal? '(1)))    ;; Should return NIL
(print "================================================")

;; 6) Construya una función Analiza, con argumento X, que responda una lista con los valores
;; de verdad correspondientes a las respuestas a las siguientes preguntas: ¿es X un átomo?,
;; ¿es X un número?, ¿es X una lista? , ¿es X una celda de construcción? y ¿es X una
;; lista vacía?
;; Answer:
(defun analize (x)
    (list (atom x) (numberp x) (listp x) (consp x) (null x))
)
;; Test
(print (analize NIL))   ;; Should return: (T NIL T NIL T)
(print (analize 1))     ;; Should return: (T T NIL NIL NIL)
(print (analize '(a)))  ;; Should return: (NIL NIL T T NIL)
(print (analize 'A))    ;; Should return: (T NIL NIL NIL NIL)
(print (analize '()))   ;; Should return: (T NIL T NIL T)
(print (analize T))     ;; Should return: (T NIL NIL NIL NIL)
(print "================================================")

;; 7) Defina una función Intercala que reciba como argumentos dos listas cualesquiera y,
;; como resultado entregue otra lista en la que se encuentran intercalados los elementos de
;; las listas originales; siempre en el mismo orden: un elemento de la primera lista y otro de
;; la segunda lista. Si las listas no tienen la misma longitud, todos los elementos restantes
;; de la lista más grande se colocan seguidos en la respuesta.
;; Answer:
(defun intercalate (list1 list2)
    (cond ((and (null list1) (null list2)) nil)
          ((null list1) list2)
          (T (cons (first list1) (intercalate list2 (rest list1))))
    )
)
;; Test:
(print (intercalate NIL NIL))               ;; Should return NIL
(print (intercalate '() '()))               ;; Should return NIL
(print (intercalate '() '(a)))              ;; Should return (A)
(print (intercalate '(a b) '()))            ;; Should return (A B)
(print (intercalate '(a b) '(c d)))         ;; Should return (A C B D)
(print (intercalate '(a b e f g) '(c d)))   ;; Should return (A C B D E F G)
(print "================================================")

;; 8) Programe un predicado MismoTipo que reciba como argumento dos listas de la misma
;; longitud y como respuesta, devuelva T si ambas listas tienen elementos del mismo
;; tipo y en las mismas posiciones, NIL en caso contrario. Observe que los elementos no
;; requieren ser iguales, sólo del mismo tipo de datos.
;; Answer:
(defun sameType? (list1 list2) 
    (cond ((and (null list1) (null list2)) T)
          (T (and T (typep (first list1) (type-of (first list2))) (sameType? (rest list1) (rest list2))))
    )
)
;; Test:
(print (sameType? '(A 10 1/3 (T) NIL) '(b 6 2/11 (T) NIL)))  ;; Should return T
(print (sameType? '(a "x" NIL) '(y "t" ())))                 ;; Should return T
(print (sameType? '(a "x" NIL) '(y T ())))                   ;; Should return NIL
(print (sameType? '(a 7 1.4 T NIL) '(b 6 2.1 T NIL)))        ;; Should return T
(print (sameType? '(a "x" T) '(y "t" ())))                   ;; Should return NIL
(print "================================================")

;; 9) Defina una función APalíndromo, sensible a mayúsculas y minúsculas, que reciba como
;; argumento una cadena y, como respuesta entrega otra cadena que es el palíndromo de la
;; original. Ejemplo: APalíndromo("Hola") = "HolaaloH".
;; Answer:
(defun toPalindrome (text)
    (concatenate 'string text (reverse text))
)
;; Test:
(print (toPalindrome "Hola"))       ;; Should return "HolaaloH"
(print (toPalindrome "Hello"))      ;; Should return "HelloolleH"
(print (toPalindrome "a;slkdjf"))   ;; Should return "a;slkdjffjdkls;a"
(print "================================================")

;; 10) Defina un predicado Bisiesto que reciba como entrada un número entero representando
;; un año y, como respuesta, indique si se trata de un año bisiesto o no.
;; Answer:
(defun leapYear? (year) 
    (cond ((zerop (mod year 400)) T)
          ((and (zerop (mod year 4)) (not (zerop (mod year 100)))) T)
          (T NIL)
    )
)
;; Test:
(print (leapYear? 2004))    ;; Should return T
(print (leapYear? 2000))    ;; Should return T
(print (leapYear? 1000))    ;; Should return NIL
(print (leapYear? 4))       ;; Should return T
(print "================================================")
