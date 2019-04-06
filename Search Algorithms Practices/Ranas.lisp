;;;======================================================================================
;;;     Ranas.lisp
;;;     Solves the problem of the Jumping Frogs, using BFS or DFS
;;;   
;;;     State representation:
;;;         An array containing the number of Green Frogs (G) on the left,
;;;         and the number of Red Frogs (R) on the right. And in the middle
;;;         of the list an free space (_).
;;;             Initial State:          Goal State:
;;;             (3 (G G G _ R R R))     (3 (R R R _ G G G))
;;;
;;;     José Roberto Torres Mancilla
;;; January, 2019
;;;======================================================================================
(defparameter  *search-space* '())
(defparameter  *memory* '())
(defparameter  *operators*  '((:2-Derecha    2)
                        (:1-Derecha    1)
                        (:1-Izquierda  -1)
                        (:2-Izquierda  -2)))
(defparameter  *id*  -1)
(defparameter  *current-parent*  nil)
(defparameter  *solution*  nil)
(defparameter *created-nodes* 0)
(defparameter *expanded-nodes* 0)
(defparameter *max-size* 0)
;;;=======================================================================================
;;;=======================================================================================
(defun create-node (state operator)
    (incf *id*)
    (incf *created-nodes*)
    (list *id* state *current-parent* (first operator)))
;;;=======================================================================================
;;;=======================================================================================
(defun sp-size ()
    (when (< *max-size* (length *search-space*))
        (setq *max-size* (length *search-space*))))
;;;=======================================================================================
;;;=======================================================================================
(defun insert-to-open (state operator method)
    (let ((node (create-node state operator)))
        (cond ((eql method :depth-first)
                (push node *search-space*))
	        ((eql method :breath-first)
                (setq *search-space* (append *search-space* (list node))))
            (T NIL)))
    (sp-size))
;;;=======================================================================================
;;;=======================================================================================
(defun get-from-open ()
    (pop *search-space*))
;;;=======================================================================================
;;;=======================================================================================
(defun valid-operator? (operator state)
    (let ((next-index (+ (second operator) (first state))))
        (and (< -1 next-index) (< next-index (length (second state))))))
;;;=======================================================================================
;;;=======================================================================================
(defun apply-operator (operator state)
    (let* ((index (first state))
        (new-state (copy-list (second state)))
        (next-index (+ (second operator) index)))
        (rotatef (nth index new-state)
            (nth next-index new-state))
        (list next-index new-state)))
;;;=======================================================================================
;;;=======================================================================================
(defun expand (state)
    (let ((children NIL))
		(incf *expanded-nodes*)
        (dolist (operator *operators* children)
            (when (valid-operator? operator state)
                (setq children (cons (list 
                    (apply-operator operator state) operator) children))))))
;;;=======================================================================================
;;;=======================================================================================
(defun remember-state? (state memory)
    (cond ((null memory) NIL)
        ((equal state (second (first memory))) T)
        (T (remember-state? state (rest memory)))))
;;;=======================================================================================
;;;=======================================================================================
(defun filter-memories (states-ops)
    (cond ((null states-ops) NIL)
        ((remember-state? (first (first states-ops)) *memory*)
		    (filter-memories (rest states-ops)))
		(T (cons (first states-ops) (filter-memories (rest states-ops))))))
;;;=======================================================================================
;;;=======================================================================================
(defun extract-solution (node)
    (labels ((locate-node (id lst)
	    (cond ((null lst) NIL)
            ((eql id (first (first lst))) (first lst))
            (T (locate-node id (rest lst))))))
        (let ((current (locate-node (first node) *memory*)))
            (loop while (not (null current)) do
                (push current *solution*)
                (setq current (locate-node (third current) *memory*))))
	     *solution*))
;;;=======================================================================================
;;;=======================================================================================
(defun display-solution (nodes)
    (format t "Solition with ~A steps:~%~%" (1- (length nodes)))
    (let ((node NIL))
        (dotimes (i (length nodes))
            (setq node (nth i nodes))
            (if (= i 0)
                (format t "Starts at: ~A~%" (second node))
                (format t "\(~2A\) Applying ~20A we get ~A~%" i (fourth node) (second node))))
        (format t "Created nodes: ~A~%" *created-nodes*)
        (format t "Expanded nodes: ~A~%" *expanded-nodes*)
        (format t "Search space max size: ~A~%" *max-size*)))
;;;=======================================================================================
;;;=======================================================================================
(defun reset-all () 
    (setq *search-space* NIL)
    (setq *memory* NIL)
    (setq *id* 0)
    (setq *current-parent* NIL)
    (setq *solution* NIL)
    (setq *created-nodes* 0)
    (setq *expanded-nodes* 0)
    (setq *max-size* 0))
;;;=======================================================================================
;;;=======================================================================================
(defun  blind-search (initial-state goal-state method)
    (reset-all)
    (let ((node NIL)
        (state NIL)
        (sucesores  '())
        (operator  NIL)
        (goal-found  NIL))
        
        (insert-to-open initial-state NIL method)
        (loop until (or goal-found (null *search-space*)) do
            (setq node (get-from-open)
                state (second node)
                operator (third  node))
            (push node *memory*)
            (cond ((equal goal-state state)
                    (format  t  "Success. Goal found in ~A tries~%" (first node))
                    (display-solution (extract-solution node))
                    (setq goal-found T))
		        (T (setq *current-parent* (first node)) 
			        (setq sucesores (expand state))
			        (setq sucesores (filter-memories  sucesores))
			        (loop for element in sucesores do
                        (insert-to-open (first element) (second element) method)))))))
;;;=======================================================================================
;;;=======================================================================================
(time (blind-search '(3 (G G G _ R R R)) '(3 (R R R _ G G G)) ':breath-first))
(time (blind-search '(3 (G G G _ R R R)) '(3 (R R R _ G G G)) ':depth-first))