;;;======================================================================================
;;;     GLOL.lisp
;;;     Solves the Farmer (F), Wolf (W), Goat (G), Cavege (C) problem.
;;;     The boat is represented as (B)
;;;     using BFS DFS.
;;;   
;;;     State representation:
;;;             Initial State:              Goal State:
;;;               F W G C B   F W G C B       F W G C B   F W G C B
;;;             ((1 1 1 1 1) (0 0 0 0 0))   ((0 0 0 0 0) (1 1 1 1 1))
;;;
;;;     José Roberto Torres Mancilla
;;; January, 2019
;;;======================================================================================
(defparameter *search-space* '())
(defparameter *memory* '())
(defparameter *operators* '((:Farmer            (1 0 0 0))
                            (:Farmer-Wolf       (1 1 0 0))
                            (:Farmer-Goat       (1 0 1 0))
                            (:Farmer-Cavege  (1 0 0 1))))
(defparameter *id*  -1)
(defparameter *current-parent* NIL)
(defparameter *solution* NIL)
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
(defun barge-shore (state)
    (if (= 1 (fifth (first state))) 0 1))
;;;=======================================================================================
;;;=======================================================================================
(defun valid-operator? (operator state)
    (let*  ((shore  (nth (barge-shore state) state))
            (op     (second operator))
            (farmer (first  shore))
            (wolf   (second shore))
            (goat   (third  shore))                       
	        (cavege (fourth shore)))
        (and (>= farmer (first  op))
            (>= wolf    (second op))
            (>= goat    (third  op))
            (>= cavege  (fourth op)))))
;;;=======================================================================================
;;;=======================================================================================
;;; Invalid States:
;;; ((0 1 1 0) (1 0 0 1)), ((1 0 0 1) (0 1 1 0))
;;; ((0 0 1 1) (1 1 0 0)), ((1 1 0 0) (0 0 1 1))
(defun valid-state? (state)
    (let* ((f0 (first  (first state)))
           (w0 (second (first state)))
           (g0 (third  (first state)))
	       (c0 (fourth (first state)))) 
        (not (or (and (zerop f0) (= 1 w0) (= 1 g0) (zerop c0))
                 (and (= 1 f0) (zerop w0) (zerop g0) (= 1 c0))
                 (and (zerop f0) (zerop w0) (= 1 g0) (= 1 c0))
                 (and (= 1 f0) (= 1 w0) (zerop g0) (zerop c0))))))
;;;=======================================================================================
;;;=======================================================================================
(defun flip (bit) (boole BOOLE-XOR bit 1))
;;;=======================================================================================
;;;=======================================================================================
(defun apply-operator (operator state)
    (let* ((shore1    (first state))
        (shore2    (second  state))
        (f0         (first shore1))
        (f1         (first shore2))
        (w0         (second shore1))
        (w1         (second shore2))
        (g0         (third shore1))
        (g1         (third shore2))
        (c0        (fourth shore1))
        (c1        (fourth shore2))
        (b0         (fifth shore1))
        (b1         (fifth shore2))
        (op (first operator)))
        (case op
            (:Farmer
                (list (list (flip f0) w0 g0 c0 (flip b0)) (list (flip f1) w1 g1 c1 (flip b1))))
            (:Farmer-Wolf
                (list (list (flip f0) (flip w0) g0 c0 (flip b0)) (list (flip f1) (flip w1) g1 c1 (flip b1))))
            (:Farmer-Goat
                (list (list (flip f0) w0 (flip g0) c0 (flip b0)) (list (flip f1) w1 (flip g1) c1 (flip b1))))
            (:Farmer-Cavege
                (list (list (flip f0) w0 g0 (flip c0) (flip b0)) (list (flip f1) w1 g1 (flip c1) (flip b1))))
            (T "Error"))))
;;;=======================================================================================
;;;=======================================================================================
(defun expand (state)
    (let ((children NIL) (new-state NIL))
        (incf *expanded-nodes*)
        (dolist (operator *operators* children)
            (setq new-state (apply-operator operator state))
            (when (and (valid-operator? operator state) (valid-state? new-state))
                (setq children (cons (list new-state operator) children))))))
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
(time (blind-search '((1 1 1 1 1) (0 0 0 0 0)) '((0 0 0 0 0) (1 1 1 1 1)) ':breath-first))
(time (blind-search '((1 1 1 1 1) (0 0 0 0 0)) '((0 0 0 0 0) (1 1 1 1 1)) ':depth-first))
