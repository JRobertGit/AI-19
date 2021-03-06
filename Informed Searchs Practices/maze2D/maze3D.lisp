;;==================================
;;  3D Maze
;;  José Roberto Torres Mancilla
;;  April 2019
;;==================================

(load "maze_lib.lisp")
(add-algorithm 'Best-First)
(add-algorithm 'A*)

;;===================================================================
;; STATE REPRESEMTATION
;; An array with the form #(y x a c)
;; (aref state 0) => Row
;; (aref state 1) => Column
;; (aref state 2) => Aptitude
;; (aref state 3) => Cost
;; (aref state 4) => Avobe or Below
;;  #(y x) Represent the state's position on the maze
;;====================================================================

;;====================================================================
;; INITIAL STATE
;; *start* (from maze_lib.lisp)
;; GOAL STATE
;; *goal* (from maze_lib.lisp)
;;====================================================================

;;==================================
;;GENERAL SEARCH PARAMETERS
;;==================================
(defparameter *current-ancestor*  NIL)
(defparameter *id*  -1)
(defparameter *memory* '())
(defparameter *search-space* '())

;;==================================
;;BASIC OPERATIONS
;;==================================
(defun create-node (state operator)
    (incf *id*)
    (list *id* state *current-ancestor* (translate-operator (first operator))))

(defun pop-state ()
  (pop  *search-space*))

  (defun insert-node (state operator method)
    (let ((node (create-node state operator)))
        (cond ((eql method :Best-First)
                (when (and (not (remember-state? state *search-space*)))
                    (push node *search-space*)
                    (sort-search-space)) )
            ((eql method :A*)
                (update-search-space node)
                    (sort-search-space)))))

;;==================================
;;MAZE PARAMETERS & OPERATORS
;;==================================
(defparameter *operators* '((:down          (1 0))
                            (:right         (0 1))
                            (:up            (-1 0))
                            (:left          (0 -1))))
(defparameter *solution*  NIL)

;;==================================
;;MAZE HEURISTICS
;;==================================
(defun euclidean-distance (p q)
    (let ((x0 (aref q 0))
        (y0 (aref q 1))
        (x1 (aref p 0))
        (y1 (aref p 1)))
        (expt (+ (expt (- x0 x1) 2) (expt (- y0 y1) 2)) 0.5)))

;;==================================
;;APTITUDE & COST
;;==================================
(defun cost (new-state old-state)
    (+ (aref old-state 3) (euclidean-distance new-state old-state)))

(defun aptitude (state)
    (euclidean-distance state *goal*))

(defun reset-all ()
  (setq *current-ancestor* NIL)
  (setq *id* 0)
  (setq *memory* NIL)
  (setq *search-space* NIL)
  (setq *solution* NIL))

;;==================================
;;OPERATIONS
;;==================================
(defun valid-operator? (operator current-state)
    (let ((op NIL)
         (celltype -1)
         (current-walls NIL)
         (pos 0)
         (new-row (+ (aref current-state 0) (first (second operator))))
         (new-column (+ (aref current-state 1) (second (second operator))))
         (new-celltype -1)
        (new-walls NIL))

        (if (and (>= new-row 0)
                 (< new-row (get-maze-rows))
                 (>= new-column 0)
                 (< new-column (get-maze-cols)))
            (setq new-celltype (get-cell-walls new-row new-column))
            (return-from valid-operator? NIL))

        (setq celltype (get-cell-walls (aref current-state 0) (aref current-state 1)))
        (when (and (<= celltype 15) (> new-celltype 15)) (return-from valid-operator? T))

        (setq current-walls (to-binary celltype))
        (setq new-walls (to-binary new-celltype))
        (setq pos (aref current-state 4))
        (setq op (first operator))
        (if (> celltype 15)
            (if (or (and (or (eql op :left) (eql op :right))
                         (or (and (zerop pos) (= 16 celltype))
                             (and (= 1 pos) (= 17 celltype)))) 
                    (and (or (eql op :down) (eql op :up))
                         (or (and (= 1 pos) (= 16 celltype))
                             (and (zerop  pos) (= 17 celltype))))) NIL T)
            (case op
                (:left  (and (= (nth 0 current-walls) 0) (= (nth 2 new-walls) 0)))
                (:down  (and (= (nth 1 current-walls) 0) (= (nth 3 new-walls) 0)))
                (:right (and (= (nth 2 current-walls) 0) (= (nth 0 new-walls) 0)))
                (:up    (and (= (nth 3 current-walls) 0) (= (nth 1 new-walls) 0)))
                (otherwise NIL)))))

(defun apply-operator (operator state &optional (star NIL))
    (let* ((op (first operator))
        (pos (aref state 4))
        (current-row (aref state 0))
        (current-column (aref state 1))
        (celltype (get-cell-walls current-row current-column))
        (new-celltype -1)
        (new-state (make-array 5)))
        
        (setf (aref new-state 0) (+ current-row (first (second operator))))
        (setf (aref new-state 1) (+ current-column (second (second operator))))
        (setf (aref new-state 2) (aptitude new-state))
        (setq new-celltype (get-cell-walls (aref new-state 0) (aref new-state 1)))

        (if (or (and (or (eql :left op) (eql :right op))
                     (or (and (<= celltype 15) (= new-celltype 16)) 
                         (and (= 16 celltype) (= 16 new-celltype) (= 1 pos))
                         (and (= 17 celltype) (= 16 new-celltype) (zerop pos)))) 
                (and (or (eql :up op) (eql :down op))
                     (or (and (<= celltype 15) (= new-celltype 17)) 
                         (and (= 16 celltype) (= 17 new-celltype) (zerop pos))
                         (and (= 17 celltype) (= 17 new-celltype) (= 1 pos)))))
                    (setf (aref new-state 4) 1)
                    (setf (aref new-state 4) 0))
        ;; IF A*
        (if star (setf (aref new-state 3) (cost new-state state)))
        (if star (setf (aref new-state 2) (+ (aref new-state 2) (aref new-state 3))))
        new-state))

(defun expand (state &optional (star NIL))
    (let ((decendents NIL))
        (dolist (operator *operators* decendents)
            (when (valid-operator? operator state)
                (setq decendents (cons (list (apply-operator operator state star) operator) decendents))))))

(defun to-binary (number)
  (case number
    (0 '(0 0 0 0))
    (1 '(0 0 0 1))
    (2 '(0 0 1 0))
    (3 '(0 0 1 1))
    (4 '(0 1 0 0))
    (5 '(0 1 0 1))
    (6 '(0 1 1 0))
    (7 '(0 1 1 1))
    (8 '(1 0 0 0))
    (9 '(1 0 0 1))
    (10 '(1 0 1 0))
    (11 '(1 0 1 1))
    (12 '(1 1 0 0))
    (13 '(1 1 0 1))
    (14 '(1 1 1 0))
    (15 '(1 1 1 1))
    (16 '(1 0 0 0 0))
    (17 '(1 0 0 0 1))))

(defun translate-operator (operator)
  (case operator
    (:right 2)
    (:down 4)
    (:left 6)
    (:up 0))
)

(defun sort-search-space ()
    (setq *search-space* (stable-sort *search-space* #'< :key #'(lambda (x) (aref (second x) 2)))))

(defun update-search-space (node)
    (let ((state NIL)
        (state-cost NIL)
        (memory-state NIL)
        (memory-cost NIL)
        (found? NIL))
        (when (null *search-space*)
            (push node *search-space*)
            (return-from update-search-space NIL))
        (setq state (second node))
        (setq state-cost (aref state 3))
        (loop for i from 0 to (1- (length *search-space*)) do
            (setq memory-state (second (nth i *search-space*)))
            (setq memory-cost (aref memory-state 3))
            (cond ((and (equalp (aref state 0) (aref memory-state 0))
                        (equalp (aref state 1) (aref memory-state 1))
                        (equalp (aref state 4) (aref memory-state 4)))
                (when (< state-cost memory-cost) (setf (nth i *search-space*) node))
                (setq found? T))))
        (if (not found?) (push node *search-space*))))

(defun remember-state? (state memory-space)
    (let ((space-node (second (first  memory-space))))
        (cond ((null memory-space) NIL)
            ((and (equalp (aref state 0) (aref space-node 0))
                  (equalp (aref state 1) (aref space-node 1))
                  (equalp (aref state 4) (aref space-node 4))) T)
            (T (remember-state? state (rest memory-space))))))

(defun filter-memories (nodes)
    (cond ((null nodes) NIL)
	      ((remember-state? (first (first nodes)) *memory*)
           (filter-memories (rest  nodes)))
          (T (cons (first nodes) (filter-memories (rest nodes))))))

(defun extract-solution (node)
    (labels ((locate-node (id momery)
            (cond ((null momery) NIL)
                  ((eql id (first (first momery))) (first momery))
                  (T (locate-node id (rest momery))))))
        (let ((current (locate-node (first node) *memory*)))
             (loop while (and (not (null current)) (not (null (fourth current)))) do
                (setq *solution* (cons (fourth current) *solution*))
                (setq current (locate-node (third current) *memory*))))))

(defun Best-First ()
    (reset-all)
    (let ((node NIL)
    (state NIL)
    (aux (make-array 2))
    (decendents  NIL)
    (goal-found?  NIL)
    (initial-state (make-array 5))
    (method :Best-First))

    (setf (aref initial-state 0) (aref *start* 0))
    (setf (aref initial-state 1) (aref *start* 1))
    (setf (aref initial-state 2) (aptitude initial-state))
    (setf (aref initial-state 4) 0)

    (insert-node initial-state NIL method)
    (loop until (or goal-found? (null *search-space*)) do
        (setq node (pop-state))
        (setq state (second node))
        (setf (aref aux 0) (aref state 0))
        (setf (aref aux 1) (aref state 1))
        (push node *memory*)
        (cond ((equalp *goal* aux)
                (extract-solution node)
                (format t "Solution found ~a~% " *solution*)
                (setq goal-found? T))
            (T (setq *current-ancestor* (first node))
    			(setq decendents (filter-memories (expand state)))
    			(loop for element in decendents do
    			    (insert-node (first element) (second element) method)))))))

(defun A* ()
    (reset-all)
    (let ((node NIL)
    (state NIL)
    (aux (make-array 2))
    (decendents  NIL)
    (goal-found?  NIL)
    (initial-state (make-array 5))
    (method :A*))

    (setf (aref initial-state 0) (aref *start* 0))
    (setf (aref initial-state 1) (aref *start* 1))
    (setf (aref initial-state 2) (aptitude initial-state))
    (setf (aref initial-state 3) 0)
    (setf (aref initial-state 4) 0)

    (insert-node initial-state NIL method)
    (loop until (or goal-found? (null *search-space*)) do
        (setq node (pop-state))
        (setq state (second node))
        (setf (aref aux 0) (aref state 0))
        (setf (aref aux 1) (aref state 1))
        (push node *memory*)
        (cond ((equalp *goal* aux)
                (extract-solution node)
                (format t "Solution found ~a~% " *solution*)
                (setq goal-found? T))
            (T (setq *current-ancestor* (first node))
    			(setq decendents (filter-memories (expand state T)))
    			(loop for element in decendents do
    			    (insert-node (first element) (second element) method)))))))

(start-maze)
