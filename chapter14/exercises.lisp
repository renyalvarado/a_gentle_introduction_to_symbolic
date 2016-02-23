;;; 14.3
(defmacro set-nil (var)
  (list 'setq var nil))

;;; 14.4
(defmacro simple-rotatef (x y)
  `(let ((old-x ,x)
	 (old-y ,y))
     (setf ,x old-y)
     (setf ,y old-x)))

;;; 14.5
(defmacro set-mutual (x y)
  `(progn
     (setf ,x ',y)
     (setf ,y ',x)))

;;; 14.6
(defmacro variable-chain (&rest vars)
  `(progn
     ,@(do ((l vars (cdr l))
	  (res nil))
	 ((null (rest l)) (reverse res))
       (push `(setf ,(car l) ',(cadr l)) res))))

;;; 14.7
;;; Example
(defvar *nodes*)
(defvar *arcs*)
(defvar *current-node*)

(defun initialize ()
  (setf *nodes* nil)
  (setf *arcs* nil)
  (setf *current-node* nil))

(defstruct (node (:print-function print-node))
  (name nil)
  (inputs nil)
  (outputs nil))

(defun print-node (node stream depth)
  (format stream "#<Node ~A"
	  (node-name node)))

(defun add-node (name)
  (let ((new-node (make-node :name name)))
    (setf *nodes* (nconc *nodes* (list new-node)))
    new-node))

(defun find-node (name)
  (or (find name *nodes* :key #'node-name)
      (error "No node named ~A exists." name)))

(defmacro defnode (name)
  `(add-node ',name))

(defstruct (arc (:print-function print-arc))
  (from nil)
  (to nil)
  (label nil)
  (action nil))

(defun print-arc (arc stream depth)
  (format stream "#<ARC ~A / ~A / ~A>"
	  (node-name (arc-from arc))
	  (arc-label arc)
	  (node-name (art-to arc))))

(defun add-arc (from-name label to-name action)
  (let* ((from (find-node from-name))
	 (to (find-node to-name))
	 (new-arc (make-arc :from from
			    :label label
			    :to to
			    :action action)))
    (setf *arcs* (nconc *arcs* (list new-arc)))
    (setf (node-outputs from)
	  (nconc (node-outputs from)
		 (list new-arc)))
    (setf (node-inputs to)
	  (nconc (node-inputs to)
		 (list new-arc)))
    new-arc))

(defmacro defarc (from label to &optional action)
  `(add-arc ',from ',label ',to ',action))

(defun one-trasition ()
  (format t "~&State ~A. Input: " (node-name *current-node*))
  (let* ((ans (read))
	 (arc (find ans
		    (node-outputs *current-node*)
		    :key #'arc-label)))
    (cond (arc (format t "~&~A: " (arc-action arc))
	       (setf *current-node* (arc-to arc)))
	  (t (format t "~&No arc from ~A has label ~A.~%"
		     *current-node* ans)))
    ))

(defun fsm (&optional (starting-point 'start))
  )
