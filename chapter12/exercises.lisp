;;; 12.4.a
(defstruct node
  name
  question
  yes-case
  no-case)

;;; 12.4.b
(defvar *node-list*)
(defun init ()
  (setf *node-list* nil))

;;; 12.4.c
(defun add-node (name question yes-case no-case)
  (let ((new-node (make-node :name name
			     :question question
			     :yes-case yes-case
			     :no-case no-case)))
    (push new-node
	  *node-list*)
    (node-name new-node)))

;;; 12.4.d
(defun find-node (name)
  (find-if #'(lambda (tmp-node)
	       (equal (node-name tmp-node)
		      name)
	       tmp-node) *node-list*))

;;; 12.4.e
(defun process-node (name)
  (let ((tmp-node (find-node name)))
    (cond ((null tmp-node)
	   (format t "~& ~S hasn't been defined yet" name)
	   nil)
	  (t
	   (format t "~&~S " (node-question tmp-node))
	   (if (y-or-n-p)
	       (node-yes-case tmp-node)
	       (node-no-case tmp-node))))))

(defun run ()
  (do ((current-node 'start))
      (nil)
    (cond ((stringp current-node)
	   (format t "~&~S" current-node)
	   (return))
	  ((null current-node) (return))
	  (t
	   (setq current-node (process-node current-node))))))

(defun add-new-node ()
  (labels ((ask-for (question)
	     (format t "~&~A " question)
	     (read)))
    (let ((name (ask-for "Name: "))
	  (question (ask-for "Question: "))
	  (yes-case (ask-for "Yes case: "))
	  (no-case (ask-for "No case: ")))
      (add-node name question yes-case no-case))))

(init)
(add-node 'start
	  "Does the engine turn over?"
	  'engine-turns-over
	  'engine-wont-turn-over)
(add-node 'engine-turns-over
	  "Will the engine run for any period of time?"
	  'engine-will-run-briefly
	  'engine-wont-run)
(add-node 'engine-wont-run
	  "Is there gas in the tank?"
	  'gas-in-tank
	  "Fill the tank and try starting the engine again")
(add-node 'engine-wont-turn-over
	  "Do you hear any sound when you turn the key?"
	  'sound-when-turn-key
	  'no-sound-when-turn-key)
(add-node 'no-sound-when-turn-key
	  "Is the battery voltage low?"
	  "Replace the battery"
	  'battery-voltage-ok)
(add-node 'battery-voltage-ok
	  "Are there battery cables dirty or loose?"
	  "Clean the cables and tighten the connections"
	  'battery-cables-good)

;;; 12.5
(defstruct starship
  (name nil)
  (speed 0)
  (condition 'green)
  (shields 'down))

(defun print-captain (captain  stream depth)
  (format stream "#<CAPTAIN ~A>" (captain-name captain)))

(defstruct (captain
	     (:print-function print-captain))
  name
  age
  ship)

(defvar enterprise)
(setq enterprise (make-starship :name "Enterprise"))

(defvar kirk)
(setq kirk (make-captain :name "James T. Kirk"
			 :age 35
			 :ship enterprise))
