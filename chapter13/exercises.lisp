;;; 13.1
(defun subprop (sym elem prop)
  (setf (get sym prop)
	(remove elem
		(get sym prop))))

;;; 13.2
(defun forget-meeting (person-a person-b)
  (subprop person-a 'person-b 'has-met)
  (subprop person-b 'person-a 'has-met)
  t)

;;; 13.3
(defun my-get (sym elem)
  (do* ((props (symbol-plist sym)
	       (cddr props))
	(prop (car props)
	      (car props)))
       ((null props))
    (if (equal prop elem)
	(return (cadr props)))))

;;; 13.4
(defun hasprop (sym prop)
  (not (equal (get sym prop 'none) 'none)))

;;; 13.8 Histogram - Array
;;; 13.8.a
(defvar *hist-array*)
(defvar *total-points*)

;;; 13.8.b
(defun new-histogram (bins)
  (setf *total-points* 0)
  (setf *hist-array* 
	(make-array bins
		    :initial-element 0)))

;;; 13.8.c
(defun record-value (num)
  (if (and (>= num 0)
	   (< num (length *hist-array*)))
      (progn (incf *total-points*)
	     (incf (aref *hist-array* num)))
      (format t "Num out of value")))

;;; 13.8.d
(defun print-hist-line (num)
  (let ((value (aref *hist-array* num)))
    (format t "~&~2D [~3D] " num value)
    (dotimes (i value)
      (format t "*"))))

;;; 13.8.e
(defun print-histogram ()
  (dotimes (i (length *hist-array*))
    (print-hist-line i))
  (format t "~&~7D total" *total-points*))
