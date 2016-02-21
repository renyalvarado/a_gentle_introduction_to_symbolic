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


;;; 13.9 Cryptogram - Hash
;;; 13.9.a
(defvar *crypto-tex*)
(setf *crypto-tex*
      '("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf"
	"enlpo pib slafml pvv bfwkj"))

(defvar *encipher-table*)
(setf *encipher-table* (make-hash-table))

(defvar *decipher-table*)
(setf *decipher-table* (make-hash-table))

;;; 13.9.b
(defun make-substitution (letter-a letter-b)
  (setf (gethash letter-b *encipher-table*) letter-a)
  (setf (gethash letter-a *decipher-table*) letter-b))

;;; 13.9.c
(defun undo-substitution (letter-a letter-b)
  (setf (gethash letter-b *encipher-table*) nil)
  (setf (gethash letter-a *decipher-table*) nil))

;;; 13.9.d
(defun clear ()
  (clrhash *encipher-table*)
  (clrhash *decipher-table*))

;;; 13.9.e
(defun decipher-string (encoded-string)
  (do* ((len (length encoded-string))
	(new-string (make-string len :initial-element #\Space))
  	(i 0 (+ i 1))
  	(letter)
	(decoded-letter))
       ((= i len) new-string)
    (setf letter (aref encoded-string i))
    (setf decoded-letter (gethash letter *decipher-table*))
    (when decoded-letter
      (setf (aref new-string i) decoded-letter))))

;;; 13.9.f
(defun show-line (line)
  (format t "~%~A~%~A~%"
	  line
	  (decipher-string line)))

;;; 13.9.g
(defun show-text (cryptogram)
  (format t "~&----------------")
  (dolist (line cryptogram)
    (show-line line))
  (format t "~&----------------"))

;;; 13.9.h
(defun get-first-char (x)
  (char-downcase
   (char (format nil "~A" x) 0)))

;;; 13.9.i
(defun read-letter ()
  (let ((answer (read)))
    (if (or (equal answer 'end)
	    (equal answer 'undo))
	answer
	(get-first-char answer))))

;;; 13.9.j
(defun sub-letter (code)
  (if (gethash code *decipher-table*)
      (format t "~&~A has been deciphered as ~S"
	      code
	      (gethash code *decipher-table*))
      (let ((letter))
	(format t "~&What does ~A decipher to? " code)
	(setf letter (read-letter))
	(cond ((not (characterp letter))
	       (format t "~&~A is not a character" letter))
	      ((gethash letter *encipher-table*)
	       (format t "~&~A was cipher already" letter))
	      (t (make-substitution code letter))))))

;;; 13.9.k
(defun undo-letter ()
  (let ((letter)
	(clear))
    (format t "~&Undo which letter?: ")
    (setf letter (read-letter))
    (setf clear (gethash letter *decipher-table*))
    (cond ((not (characterp letter))
	   (format t "~&Invalid input!"))
	  (clear
	   (undo-substitution letter clear))
	  (t
	   (format t "~&The letter ~A is not deciphered" letter)))))

;;; 13.9.k
(defun solve (cryptogram)
  (do ((letter nil))
      (nil)
    (show-text cryptogram)
    (format t "~&Substitute which letter?: ")
    (setf letter (read-letter))
    (cond ((characterp letter)
	   (sub-letter letter))
	  ((equal letter 'undo)
	   (undo-letter))
	  ((equal letter 'end)
	   (return-from solve t))
	  (t
	   (format t "There is an error!")))))



