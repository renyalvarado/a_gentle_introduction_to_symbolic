(defun add1 (x)
  (+ x 1))

(defvar daily-planet '((olsen jimmy 123-76-4535 cub-reporter)
		       (kent  clark 089-53-6787 reporter)
		       (lane  lois  951-26-1438 reporter)
		       (white perry 355-16-7439 editor)))

(defun extract-social-number (line)
  (caddr line))

(defun greater-than-five-p (x)
  (> x 5))

(mapcar #'(lambda (n)
	    (- n 7))
	'(1 2 3 4 5))

(mapcar #'(lambda (n)
	    (eq n 't))
	'(1 2 t 4 5 t 7))

(mapcar #'(lambda (n)
	    (cond ((eq 'up) 'down)
		  ((eq 'down) 'up)))
	'(up down up up))

(mapcar #'(lambda (n)
	    (cond ((eq 'up n) 'down)
		  ((eq 'down n) 'up)))
	'(up down up up))

(defun return-roughly-equal (mylist mykey)
  (find-if #'(lambda (n)
	       (let ((min (- mykey 10))
		     (max (+ mykey 10)))
		 (and (> n min)
		      (< n max))))
	   mylist))

(defun find-nested (mylist)
  (find-if #'consp mylist))

(defvar note-table)

(setf note-table '((c . 1)
		   (c-sharp . 2)
		   (d . 3)
		   (d-sharp . 4)
		   (e . 5)
		   (f . 6)
		   (f-sharp . 7)
		   (g . 8)
		   (g-sharp . 9)
		   (a . 10)
		   (a-sharp . 11)
		   (b . 12)))

(defun numbers (mylist)
  (sublis note-table mylist))

(defun numbers-2 (mylist)
  (mapcar #'(lambda (n)
	      (cdr (assoc n note-table)))
	  mylist))

(defun notes (mylist)
  (mapcar #'(lambda (n)
	      (car (rassoc n note-table)))
	  mylist))

(defun raise (n mylist)
  (mapcar #'(lambda (x) (+ x n))
	  mylist))

(defun normalize (mylist)
  (mapcar #'(lambda (n)
	      (cond ((> n 12) (- n 12))
		    ((< n 1)  (+ n 12))
		    (t n)))
	  mylist))

(defun transpose (n mylist)
  (notes (normalize (raise n (numbers mylist)))))
