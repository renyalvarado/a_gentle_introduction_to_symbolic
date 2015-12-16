;; Format

(defun pilots ()
  (format t "~&There are old pilots,
             ~&and there are bold pilots,
             ~&but there are no old bold pilots"))

(defun draw-line (n)
  (cond ((zerop n) nil)
	(t (format t "*")
	   (draw-line (- n 1)))))

(defun draw-line2 (n)
  (labels ((draw-aux  (n mytext)
	     (cond ((zerop n) (format t mytext))
		   (t (draw-aux (- n 1)
				(concatenate 'string mytext "*"))))))
    (draw-aux n "")))

(defun draw-box (n m)
  (cond ((zerop m) nil)
	(t (draw-line n)
	   (format t "~&")
	   (draw-box n (- m 1)))))

(defun ninety-nine-bottles (n)
  (cond ((zerop n) nil)
	(t (format t "~S bottles of beer on the wall ~&" n)
	   (format t "~S bottles of beer ~&" n)
	   (format t "Take  one down ~&")
	   (format t "Pass it around ~&")
	   (ninety-nine-bottles (- n 1)))))

(defun print-board (mylist)
  (labels ((print-value (element)
	     (cond ((null element) " ")
		   (t element)))
	   (print-my-element (n mylist)
	     (cond ((null mylist) nil)
		   (t (format t " ~A " (print-value (car mylist)))
		      (cond ((equal (mod n 3) 2)
			     (format t "~&")
			     (if (not (equal n 8))
				 (format t "-----------~&")))
			    (t (format t "|")))
		      (print-my-element (+ n 1) (cdr mylist)))))	   )
    (print-my-element 0 mylist)))


;; Read

(defun pay ()
  (format t "~&Pay Given")
  (format t "~&Hourly wage: ")
  (let ((wage (read)))
    (format t "~&Hours:")
    (let ((hours (read)))
      (format t "~&Gross pay: ~A" (* wage hours))
	)))

(defun cookie-monster ()
  (format t "~&Gime me cookie!!!")
  (format t "~&Cookie? ")
  (let ((answer (read)))
    (cond ((equal answer 'cookie)
	   (format t "Thank you! ... Munch munch much ... BURP"))
	  (t (format t "No want ~A~%~%" answer)
	     (cookie-monster)))))

;; 9.10

(defun space-over (n)
  (cond ((< n 0)
	 (format t "Error"))
	((> n 0)
	 (format t " ")
	 (space-over (- n 1)))
	(t nil)))

(defun plot-one-point (plotting-string y-val)
  (space-over y-val)
  (format t "~A~&" plotting-string))

(defun plot-points (plotting-string mylist)
  (mapcar #'(lambda (n) (plot-one-point plotting-string n)) mylist))

(defun generate (m n)
  (cond ((> m n) nil)
	(t (cons m (generate (+ m 1) n)))))

(defun square (n)
  (* n n))

(defun make-graph ()
  (format t "Function to graph? ")
  (let ((func (read)))
      (format t "Starting x value? ")
   (let ((start (read)))
     (format t "Ending x value? ")
     (let ((end (read)))
       (format t "Plottin string? ")
       (let ((plotting-string (read)))
	 (plot-points plotting-string
		      (mapcar func (generate start end))))))))

