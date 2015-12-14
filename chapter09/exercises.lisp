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
