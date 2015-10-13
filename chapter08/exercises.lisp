(defun anyoddp (x)
  (cond ((null x) nil)
	((oddp (car x)) t)
	(t (anyoddp (cdr x)))
	))

(defun anyoddp-2 (x)
  (if (null x)
      nil
      (if (oddp (car x))
	  t
	  (anyoddp-2 (cdr x)))))

(defun fact (n)
  (cond ((zerop n) 1)
	(t (* n (fact (- n 1))))))


(defun laugh (n)
  (cond ((zerop n) '())
	(t (cons 'ha
		 (laugh (- n 1))))
	))

(defun add-up (mylist)
  (cond ((null mylist) 0)
	(t (+ (car mylist)
	      (add-up (cdr mylist))))))

(defun alloddp (mylist)
  (cond ((null mylist) t)
	((oddp (car mylist)) (alloddp (cdr mylist)))
	(t nil)))

(defun rec-member (x mylist)
  (cond ((null mylist) nil)
	((equal x (car mylist)) mylist)
	(t (rec-member x (cdr mylist)))))

(defun rec-assoc (x mylist)
  (cond ((null mylist) nil)
	((equal x (caar mylist)) (car mylist))
	(t (rec-assoc x (cdr mylist)))))

(defun rec-nth (n mylist)
  (cond ((null mylist) nil)
	((equal n 0) (car mylist))
	(t (rec-nth (- n 1)
		    (cdr mylist)))))

(defun add1 (n)
  (+ n 1))

(defun sub1 (n)
  (- n 1))

(defun rec-plus (x y)
  (cond ((zerop y) x)
	(t (rec-plus (add1 x) (sub1 y)))))

