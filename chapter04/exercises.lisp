(defun make-even (x)
  (if (evenp x) x (+ x 1)))

(defun further (x)
  (if (> x 0)
      (+ x 1)
      (if (< x 0)
	  (- x 1)
	  0)))

(defun my-not (x)
  (if (equal x t)
      nil
      t))

(defun orderer (x y)
  (if (< x y)
      (list x y)
      (list y x)))

(defun probando-numeros (x)
  (cond ((equal x 5) 'es-cinco)
	((equal x 7) 'es-otro-numero)))

(defun my-abs (x)
  (cond ((< x 0) (* -1 x))
	(t x)))

(defun emphasize3 (x)
  (cond ((equal (car x) 'good) (cons 'great (cdr x)))
	((equal (car x) 'bad) (cons 'awful (cdr x)))
	(t (cons 'very x))))

(defun make-odd (x)
  (cond ((not (oddp x)) (+ x 1))
	(t x)))

(defun constraint (x min max)
  (cond ((< x min) min)
	((> x max) max)
	(t x)))

(defun constraint-if (x min max)
  (if (< x min)
      min
      (if (> x max)
	  max
	  x)))

(defun firstzero (mylist)
  (cond ((equal (car mylist) '0) 'first)
	((equal (cadr mylist) '0) 'second)
	((equal (caddr mylist) '0) 'third)
	(t 'none)))

(defun cycle (x)
  (if (equal x 99)
      1
      (+ x 1)))

(defun howcompute (x y z)
  (cond ((equal (+ x y) z) 'sum-of)
	((equal (* x y) z) 'product-of)
	(t '(beats me))))

(defun geq (x y)
  (or (equal x y) (> x y)))

(defun sqrt-double-div2 (x)
  (cond ((and (oddp x) (> x 0)) (* x x))
	((and (oddp x) (< x 0)) (* 2 x))
	(t (/ x 2))))

(defun mytest (gender age)
  (if (or (and (or (equal gender 'boy)
		   (equal gender 'girl))
	       (equal age 'child))
	  (and (or (equal gender 'man)
		   (equal gender 'woman))
	       (equal age 'adult)))
      t))

(defun referee (x y)
  (cond ((equal x y) 'tie)
	((or (and (equal x 'rock)
		  (equal y 'scissors))
	     (and (equal x 'scissors)
		  (equal y 'rock))
	     (and (equal x 'paper)
		  (equal y 'rock)) 
	     (and (equal x 'rock)
		  (equal y 'pape))
	     (and (equal x 'rock)
		  (equal y 'pape)))
	 'first-win)
	(t 'second-wins)))

(defun and-if (x y z w)
  (if x
      (if y
	  (if z 
	      w))))

(defun and-cond (x y z w)
  (cond ((not x) nil)
	((not y) nil)
	((not z) nil)
	(t w)))

(defun compare (x y)
  (cond ((equal x y) 'number-are-the-same)
	((< x y) 'first-is-smaller)
	((> x y) 'first-is-bigger)))

(defun compare-if (x y)
  (if (equal x y)
      'number-are-the-same
      (if (< x y)
	  'first-is-smaller
	  (if (> x y)
	      'first-is-bigger))))

(defun compare-and-or (x y)
  (or (and (equal x y) 'number-are-the-same)
      (and (< x y) 'first-is-smaller)
      (and (> x y) 'first-is-bigger)))

(defun gtest (x y)
  (or (> x y)
      (zerop x)
      (zerop y)))

(defun gtest-if (x y)
  (if (> x y)
      t
      (if (zerop x)
	  t
	  (if (zerop y)
	      t
	      nil))))

(defun gtest-cond (x y)
  (cond ((> x y) t)
	((zerop x) t)
	((zerop y) t)
	(t nil)))

(defun boilingp-cond (temp scale)
  (cond ((and (equal scale 'fahrenheit)
	      (> temp 212)))
	((and (equal scale 'celsius)
	      (> temp 100)))
	(t nil)))

(defun boilingp-cond-cond (temp scale)
  (cond ((equal scale 'fahrenheit)
	 (cond ((> temp 212) t)
	       (t nil)))
	((equal scale 'celsius)
	 (cond ((> temp 100) t)
	       (t nil)))
	(t nil)))

(defun boilingp-if (temp scale)
  (if (and (equal scale 'fahrenheit)
	   (> temp 212))
      t
      (if (and (equal scale 'celsius)
	       (> temp 100))
	  t
	  nil)))

(defun otro-abs (x)
  (if (< x 0)
      (- x)
      x))

(defun logical-and (x y)
  (if x
      (if y
	  t)))
(defun logical-or (x y)
  (if x
      t
      (if y
	  t
	  nil)))

(defun nand (x y)
  (not (and x y)))

(defun not2 (x)
  (nand x x))

(defun nand-logical-and (x y)
  (nand (nand x y) (nand x y)))

(defun nand-logical-or (x y)
  (nand (nand (nand (nand x x)
		    (nand y y))
	      (nand (nand x x)
		    (nand y y)))
	(nand (nand (nand x x)
		    (nand y y))
	      (nand (nand x x)
		    (nand y y)))))

