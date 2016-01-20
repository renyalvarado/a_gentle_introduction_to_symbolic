;;; 11.1
(defun it-member (element mylist)
  (dolist (x mylist)
    (if (equal x element)
      (return t))))

;;; 11.2
(defun it-assoc (element mylist)
  (dolist (x mylist)
    (if (equal element (car x))
      (return x))))

;;; 11.3
(defun rec-check-all-odd (mylist)
  (cond ((null mylist) t)
	((evenp (car mylist)) nil)
	(t (rec-check-all-odd (cdr mylist)))))

;;; 11.4
(defun it-length (mylist)
  (let ((mylength 0))
    (dolist (x mylist mylength)
      (incf mylength 1)	)))

;;; 11.5
(defun it-nth (n mylist)
  (let ((mylength 0))
    (dolist (x mylist)
      (if (equal n mylength)
	  (return x)
	  (incf mylength 1)))))

;;; 11.6
(defun it-union (list01 list02)
  (dolist (x list01 list02)
    (if (not (member x list02))
     (push x list02))))

;;; 11.8
(defun it-reverse (mylist)
  (let ((mylist-rev nil))
    (dolist (x mylist mylist-rev)
      (push x mylist-rev))))

;;; 11.9
(defun check-all-odd (mylist)
  (do ((e mylist (cdr e)))
      ((null e) t)
    (if (evenp (car e))
	(return nil))))

;;; 11.10
(defun launch (n)
  (dotimes (i n)
    (format t "~S..." (- n i)))
  (format t "Blast off!"))

;;; 11.11
(defun find-largest (list-of-numbers)
  (do* ((largest (car list-of-numbers))
	(list-tmp list-of-numbers (cdr list-tmp))
	(x (car list-tmp) (car list-tmp)))
       ((null list-tmp) largest)
    (when (> x largest)
      (setf largest x))))

;;; 11.12
(defun power-of-2 (n)
  (do ((result 1 (+ result result))
       (i 0 (+ i 1)))
      ((equal i n) result)))

;;; 11.13
(defun first-non-integer (x)
  (dolist (i x)
    (unless (integerp i)
      (return i))))

;;; 11.18
(do ((i 0 (+ i 1)))
    ((equal i 5) i)
  (format t "~&I = ~S" i))

(defun fibonacci-do (n)
  (cond ((equal n 1) 1)
	(t (do ((i 2 (+ i 1))
		(fb 0)
		(fb-n-1 1 fb)
		(fb-n-2 0 fb-n-1))
	       ((> i n) fb)
	     (setf fb (+ fb-n-1 fb-n-2))))))


;;; 11.22.a
(defun complement-base (base)
  (cadr (assoc base '((t a) (a t) (g c) (c g)))))

;;; 11.22.b
(defun complement-strand (strand)
  (do ((s strand (rest s))
       (result nil (cons (complement-base (car s))
			 result)))
      ((null s) (reverse result))))

;;; 11.22.c
(defun make-double (strand)
  (do ((s strand (rest s))
       (result nil (cons (list (car s)
			       (complement-base (car s)))
			 result)))
      ((null s) (reverse result))))

;;; 11.22.d
(defun count-bases (strand)
  (do* ((total-bases (list (list 'a 0)
			   (list 't 0)
			   (list 'g 0)
			   (list 'c 0)))
	(s strand (cdr s))
	(e (car s) (car s)))
       ((null s) total-bases)
    (cond ((consp e)
	   (incf (cadr (assoc (car e) total-bases)))
	   (incf (cadr (assoc (cadr e) total-bases))))
	  (t
	   (incf (cadr (assoc e total-bases)))))))

;;; 11.22.e
(defun prefixp (prefix strand)
  (do ((p prefix (cdr p))
       (s strand (cdr s)))
      ((null p) t)
    (when (not (equal (car p) (car s)))
      (return nil))))

;;; 11.22.e
(defun prefixp (prefix strand)
  (do ((p prefix (cdr p))
       (s strand (cdr s)))
      ((null p) t)
    (when (not (equal (car p) (car s)))
      (return nil))))

;;; 11.22.f
(defun appearsp (strand01 strand02)
  (do ((s02 strand02 (rest s02)))
      ((null s02))
    (if (prefixp strand01 s02)
	(return t))))

;;; 11.22.g
(defun coverp (strand01 strand02)
  (do* ((len01 (length strand01))
       (s02 strand02 (nthcdr len01 s02)))
      ((null s02) t)
    (unless (prefixp strand01 s02)
      (return nil))))

;;; 11.22.h
(defun prefix (len strand)
  (do ((s strand (cdr s))
       (i 0 (+ i 1))
       (mypref nil))
      ((equal i len) (reverse mypref))
    (setf mypref (cons (car s) mypref))))

;;; 11.22.i
(defun kernel (strand)
  (do* ((s strand (cdr s))
	(krn (list (car s)) (append krn (list (car s)))))
      ((null s) krn)
    (if (coverp krn strand)
	(return krn))))

;;; 11.22.j
(defun draw-item (elem sep)
  (format t "~A~A~A~A~A" sep sep elem sep sep))

(defun draw-line (size elem sep)
  (format t "~&~&")
  (dotimes (i size)
    (draw-item elem sep)))

(defun draw-strand (mylist)
  (format t "~&~&")
  (dolist (i mylist)
    (draw-item i " ")))

(defun draw-dna (strand)
  (let ((len (length strand)))
    (draw-line len "-" "-")
    (draw-line len "!" " ")
    (draw-strand strand)
    (draw-line len "·" " ")
    (draw-strand (complement-strand strand))
    (draw-line len "!" " ")
    (draw-line len "-" "-")))
