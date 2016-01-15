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

;;; 11.3
(defun first-non-integer (x)
  (dolist (i x)
    (unless (integerp i)
      (return i))))
