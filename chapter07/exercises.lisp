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

(defun pickout-between-1-and-5 (mylist)
  (remove-if-not #'(lambda (n)
		     (and (> n 1) (< n 5)))
		 mylist))

(defun count-the (mylist)
  (length (remove-if-not #'(lambda (n)
			     (eq n 'the))
			 mylist)))

(defun count-list-length-2 (mylist)
  (length (remove-if-not #'(lambda (n)
			     (equal (length n) 2))
			 mylist)))

(defun my-setdiff (x y)
  (remove-if #'(lambda (e) (member e y))
	     x))

(defun my-setintersection (x y)
  (remove-if-not #'(lambda (e) (member e y))
	     x))

(defun my-setunion (x y)
  (append (my-setdiff y x) x))

(defun rank (card)
  (car card))

(defun suit (card)
  (cadr card))

(defvar my-hand)

(setf my-hand '((3 hearts)
		(5 clubs)
		(2 diamonds)
		(4 diamonds)
		(ace spades)))

(defun count-suit (my-suit)
  (length (remove-if-not #'(lambda (card)
			     (eq my-suit (suit card)))
			 my-hand)))

(defvar colors)

(setf colors '((clubs black)
	       (diamonds red)
	       (hearts red)
	       (spades black)))

(defun color-of (card)
  (cadr (assoc (suit card) colors)))

(defun first-red (hand)
  (find-if #'(lambda (card)
	       (eq 'red (color-of  card)))
	   hand))

(defun black-cards (hand)
  (remove-if-not #'(lambda (card)
		     (eq 'black (color-of  card)))
		 hand))

(defun what-rank (suit hand)
  (mapcar #'car
	  (remove-if-not #'(lambda (card)
			     (eq suit (suit card)))
			 hand)))

(defvar all-ranks)

(setf all-ranks '(2 3 4 5 6 7 8 9 10 jack queen king king ace))

(defun higher-rank-p (x y)
  (consp (member (rank y) (member (rank x) all-ranks))))

(defun total-list (mylist)
  (reduce #'+ (mapcar #'length mylist)))

(defun all-odd (mylist)
  (every #'oddp mylist))

(defun none-odd (mylist)
  (every #'evenp mylist))

(defun not-all-odd (mylist)
  (find-if #'evenp mylist))

(defun not-none-odd (mylist)
  (find-if #'oddp mylist))


(defvar database)

(setf database 
      '((b1 shape brick)
	(b1 color green)
	(b1 size small)
	(b1 supported-by b2)
	(b1 supported-by b3)
	
	(b2 shape brick)
	(b2 color red)
	(b2 size small)
	(b2 supports b1)
	(b2 left-of b3)

	(b3 shape brick)
	(b3 color red)
	(b3 size small)
	(b3 supports b1)
	(b3 right-of b2)

	(b4 shape pyramid)
	(b4 color blue)
	(b4 size large)
	(b4 supported-by b5)

	(b5 shape cube)
	(b5 color green)
	(b5 size large)
	(b5 supports b4)

	(b6 shape brick)
	(b6 color purple)
	(b6 size large)))

(defun match-element (x y)
  (or (eq x y)
      (eq y '?)))

(defun match-triple (x y)
  (every #'match-element x y))

(defun fetch (element)
  (remove-if-not #'(lambda (n)
		     (match-triple n element))
		 database))

; What shape is block b4?
(fetch '(b4 shape ?))

; Which blocks are bricks?
(fetch '(? shape brick))

; What relation is b2 to b3?
(fetch '(b2 ? b3))

; List the color of every block
(fetch '(? color ?))

; What facts are known about block b4?
(fetch '(b4 ? ?))

(defun ask-color (block-name)
  (list block-name 'color '?))

(defun supporters (x)
  (mapcar #'car (fetch (list '? 'supports x))))

(defun supp-cube (block)
  (> (length (mapcar #'(lambda (n)
			 (fetch (list n 'shape 'cube)))
		     (supporters block)))
     0))

(defun desc1 (block)
  (fetch (list block '? '?)))

(defun desc2 (block)
  (mapcar #'cdr (desc1 block)))

(defun description (block)
  (reduce #'append (desc2 block) ))

(defvar words)

(setf words '((one un)
		      (two deux)
		      (three trois)
		      (four quatre)
	      (five cinq)))

(mapcar #'append words (mapcar #'list '(uno dos tres cuatro cinco)))
