(defvar line)
(setf line '(roses are red))

(defun last-element-cons (mylist)
  (car (last mylist)))

(defun last-element-reverse (mylist)
  (car (reverse mylist)))

(defun last-element-nth (mylist)
  (nth (- (length mylist) 1)
       mylist))

(defun next-to-last (mylist)
  (cadr (reverse mylist)))

(defun next-to-last-nth (mylist)
  (let ((tamanno (length mylist)))
    (if (> tamanno 1)
	(nth (- tamanno 2) mylist))))

(defun my-butlast (mylist)
  (reverse (cdr (reverse mylist))))

(defun palindromep (mylist)
  (equal mylist
	 (reverse mylist)))

(defun make-palindrome (mylist)
  (append mylist
	  (reverse mylist)))

(defun contains-article-p (mylist)
  (intersection '(the a or and) mylist))

(defun contains-article-cond (mylist)
  (or (member 'the mylist)
      (member 'a mylist)
      (member 'and mylist)))

(defun contains-article-member (mylist)
  (not (and (not (member 'the mylist))
	    (not (member 'a mylist))
	    (not (member 'and mylist)))))

(defun add-vowels (letters)
  (union letters '(a e i o u)))

(defun my-subsetp (x y)
  (and (set-difference x y) t))

(defun set-equal (x y)
  (and (subsetp x y)
       (subsetp y x)))

(defun proper-subsetp (x y)
  (and (subsetp x y)
       (not (subsetp y x))))

(defvar my-features)

(setf my-features '(large red shiny cube -vs-
		    small shiny red four-sided pyramid))

(defun right-side (elements)
  (cdr (member '-vs- elements)))

(defun left-side (elements)
  (reverse(right-side (reverse elements))))



(defun count-common (elements)
  (length (intersection (right-side elements)
			(left-side elements))))

(defun compare (elements)
  (list (count-common elements) 'common 'features))

(defvar produce)

(setf produce '((apple . fruit)
		(celery . veggie)
		(banana . fruit)
		(lettuce . veggie)))

(defvar books)

(setf books '((war-and-peace leo-tolstoy)
	      (the-history-of-the-peloponnesian-war thucydides)
	      (phaedrus plato)
	      (the-quixote miguel-de-cervantes)
	      (hamlet shakespeare)))

(defun who-wrote (title)
  (cadr(assoc title books)))

(defvar nerd-states)

(setf nerd-states '((sleeping . eating)
		    (eating . waiting)
		    (waiting . programming)
		    (programming . debugging)
		    (debugging . sleeping)))

(defun nerdus (state)
  (cdr (assoc state nerd-states)))

(defun sleepless-nerd (state)
  (let ((next-state (nerdus state)))
    (if (equal next-state 'sleeping)
	'eating
	next-state)))

(defun nerd-on-caffeine (state)
  (nerdus (nerdus state)))

(defun swap-first-and-last (mylist)
  (let* ((myfirst (list (first mylist)))
	 (mylast (last mylist))
	 (myrest (reverse (cdr (reverse (cdr mylist))))))
    (if (equal (length mylist) 1)
	mylist
	(append mylast myrest myfirst))
    ))

(defun rotate-left (mylist)
  (append (cdr mylist) (list (car mylist))))

(defun rotate-right (mylist)
  (let ((mylist-inverted (reverse mylist)))
    (cons  (first mylist-inverted)
	   (reverse (cdr mylist-inverted)))))

(defvar rooms)

(setf rooms
      '((living-room
	 (north front-stairs)
	 (south dining-room)
	 (east kitchen))
	
	(upstairs-bedroom
	 (west library)
	 (south front-stairs))
	
	(dining-room
	 (north living-room)
	 (east pantry)
	 (west downstairs-bedroom))

	(kitchen
	 (west living-room)
	 (south pantry))

	(pantry
	 (north kitchen)
	 (west dining-room))

	(downstairs-bedroom
	 (north back-stairs)
	 (east dining-room))

	(back-stairs
	 (south downstairs-bedroom)
	 (north library))

	(front-stairs
	 (north upstairs-bedroom)
	 (south living-room))

	(library
	 (east upstairs-bedroom)
	 (south back-stairs))
	))

(defun choices (choice)
  (cdr (assoc choice rooms)))

(defun look (direction choice)
  (cadr (assoc direction (choices choice))))

(defvar loc)

(defun set-robbie-location (place)
  "Moves Robbie to PLACE by setting
  the variable LOC."
  (setf loc place))

(defun how-many-choices ()
  (length (choices loc)))

(defun upstairsp (location)
  (or (equal 'upstairs-bedroom location)
      (equal 'library location)))

(defun onstairsp (location)
  (or (equal 'front-stairs location)
      (equal 'back-stairs location)))

(defun where ()
  (if (onstairsp loc)
      (list 'robbie 'is 'on 'the loc)
      (list 'robbie 'is
	    (if (upstairsp loc)
		'upstairs
		'downstairs)
	    'in 'the loc)))

(defun move (direction)
  (let ((location (cadr (assoc direction (choices loc)))))
    (if location
	(progn (set-robbie-location location)
	       (where))
	'(outch! robbie hit a wall))))

(defun royal-we (mylist)
  (subst 'we 'i mylist))

