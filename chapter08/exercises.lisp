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

(defun fib (n)
  (cond ((or (equal n 0)
	     (equal n 1)) 1)
	(t (+ (fib (- n 2))
	      (fib (- n 1))))))

(defun find-first-odd (x)
  (cond ((null x) nil)
	((oddp (car x)) (car x))
	(t (find-first-odd (cdr x)))))

(defun last-element (x)
  (cond ((atom (cdr x)) (car x))
	(t (last-element (cdr x)))))

(defun add-nums (n)
  (cond ((zerop n) 0)
	(t (+ n
	      (add-nums (- n 1))))))

(defun all-equal (x)
  (cond ((null (cdr x)) t)
	(t (and (equal (car x) (cadr x))
		(all-equal (cdr x))))))

(defun count-down (x)
  (cond ((zerop x) nil)
	(t (cons x (count-down (- x 1))))))

(defun applicative-fact (n)
  (reduce #'* (count-down n)))

(defun count-down-to-zero (x)
  (cond ((equal x -1) nil)
	(t (cons x (count-down-to-zero (- x 1))))))

(defun count-down-to-zero2 (x)
  (cond ((< x 0) nil)
	(t (cons x (count-down-to-zero2 (- x 1))))))

(defun square-list (x)
  (cond ((null x) nil)
	(t (cons (* (car x) (car x)) (square-list (cdr x))))))

(defun my-nth-mod (n x)
  (cond ((null x) x)
	((zerop n) (car x))
	(t (my-nth-mod (- n 1) (cdr x)))))

(defun my-member (n x)
  (cond ((null x) nil)
	((equal n (car x)) x)
	(t (my-member n (cdr x)))))

(defun my-assoc (n x)
  (cond ((null x) nil)
	((equal n (caar x)) (car x))
	(t (my-assoc n (cdr x)))))

(defun compare-legths (x1 x2)
  (cond ((and (null x1) (null x2)) 'same-length)
	((null x1) 'second-is-longer)
	((null x2) 'first-is-longer)
	(t (compare-legths (cdr x1) (cdr x2)))))

(defun sum-numeric-elements (x)
  (cond ((null x) 0)
	((numberp (car x))
	 (+ (car x) (sum-numeric-elements (cdr x))))
	(t (sum-numeric-elements (cdr x)))))

(defun my-remove (n x)
  (cond ((null x) nil)
	((equal n (car x))
	 (my-remove n (cdr x)))
	(t (cons (car x) (my-remove n (cdr x))))))

(defun my-intersection (x y)
  (cond ((null x) nil)
	((member (car x) y)
	 (cons (car x) (my-intersection (cdr x) y)))
	(t (my-intersection (cdr x) y))))

(defun my-set-difference (x y)
  (cond ((null x) nil)
	((member (car x) y) (my-set-difference (cdr x) y))
	(t (cons (car x) (my-set-difference (cdr x) y)))))

(defun count-odd (x)
  (cond ((null x) 0)
	((oddp (car x))
	 (+ 1 (count-odd (cdr x))))
	(t (count-odd (cdr x)))))

(defun count-atoms (x)
  (cond ((atom x) 1)
	(t (+ (count-atoms (car x))
	      (count-atoms (cdr x))))))

(defun count-cons (x)
  (cond ((atom x) 0)
	(t (+ 1
	      (count-cons (car x))
	      (count-cons (cdr x))))))

(defun sum-tree (x)
  (cond ((numberp (car x)) (car x))
	((atom x) 0)
	(t (+      (sum-tree (car x))
		   (sum-tree (cdr x))))))

(defun my-subst (old new x)
  (cond ((equal x old) new)
	((atom x) x)
	(t (cons (my-subst old new (car x))
		 (my-subst old new (cdr x))))))

(defun flatten (x)
  (cond ((atom x) (list x))
	(t (append (flatten (car x))
		   (and (cdr x)
			(flatten (cdr x)))))))
(defun tree-depth (x)
  (cond ((atom x) 0)
	(t (+ 1 (max (tree-depth (car x))
		     (tree-depth (cdr x)))))))

(defun count-up (n)
  (cond ((zerop n) '())
	(t (append (count-up (- n 1))
		   (list n)))))

(defun make-loaf (n)
  (if (zerop n)
      nil
      (cons 'x
	    (make-loaf (- n 1)))))

(defun bury (x n)
  (cond ((zerop n) x)
	(t (list (bury x (- n 1))))))

(defun pairings (x y)
  (cond ((null x) nil)
	(t (cons (list (car x) (car y))
		 (pairings (cdr x) (cdr y))))))

(defun sublists (x)
    (cond ((atom x) x)
	  (t (cons (cons (sublists (car x))
			 (car (sublists (cdr x))))
		   (sublists (cdr x))))))

(defun sublists2 (x)
  (cond ((null x) nil)
	(t (cons x
		 (sublists2 (cdr x))))))

(defun my-reverse (tree)
  (aux-reverse tree '()))

(defun aux-reverse (tree reverted)
  (cond ((null tree) reverted)
	(t (aux-reverse (cdr tree)
			(cons (car tree) reverted)))))

(defun largest-even (x)
  (cond ((null x) 0)
	((oddp (car x)) (largest-even (cdr x)))
	(t (max (car x)
		(largest-even (cdr x))))))

(defun huge (n)
  (aux-huge n n))

(defun aux-huge (n expp)
  (cond ((zerop expp) 1)
	(t (* n (aux-huge n (- expp 1))))))

(defun every-other (x)
  (cond ((null x) nil)
	(t (cons (car x)
		 (every-other (cddr x))))))

(defun left-half (x)
  (left-half-aux x (ceiling (length x) 2)))

(defun left-half-aux (x n)
  (cond ((zerop n) nil)
	(t (cons (car x)
		 (left-half-aux (cdr x) (- n 1))))
	))

(defun merge-lists (x y)
  (cond ((null x) y)
	((null y) x)
	((< (car x) (car y)) (cons (car x)
				   (merge-lists (cdr x) y)))
	(t (cons (car y)
		 (merge-lists x (cdr y) )))
	))

(defun fact-debugger (n)
  (cond ((zerop n) (break "N is zero"))
	(t (* n (fact-debugger (- n 1))))))


(defvar family)

(setf family
      '((colin nil nil)
	(deirdre nil nil)
	(arthur nil nil)
	(kate nil nil)
	(frank nil nil)
	(linda nil nil)
	(suzanne colin deirdre)
	(bruce arthur kate)
	(charles arthur kate)
	(david arthur kate)
	(ellen arthur kate)
	(george frank linda)
	(hillary frank linda)
	(andre nil nil)
	(tamara bruce suzanne)
	(vincent bruce suzanne)
	(wanda nil nil)
	(ivan george ellen)
	(julie george ellen)
	(marie george ellen)
	(nigel andre hillary)
	(frederick nil tamara)
	(zelda vincent wanda)
	(joshua ivan wanda)
	(quentin nil nil)
	(robert quentin julie)
	(olivia nigel marie)
	(peter nigel marie)
	(erica nil nil)
	(yvette robert zelda)
	(diana peter erica)))


(defun father (x)
  (cadr (assoc x family)))

(defun mother (x)
  (caddr (assoc x family)))

(defun parents (x)
  (union (and (father x) (list (father x)))
	 (and (mother x) (list (mother x)))))

(defun children (person)
  (children-aux person family))

(defun children-aux (person tmp-family)
  (cond ((null tmp-family) nil)
	((member person (cdar tmp-family))
	 (cons (caar tmp-family)
	       (children-aux person (cdr tmp-family))))
	(t (children-aux person (cdr tmp-family)))))

(defun siblings (person)
  (set-difference (union (children (father person))
			 (children (mother person)))
		  (list person)))

(defun mapunion (f mylist)
  (and mylist (reduce #'union (mapcar f mylist))))

(defun grandparents (person)
  (mapunion #'parents (parents person)))

(defun cousins (person)
  (mapunion #'children
	    (mapunion #'siblings
		      (parents person))))

(defun descended-from-aux (x myparents)
  (cond ((null myparents) nil)
	((member x myparents) t)
	(t (descended-from-aux x (mapunion #'parents myparents)))))

(defun descended-from (x y)
  (descended-from-aux y (parents x)))

(defun descended-from2 (x y)
  (cond ((null x) nil)
	((member y (parents x)) t)
	(t (or (descended-from2 (father x) y)
	       (descended-from2 (mother x) y)))))

(defun ancestors (person)
  (cond ((null person) nil)
	(t (union (parents person)
		  (union (ancestors (father person))
			 (ancestors (mother person)))))))

(defun generation-gap (x y)
  (cond ((null x) 0)
	((member y (parents x)) 1)
	(t (max (+ 1 (generation-gap (father x) y))
		(+ 1 (generation-gap (mother x) y))))
	))

(defun generation-gap-aux (x y n)
  (cond ((null x) nil)
	((equal x y) n)
	(t (or (generation-gap-aux (father x) y (+ n 1))
	       (generation-gap-aux (mother x) y (+ n 1))))))

(defun generation-gap (x y)
  (generation-gap-aux x y 0))

(descended-from 'robert 'deirdre)

(ancestors 'yvette)

(generation-gap 'olivia 'frank)

(cousins 'peter)

(grandparents 'olivia)

(defun tr-count-up-aux (n mylist)
  (cond ((zerop n) mylist)
	(t (tr-count-up-aux (- n 1) (cons n mylist)))))

(defun tr-count-up (n)
  (tr-count-up-aux n '()))


(defun fact-aux (n result)
  (cond ((zerop n) result)
	(t (fact-aux (- n 1) (* n result)))
    ))

(defun tr-fact (n)
  (fact-aux n 1))

(defun tr-union-aux (x y result)
  (cond ((null x) (append y result))
	((not (member (car x) y))
	 (tr-union-aux (cdr x) y (cons (car x)
				       result)))
	(t (tr-union-aux (cdr x) y result))))

(defun tr-union (x y)
  (tr-union-aux x y '()))

(defun tr-intersection-aux (x y result)
  (cond ((null x) result)
	((member (car x) y)
	 (tr-intersection-aux (cdr x) y
			      (cons (car x) result)))
	(t (tr-intersection-aux (cdr x) y result))))

(defun tr-intersection (x y)
  (tr-intersection-aux x y '()))


(defun tr-set-difference-aux (x y result)
  (cond ((null x) result)
	((member (car x) y)
	 (tr-set-difference-aux (cdr x) y result))
	(t (tr-set-difference-aux (cdr x) y
				  (cons (car x) result)))))

(defun tree-find-if (f x)
  (cond ((and x (atom x) (funcall f x)) x)
	((atom x) nil)
	(t (or (tree-find-if f (car x))
	       (tree-find-if f (cdr x))))))

(defun lb-tr-count-slices (loaf)
  (labels ((lb-tr-cs1 (loaf-int n)
	     (cond ((null loaf-int) n)
		   (t (lb-tr-cs1 (cdr loaf-int) (+ n 1))))))
    (lb-tr-cs1 loaf 0)))


(defun lb-tr-reverse (tree)
  (labels ((aux-reverse-int (tree-int reverted)
	     (cond ((null tree-int) reverted)
		   (t (aux-reverse-int (cdr tree-int)
				       (cons (car tree-int) reverted))))))
    (aux-reverse-int tree '())))

(defun arith-eval (expre)
  (cond ((numberp expre) expre)
	(t (funcall (cadr expre)
		    (arith-eval (car expre))
		    (arith-eval (caddr expre))))))

(defun legalp (expre)
  (cond ((numberp expre) t)
	(t (and (member (cadr expre) '(+ - * /))
		(legalp (car expre))
		(legalp (caddr expre))))))

(defun factor-tree (mynumber)
  (labels ((factor-tree-aux (n p)
	     (cond ((equal n p) p)
		   ((zerop (rem n p))
		    (list n p (factor-tree-help (/ n p) p)))
		   (t (factor-tree-help n (+ p 1))))))
    (factor-tree-aux mynumber 2)))
