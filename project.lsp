;computes the sum of two numbers

(defun sum (x y)
	(+ x y)
)

;counts the number of atoms in list L

(defun count_atoms (L)
	(cond ((null L) 0)				;if L is empty 0 atoms
		((atom (car L)) (+ 1 (count_atoms (cdr L))))	
									;if first element of is atom then count
									;atoms in cdr of L and add 1
		(T (count_atoms (cdr L)))	;else count atoms in cdr of L
	)
)

(defun f1 (L)
	(- (length L) (count_atoms L))
) 

(defun f2 (L)
	(cond ((null L) nil)		;returns nil if no atoms present
		((atom (car L)) T)		;return true if car L is an atom
		(T (f2 (cdr L)))		;else check the next element of the list
	)
)

;modulus function that returns x mod y
(defun modulus (x y)
	(cond ((or (< x 0) (<= y 0)) -1)	;invalid inputs
		((>= x y) (modulus (- x y) y))	;check if x > y, if so recurse
										;and check x - y
		(T x)							;x is value of x % y
	)
) 

(defun f3 (L)
	(cond ((null L) nil)	;if L is empty return nil
		((= (mod (car L) 2) 1) (append (list (car L)) (f3 (cdr L))))
							;if car L is even, add car L to new list
							;check next element in the list
		((= (mod (car L) 2) 0) (f3 (cdr L)))
							;if car L is even, check next element in the list
	)
)

(defun f4 (L)
	(cond ((null (cdr L)) (car L))
		(T (f4 (cdr L)))
	) 
)

(defun f5 (L)
	(cond ((null (cdr L)) (list (car L)))
		(T (append (f5 (cdr L)) (list (car L))))
	)
)

(defun f6 (L)
	(cond ((null (car L)) nil)
		((listp (car L)) (car L))
		(T (f6 (cdr L)))
	)
)

(defun f7 (L)
	(cond ((null (car L)) nil)
		((listp (car L)) (append (list (car L)) (f7 (cdr L))))
		(T (f7 (cdr L)))
	)
)

(defun f8 (L)
	(cond ((null (car L)) 1)
		((numberp (car L)) (* (car L) (f8 (cdr L))))
		((listp (car L)) (* (f8 (car L)) (f8 (cdr L))))
	)
)

;decides whether x is a member of L

(defun my_member (x L)
	(cond ((null L) nil)
		((equal x (car L)) T)
		(T (my_member x (cdr L)))
	)
)

(defun f9 (L)
	(cond ((null (cdr L)) L)
		((my_member (car L) (cdr L)) (f9 (cdr L)))
		(T (append (list (car L)) (f9 (cdr L))))
	)
)

(defun f10 (L M)
	(cond ((null (car L)) nil)
		((my_member (car L) M) (append (list (car L)) (f10 (cdr L) M)))
		(T (f10 (cdr L) M))
	)
)