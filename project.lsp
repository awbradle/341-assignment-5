;Andrew Bradley
;COSC 341
;Assignment 5


;counts the number of atoms in list L
;from Prof Maniccam's demonstration list of LISP functions

(defun count_atoms (L)
	(cond ((null L) 0)				;if L is empty 0 atoms
		((atom (car L)) (+ 1 (count_atoms (cdr L))))	
									;if first element of is atom then count
									;atoms in cdr of L and add 1
		(T (count_atoms (cdr L)))	;else count atoms in cdr of L
	)
)

;counts the number of lists in a list.
;number of lists = length L - count_atoms L
(defun f1 (L)
	(- (length L) (count_atoms L))
) 

;Returns whether a given list contains any atoms
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

;returns the odd integers in L
(defun f3 (L)
	;if L is empty, return nil
	(cond ((null L) nil)	;if L is empty return nil
		((= (modulus (car L) 2) 1) (append (list (car L)) (f3 (cdr L))))
							;if car L is even, add car L to new list
							;check next element in the list
		((= (modulus (car L) 2) 0) (f3 (cdr L)))
							;if car L is even, check next element in the list
	)
)

;returns last element of a list L
(defun f4 (L)
	;if cdr is empty return car L
	(cond ((null (cdr L)) (car L))
		;else car is not last element, recurse
		(T (f4 (cdr L)))
	) 
)

;reverses order of list L
(defun f5 (L)
	;case cdr L is empty, return car L
	(cond ((null (cdr L)) (list (car L)))
		;recurse putting car L at the end of the list
		(T (append (f5 (cdr L)) (list (car L))))
	)
)

;returns the first list in list L
(defun f6 (L)
	;if list contains no lists, return nil
	(cond ((null (car L)) nil)
		;if car L is a list, return car L
		((listp (car L)) (car L))
		;else check the next item
		(T (f6 (cdr L)))
	)
)

;return a list of all the lists in L
(defun f7 (L)
	;if L is empty, return nil
	(cond ((null (car L)) nil)
		;if car L is a list, return list of car L + recursion
		((listp (car L)) (append (list (car L)) (f7 (cdr L))))
		;else check next item in list
		(T (f7 (cdr L)))
	)
)

;returns the product of all of the integers everywhere in L
(defun f8 (L)
	(cond ((null (cdr L)) (car L))
		;if car L is a number, find product car L * recursion
		((numberp (car L)) (* (car L) (f8 (cdr L))))
		;if car L is a list, find product of list car L
		((listp (car L)) (* (f8 (car L)) (f8 (cdr L))))
	)
)

;decides whether x is a member of L
;from Prof Maniccam's demonstration list of LISP functions

(defun my_member (x L)
	(cond ((null L) nil)
		((equal x (car L)) T)
		(T (my_member x (cdr L)))
	)
)

;remove duplicates in a list
(defun f9 (L)
	;if cdr L is empty, return list L
	(cond ((null (cdr L)) L)
		;if car L is in cdr L, ignore duplicate
		((my_member (car L) (cdr L)) (f9 (cdr L)))
		;else put unique element into list
		(T (append (list (car L)) (f9 (cdr L))))
	)
)

;finds the intersection of lists L, M
(defun f10 (L M)
	;if L is empty return nill
	(cond ((null (car L)) nil)
		;if car L is in M, add to list
		((my_member (car L) M) (append (list (car L)) (f10 (cdr L) M)))
		;else check next element of L
		(T (f10 (cdr L) M))
	)
)

;returns T is x is prime, else returns nil
(defun f11 (x)
	;if x is less than two, we know it is not prime
	(cond ((< x 2) nil)
		;pass x to prime function
		(T (prime x x))
	)
)

;checks helps f11 by checking every factor from 2 to n-1
(defun prime (x y)
	;return true if x is 2 or no factors have been found
	(cond ((< y 3) T)
		;if a factor is found, return nil
		((= (modulus x (- y 1)) 0) nil)
		;if possible factor tried is not a factor, try next possible factor 
		(T (prime x (- y 1)))
	)
)

;inserts x into sorted list L.
(defun insert (x L)
	;if list is empty, return x
	(cond ((null L) (list x))
		;if x is less than first item of L, put x at the head of list
		((< x (car L)) (cons x L))
		;else check next value of L
		(T (cons (car L)(insert x (cdr L))))
	)
)

;calls insert and does an insertion sort on list L. Returns the sorted list
(defun f12 (L)
	;return list of 1 item or less as a list of that item
	(cond ((null (cdr L)) (list (car L))) 
		;sort the car of L into recursion of the sorted cdr
		(T (insert (car L) (f12 (cdr L))))
	)
)
 