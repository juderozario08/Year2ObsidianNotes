Motivation:
- Remember that in a queue we have first-in first out
- In a priority queue, dequeueing an item depends on its priority.
- Therefore, highest priority items are dequeued first
- How can we implement this?
	- Using a list and sort functions is not ideal; sorting a list is O(n log n)
	- We can do better if we use a tree structure called binary heap, allowing us do enqueue and dequeue elements in
	- We don’t need to define a custom node structure and we get to any element in O(1)!
- Properties:
	- Structure: it is a complete binary tree
	- Order: in a min-head (max-heap), for every node x with parent p, the key in p is smaller (greater) than or equal to the key in x
```lisp
(defun hparent (i) (floor (- i 1) 2)); function for getting the parent location i-1//2
(defun hrt (i) (* (+ i 1) 2)) ; i+1 * 2
(defun hlt (i) (- (hrt i) 1)) ; hrt-1
(defun heap-push (node vec)
	(vector-push-extend node vec) ; inserts item at fill-pointer position (end of the array)
	(head-up vec (1- (length vec))))
(defun heap-up (vec i)
	(when (and (> i 0) (> (aref vec i) (aref vec (hparent i))))
		(rotatef (aref vec i) (aref vec (hparent i))) ; Swaps child with parent
	)
)
```
- **Operations on Heaps: Delete Max**
	- Remove tree root
	- Replace it with the last item in the heap array
	- Percolate that item down until finding its correct position
```lisp
(defun heap-down (vec beg &optional (end (length vec)))
	(let ((l (hlt beg))
		  (r (hrt beg)))
		(when (< l end)
			(let ((child))
				(if (or (>= r end)
						(> (aref vec l) (aref vec r)))
					l
					r)
			)
		)
		(when (> (aref vec child) (aref vec beg))
			(rotatef (aref vec beg) (aref vec child))
			(head-down vec child end)	
		)
	)
	vec)
(defun heap-pop (vec)
	(rotatef (aref vec 0) (aref vec (- (length vec) 1)))
	(prog1 ; this returns the result of the first form in its body
		(vector-pop vec) ; pops the item at :fill-pointer and decrements :fill-pointer
		(heap-down vec 0))
)
```