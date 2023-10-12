# Analysis of Algorithms
- Use **Big-O** notation to describe execution time
- As long as the operations in *form* do not involve loops or recursive functions, running time is always constant. *O(1)*
- The running time of variable initialization, denoted by *T(n) = T(varInit) + T(form)*

# O(T(n)) and T(n): Loops
(DOTIMES VarTestRes bodyForm*) or (DO VarTestRes bodyForm*)
As long as the variable initializations, variable update operations, test, and result form computation in VarTestRes do not involve loops or recursive functions:
- We will ignore the time taken by DO and DOTIMES to carry out those operations
- We will only consider the bodyForm*
![[Pasted image 20231011200126.png]]
![[Pasted image 20231011211411.png]]
```lisp
; This function is called inside g(n)
(defun f (a b)
	(let ((acc 0))
		(dotimes (i a)
			(incf acc)
		)
	)
)
(defun g(n)
	(dotimes (i n)
		(dotimes (j n (f i j))
			(setf x (+ i j))
		)
	)
)
```
**Step by step process of calculating the Big-O notation**
```lisp
(dotimes i n) ; This does Sigma i = 0 -> (n-1) so the runtime is n.
(dotimes (j n (f i j)) ; This does Sigma i = 0 -> (n-1) so the runtime for this is also n as there is only one step. 
; But the return here calls the other function f and passes i as input.
(defun f (a b)
	(dotimes (i a)
; So f is increasing everytime i is increasing so this becomes Sigma i = 0 -> n-1 where i number of steps is being added each time making it ∑i. 
; So the final equation becomes n^2 + n(n-1)/2. So final notation is O(n^2).
```
