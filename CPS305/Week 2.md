   #cond
```lisp
(defun whereis (city)
	; cond allows automatic if and else without writing it
	; eq is to check if two things are equal
	(cond ((eq city 'toronto) 'canada)
		  ((eq city 'whatever) 'heaven)
		  ; checks if city is either dumbstuff or wtfiseventhat
		  ; then returns bruh
		  ((or (eq city 'dumbstuff ) (eq city 'wtfiseventhat)) 'bruh)
		  ; Last case scenario, print unknown
		  ; the T means that the condition is True
		  (t 'unknown')
	)
)
```
***cond*** allows the execution of *forms* to be dependent on *test-form*.
*eq*: checks if two things are equal to each other.
*form*: any object meant to be evaluated. 
*test-form*: an ordered set of adjacent forms appearing in another form, and defined by their context in that form to be executed as if within a progn.

#dotimes
```lisp
; For loop in lisp
(dotimes (var count-form [result-form])
	body-form*) ; returns the value of the result form or returns nil
; The secondary bracket inside is for all variables.
; Everything outside the secondary bracket inside is for body code
(let ((age 18))
	 (dotimes (i 3 age)
		 (setf age (+ age 1))
		 (format t "Age: ~a Loop: ~a~%" age i)
	 )
)

```
The *var* works like the i, the *result-form* is the return value after the loop ends. The *count-form* is like the < end_of_loop_number. *body-form* is where the conditions and the codes are written. If we do not put the *result-form* it will just return *nil*. Unless we write a return statement inside the body, then it will break the loop and print the thing that was returned. 

#do #dostar
```lisp
; each *var-definition* is a list (var init [step-form])
(do (var-definition*)
	(end-test result-form*)
	body-form*)

; Example
(do (
		 (temp1 1 (1+ temp1)) ; variable, initialize, step
		 (temp2 0 (1- temp2)) ; variable, initialize, step
	)
	((> (- temp1 temp2) 5) temp1) ; end-test condition, return value
	(format t "temp1: ~a temp2: ~a~%" temp1 temp2)
)
```
- First temp1 = 1 and temp2 = 0
- When temp1 - temp2 > 5, we return temp1
- Otherwise we do the step
- The function can also be written without a return value
```lisp
(do* 
	(
		(i 0 (1+ i))
		(s (read-line) (read-line))	; This one is saying initially its a read line but also the next step will be a read line
		(acc (length s) (+ acc (length s))) ; Add the lenght of the user input to acc
	)
	((equal s "exit")) ; Exit condition is s = 'exit'
	(format t "~a: ~a ~a~%" i s acc) ; Otherwise keep printing 
)
```
**Do** loops work until a condition is met and then it will exit. Kind of like **while false**. **Do**** works let* where it initiates the variables serially instead of parallel.

# Algorithm Analysis
- Comparison of programs depends on the criteria used for the comparison
	- readability
	- the algorithm itself
- In Algorithm Analysis, we focus on the algorithm
- In particular, we are concerned with comparing algorithms based upon the amount of **computing resources** that each algorithm uses.

# Computing Resources
- Two ways of looking at **computing resources** an algorithm requires to solve a problem
	- The amount of **memory**
	- The amount of **time**, usually refers as **execution time** or **running time**

# Big-O notation
- In general, the running time of an algorithm grows with the size of the input
- The notion for **input size** depends on the problem being studied
- **T(n)** represents the **running time** of an algorithm on an input of size *n*.
- The **Order of magnitude** function ***O(f(n))*** describes the part *f(n)* of *T(n)* that increases the fastest when n grows.
- When *n* gets larger, the term *n^2* becomes the most important
- Running time ***T(n)*** above has an order magnitude *O(n^2)*
# ------------Memorize the Order-------------
![[Pasted image 20231011193646.png]]
![[Pasted image 20231011193700.png]]

# Obtaining the order of Magnitude
- Identify which of ***T(n)*** increases its value fastest when n grows
- Remove the other terms

# Practice
```lisp
(defun vowelp (c)  
	(or 
		(char= c #\a) 
		(char= c #\e) 
		(char= c #\i) 
		(char= c #\o)
		(char= c #\u)
	)
)
(defun cpt-vowels (s) (dotimes (i (length s) s)
(when (vowelp (aref s i))  
(setf (aref s i) (cpt-char (aref s i))))))
```