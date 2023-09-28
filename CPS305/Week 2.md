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





