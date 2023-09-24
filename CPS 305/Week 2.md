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
```
The *var* works like the i, the *result-form* is the return value after the loop ends. The *count-form* is like the < end_of_loop_number. *body-form* is where the conditions and the codes are written. If we do not put the *result-form* it will just return *nil*. Unless we write a return statement inside the body, then it will break the loop and print the thing that was returned. 

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





