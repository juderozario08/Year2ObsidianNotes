😳```lisp
(/ (- 2 (- 3 (+ 6 (/ 4 5)))) (* 3 (- 6 2)))
```
This expression first does the smallest bracket and then goes up slowly based on the order.

#sqrt
```lisp
sqrt 4
```
This works like the square root. Takes only one number

#quote
```lisp
(quote(+ 2 3))
'(+ 2 3)
```
Quote v also written as 'v where v is any object just returns the object.

#if
```lisp 
; Test From, Then Form, Else form
; First, we check if the test form (nil) is true. Since this is not true, we just return the else form's evaluation. 
(if nil 4 (* 3 2))
```
This would return 6. 

#lambda
```lisp
(lambda (x) (* x x))
; lambda first with the parameters
; Then we write the evaluation that the computer is expected to do
; Then we just pass in the 2 or more variables that the user may input
((lambda (x y) (+ (* x y) x)) 3 1)
```
Steps in the example: 
x * y -> result + x
x = 3, y = 1
Lambda then calls the function, takes the inputs and then gives the answer. 

#let
```lisp 
; Naming a variable
; Assigns variable parallely so can't refer to a previous variable
; Unless the assigning is complete 
(let (x 4)
	 (* x x))
```
The *Let* statement lets the user set a variable to a certain expression or number. 

#and_or
```lisp
; And and Or are evaluated from left to right
(and 1 2 3)
(or nil 4 10)
```
The and and or statement can have unlimited inputs and a break condition would be the only thing that will get the code return a value when working with default values. 

#let_multiple
```lisp
; Usually just used for creating multiple variables in one line where one variable can use the value of the previous one assigned.
; Assigns variable sequentially, so assigns one line at a time
; e1 and x1 are evaluated before moving to e2 and x2.
(let* (a 2) 
	  (b 4) 
	  (c -1) 
	  (delta (- (* b b) (* 4 a c))) 
	  (res (sqrt delta))
	  )
(/ (+ (- b) res) (* 2 a))
```
This one names all the variables and evaluates them as the variables are being introduced. However, if we did this with a regular *let* statement. It will produce an error. Also *let* statements have their own scope which does not let them change the value of the variable if previously mentioned. It only creates a temporary copy within the scope of let. 

#defun
```lisp
; Format for creating a function
(defun average (x y)
	(/ (+ x y) 2.0))
average ((* 2 2) (/ 10 2)) -> 4.5
```

#floor
```lisp
; This does floor division by 10
; Cases work like switch statements, and each time, it checks whether
; variable is = to the number mentioned, and if it is,
; then return the character related to it.
; otherwise is the default/the last case scenario when nothing else matches,
; then return the last condition
(case (floor variable 10)
	(10, "A")
	(9, "A")
	(8, "B")
	(7, "C")
	(6, "D")
	(otherwise "F")
)

```
This one teaches how floor works and how switch cases work in lisp.

#ash
```lisp
; Shifts bits to the left or right based on the number given as an argument, same as floor division by 2
(ash 5 -1)
(ash 6 1)
```
So in this case, 101 changes to 010 which is = to 2. When we do plus 1, it shifts one bit to the left, then it multiplies by 2.

#defvar
```lisp
; This creates a global variable lowerb and capitalizes and stores 1 in it
(defvar *lowerb* 1)
(defvar *uppperb* 100)
```
This variable is available for all functions and values within the file

#setf 
```lisp
; This changes the upperb value to 10 permanently instead of a temporary change within the scope
(setf *upperb* 10)
```
This changes the value of upperb permanently within the file. ":=" this is a synonym for setf.

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
