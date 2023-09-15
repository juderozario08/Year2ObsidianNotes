```lisp
(/ (- 2 (- 3 (+ 6 (/ 4 5)))) (* 3 (- 6 2)))
```
This expression first does the smallest bracket and then going up slowly based on the order.

```lisp
sqrt 4
```
This works like the square root. Takes only one number

```lisp
(quote(+ 2 3))
'(+ 2 3)
```
Quote v also written as 'v where v is any object just returns the object.

```lisp 
;Test From, Then Form, Else form
;First we check if the test form (nil) is true. Since this is not true, we just return the else form's evaluation. 
(if nil 4 (* 3 2))
```
This would return 6. 


```lisp
(lambda (x) (* x x))
; lambda first with the parameters
; Then we write the evaluation that the computer is expected to do
; Then we just pass in the 2 or more vairables that the user may input
((lambda (x y) (+ (* x y) x)) 3 1)
```
Steps in the example: 
x * y -> result + x
x = 3, y = 1
Lambda then calls the function, takes the inputs and then gives the answer. 

```lisp 
; Naming a variable 
(let (x 4) (* x x))
```
The *Let* statement lets the user set a variable to a certain expression or number. 

```lisp
; And and Or are evaluated from left to right
(and 1 2 3)
(or nil 4 10)
```
The and and or statement can have unlimited inputs and a break condition would be the only thing that will get the code return a value when working with default values. 

```lisp
; Usually just used for creating multiple variables in one line
; e1 and x1 are evaluated before moving to e2 and x2.
(let* (a 2) 
	  (b 4) 
	  (c -1) 
	  (delta (- (* b b) (* 4 a c))) 
	  (res (sqrt delta))
	  )
(/ (+ (- b) res) (* 2 a))
```
This one names all the variables and evaluates them as the variables are being introduced. However if we did this with a regular *let* statement. It will produce an error. 

```lisp
; Format for creating a function
(defun average (x y)
	(/ (+ x y) 2.0))
average ((* 2 2) (/ 10 2)) -> 4.5
```


# Practice Examples
```lisp 
;Function practice examples

(defun cube (x) (* x x x))
(defun add1 (x) (+ x 1))
(defun add2 (x) (add1 (add1 x)))
(defun max2 (x y) if (> x y) x y)
(defun onemoreep (x y) (= x (add1 y)))

; Write a function that takes in a final score n. 0 <= n <= 100, and returns the respective grade number:

(defun convert-to-letter-grade(numeric-grade)
	(case (floor numeric-grade 10)
		(10 "A")
		(9 "A")
		(8 "B")
		(7 "C")
		(6 "D")
		(otherwise "F")
	)
)
```
