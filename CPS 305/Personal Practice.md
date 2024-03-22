# Practice Structs
```lisp
(defstruct CPSCourse name professor test1 test2 assignment final)
(defstruct Student name studentID courses)

(defun tester ()
  (let* 
		((cps305 (make-CPSCourse :name "CPS305" :professor "Bahoo" :test1 25 :test2 25 :assignment 10 :final 40))
	    (student (make-Student :name "Jude Rozario" :studentID 501166063 :courses (list cps305))))
	    (values (Student-name student)
            (Student-studentID student)
            (Student-courses student)
            (CPSCourse-name cps305)
            (CPSCourse-professor cps305)
            (CPSCourse-test1 cps305)
            (CPSCourse-test2 cps305)
            (CPSCourse-assignment cps305)
            (CPSCourse-final cps305)
	    )
	)
)

; We can also pass an operator using #'operator
; Inside the function we just write funcall varname instead of the operator itself
```
# Practice Questions
```lisp 
; Function practice examples
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

(defun withdraw (amount)
; Check if balance >= amount
; If so then set balance to balance - amount
; Else print insufficient funds
; At the end just print the balance
	(if (>= balance amount)
		(setf balance (- balance amount))
		(print "Insufficient Funds")
	)
	balance
)

(defun vowelp (c)  
	(or 
		(char= c #\a) 
		(char= c #\e) 
		(char= c #\i) 
		(char= c #\o)
		(char= c #\u)
	)
)
(defun cpt-vowels (s) 
	(dotimes (i (length s) s)
		(when (vowelp (aref s i))  
			(setf (aref s i) (cpt-char (aref s i)))
		)
	)
)


```
# Quicksort
```lisp
(defun quickSortm (vec op)
  (quicksort-main vec op 0 (- (length vec) 1))
  vec
  )
(defun quickSort-main (vec op low high)
  (if (> high low)
      (let ((pivot (partition vec op low high)))
        (quicksort-main vec op low (- pivot 1))
        (quicksort-main vec op (+ pivot 1) high)
        )
      )
  )
(defun partition (vec op low high)
  (let ((pivot_value (elt vec high))
        (i low))
    (do ((j i (1+ j)))
        ((>= j high))
      (when (funcall op (elt vec j) pivot_value)
        (rotatef (elt vec j) (elt vec i))
        (setf i (1+ i))
        )
      )
    (rotatef (elt vec i) (elt vec high))
    (return-from partition i)
    )
  )
```

# Memorize this
```lisp
;;; How to flatten a list
(defun flat (a)
  (cond
    ((null a) a)
    ((listp (car a)) (append (flat (car a)) (cdr a)))
    (t (cons (car a) (flat (cdr a)))))
  )
```

```lisp
;;; How to count the number of times an item was repeated
(defun item-count (vec &optional (res (list)))
  (if (null vec) res
      (let ((item (find (car vec) res :key 'first)))
        (if item (incf (cadr item))
            (push (list (car vec) 1) res))
        (item-count (cdr vec) res)
        )
      )
  )
```