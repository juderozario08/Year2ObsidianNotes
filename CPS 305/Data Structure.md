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
- **1 -> Constant**
- **log n -> Logarithmic**
- **n -> Linear**
- **n log n -> Log linear**
- **n^2 -> Quadratic**
- **n^3 -> Cubic**
- **2^n** 
# Obtaining the order of Magnitude
- Identify which of ***T(n)*** increases its value fastest when n grows
- Remove the other terms

# Analysis of Algorithms
- Use **Big-O** notation to describe execution time
- As long as the operations in *form* do not involve loops or recursive functions, running time is always constant. *O(1)*
- The running time of variable initialization, denoted by *T(n) = T(varInit) + T(form)*

# O(T(n)) and T(n): Loops
(DOTIMES VarTestRes bodyForm*) or (DO VarTestRes bodyForm*)
As long as the variable initializations, variable update operations, test, and result form computation in VarTestRes do not involve loops or recursive functions:
- We will ignore the time taken by DO and DOTIMES to carry out those operations
- We will only consider the bodyForm*
```lisp
(dotimes (i n) ; Repeat n times
	(:= x (+ i b)) ; Step 1
	(:= y (* x 2)) ; Step 2 
)
; By the end it perfoms 2 steps n number of times making the algorithm run 2n times.
; So, Big-O notation is O(n)

(dotimes (i n)
	(:= x (+ i b))
	(dotimes (j n)
		(:= y (+ a b))
	)
)
; For the first forloop we have ∑(1 + ∑1)
; The second ∑1 is for the second for loop
; This gives n + n^2
; This makes the Big-O notation O(n^2)
```

```lisp
(defun test ()
	(:= k 0)
	(do ((i 1 (* i 2))) 
		((>= i n))
		(do something)
	)
)
; The (do loop) steps like i = i * 2 Which is the same as 2 being multiplied over and over again. Since i starts from 1.
; Then we can say i = n and p = log n (log base 2)
; So (p 0 (1+ p)) then (p >= log n 2) means that we have found what exponent gives n and then exit.
; So that means this function runs 1 + log(n) therefore making O(log n)
```

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

# The von Neumann's Bottleneck
- CPU only has access to data stored in registers
	- **numbers**
	- **pointers** - the number of bits stored in a register defines the max memory address
- But the bulk of the data is stored in **memory**
**Two Ways to store data in memory**
- **Contiguous Structure**: Occupies a single chunk of memory and its content are stored in adjacent memory. Ex- Arrays and Structs
- *Enables efficient data access*
- *Takes constant time to access an element (address of the first )*
![[Pasted image 20231012191341.png]]
- **Linked structure**: Doesn't occupy a contiguous block of memory. Ex- lists, trees, graphs.
- *Data access is less efficient*
- *Cost(in time) depends on size of the structure*
![[Pasted image 20231012192028.png]]

# Contiguous structures: The tuple abstract date struct
- All programming languages use *abstractions* called **record** and **object**.
- In lisp, abstraction is called a **structure**. 
- **Structure**: A structured data type with named slots, each holding a primitive object or another structure
- In lisp, we define it using **defstruct**. Example: Tuple/structure representing a movie
```lisp
(defstruct movie
	title director year type)
```
- Once a struct is defined, Lisp automatically creates these:
	- A constructor named MAKE-structure_name
	- accessors, named structure_name-slot_name
```lisp
; That's how you define a struct
; (defstruct object-name variables ........)
(defstruct movie title director year type)
(defun tester ()
	  (let
		; Set the movie title and director using the constructor
	    ((my_movie (make-movie :title "Blade Runner" :director "Ridley Scott")))
	    ; In the body
	    ; Set the values for movie year
	    (setf (movie-year my_movie) 1982)
	    ; Reminder that the body does not have to be inside another bracket
	    ; This returns all the values together
	    ; Typing format (structName-accessorName varName)
	    (values (movie-title my_movie)
            (movie-director my_movie)
            (movie-year my_movie)
        )
    )
)
```

# Arrays
- An array of size n will have indices from 0 - (n-1).
- Most basic data structure and favoured choice for implementing algorithms.
```lisp
initialize an array (3 4 5 6)
(make-array 3) -> (0 0 0)
(make-array 3 :initial-element 1) -> (1 1 1)

(defstruct movie title director year type) ; Create the struct for the movie
(defparameter *size* 10) ; Create a parameter for the list
(defvar *db*) ; Create the global var for the list
(setf *db* (make-array *size* :initial-element nil)) ; Set the list to initial values
(defun add-movie (m) 
	(dotimes (i *size*) ; Create a forloop
		(unless (aref *db* i) ; if db[i] does not exist set db[i] = m and return t
		(setf (aref *db* i) m) ; 
		(return t))
	)
)
```

# Lists
- Lists are a central data structure in lisp
- They are much more flexible than arrays, and allow the programmer to represent any abstract type: sets, tables, graphs, and even english sentences.
```lisp
(43 12 45 3 -1)
(RED GREEN BLUE)
(3 FRENCH HENS 2 TURTLE DOVES 1 PARTRIDGE 1 PEAR TREE)
(defun sum-n1 (n)
	(do ((i 1 (1+ i))
		(sum 0 (+ i sum)))
		((> i n) sum)
	)
)
```
- **Internal Representation (cons cell)**:
- (RED GREEN BLUE)
![[Pasted image 20231013191004.png]]
- Notice the cons cell always ends in NIL.
- (AARDVARK)
![[Pasted image 20231013191119.png]]
- ((BLUE SKY) (GREEN GRASS) (BROWN EARTH))
![[Pasted image 20231013191145.png]]
```lisp
(length '(RED GREEN BLUE))  => 3
(length '((BLUE SKY) (GREEN GRASS) (BROWN EARTH)))  =>  3
; The reason its 3 is because their internal representation contain only 3 top-level cons cells
```

# NIL: The empty list
- A list of zero elements can be written as either () or NIL
- (length NIL) -> 0
- (length ()) -> 0

# Selectors: First, Second, Third, Rest
- Lisp's primitive function for extracting elements from a list
```lisp
(first '(a b c d)) -> a
(second '(a b c d)) -> b
(third '(a b c d)) -> c
```
- REST returns a list containing everything but the first element
```lisp
(rest '(a b c d)) -> (b c d)
(rest (rest '(a b c d))) -> (c d)
```

# Functions operate on pointers
- We don't send the whole list to the called function
- We send just a reference to the first cons cell
![[Pasted image 20231013192129.png]]

# CAR and CDR
- The two halves of a cons cell has obscure names
	- left half is called CAR - Contents of Address portion of Register (node)
	- right half is called CDR - Contents of Decrement portion of Register (next)
- CAR and CDR are also names of built-in lisp functions
	- CAR is the same as first
	- CDR is the same as rest
- Built-in combinations of CAR and CDR
	- Read the combinations from right to left

# The list CONStructor
- CONS creates a cons cell
	- takes two parameters
	- returns a pointer to a new cons cell whose CAR points to the first parameter and whose CDR points to the second
![[Pasted image 20231013212434.png]]
Creating a list using recursion
```lisp
(defun mymake-list-rec (n element &optional (acc nil)) 
	(if (= n 0) acc
		(mymake-list-rec (1- n) element (cons element acc)) ; Adds element to the beginning recursively
	)
)

(defun mymake-list-it (n elem) 
	(let ((acc nil))
		(dotimes (i n acc)  
			(setf acc (cons elem acc)) ; Adds an element to 
		)
	)
)
```

# Programmer-defined Linked Lists
- Structure
	- node tuple stores information about a data item in the list and the next item in the list. Consists for 2 slots:
		- *data*: a reference to the data item
		- *next*: a reference to the next item
		- *head*: a pointer to the node at the head of the list
		- *size*: the length of the list (CAVEAT LECTOR: keeping track of size increases storage requirement)
```lisp
(defstruct node 
	data next)

(defstruct my-list 
	(head nil :type (or node null)) ; Can be of type node or null
	(size 0 :type (integer 0)) ; Has to be an integer > 0
)

(defun is-empty (list)
	(equalp list (make-my-list)) ; Equalp compares structures
)

(defun my-car (alist)
	(node-data (my-list-head alist))
)

(defun my-cdr (alist)
	(if (is-empty alist) alist
		(make-my-list :head :size)
	)
)
```

# Come back to this part for linked lists (Week 4 Lecture 2 and 3)

# Queues
- Simplest way for implementing scheduling mechanisms
- **First in first out (FIFO)**
	- printing queues
	- OS control processes
	- Customer service lines
```lisp
add the code here
```

# Stack
- Provides the simples way of saving information in a temporary storage location
- **Last in First Out (LIFO)**
- Applications
	- backward navigation in web browsers
	- mathematical expression evaluation
	- management of recursive subroutine calling
	- undo/redo operations in text editors
- Very simple to implement using a list
- Interface: push, pop, peek, is_empty
```lisp
add code here
```

# Double Ended Queue (Dequeue)
- A hybrid linear structure that provides all the capabilities of stacks and queues in a single data structure.
- It can be traversed using FIFO(Queue) or LIFO(Stack)
- 4 operations: PUSH-FRONT, PUSH-BACK, POP-FRONT, POP-BACK
- Applications
	- Main worker is processing items from the front
	- while other workers may steal the lowest-priority items from the back
```lisp
add the code here
```

# Sets
- A set is an unordered collection of items
- Each item appears only once inside a set
- Applications:
	- To track items we have already seen and processed
	- When calculating some relations between groups of elements
- The set interface:
	- ELEMENT-OF-SET? checks whether an item is in the set 
	- INSERT-SET/REMOVE-SET: adds/removes an item
	- SUBSET-SET checks whether a set is a subset of another set

# Create all the helper functions- (Lisp has a built in member function)
- Run time of element-of-set:
	- May scan the entire set (worst case scenario)
	- Set has n elements, so it might take up to n steps
	- Hence the order of growth of the run time is O(n)
```lisp
; Insert all the code here
```