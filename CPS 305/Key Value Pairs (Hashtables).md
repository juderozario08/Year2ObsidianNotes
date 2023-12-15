- Also known as maps, dictionaries, associative arrays and tables.
- Main feature: efficient access to values via keys associated with them
- Applications:
	- Memoization (Caching and Optimization technique)
	- Dictionaries (natural language processing)
	- Internal Tables (programming language environment)

# Concrete key-value data structures
- **Arrays**
	- Limitations:
		- Keys must only be numbers
		- arrays have static nature (no resizing)
- **Associative Lists**
	- An a-list is a list of *cons pairs* (:foo -> **key** , "**bar**" -> **value**)
	- Creating a cons pair
```lisp
	(cons "foo" 'bar) -> ("foo" . BAR)
	(cons :foo "bar") -> (:foo . "bar")
```
- **A-list Interface**
	- look up an entry: **ASSOC**
		- note: by default, ASSOC uses EQ to compare keys
	- add an entry: **cons** or **push**
	- remove item: lisp's **remove** function
```lisp
;; This is to look up an entry
(setf words '((one . un) (two . deux) (three trois) (four . quatre) (five . cinq)))
(assoc 'four words)
(FOUR . QUATRE)

;; This is to add an entry
; using push
(push '(six . six) words)
; using cons
(setf words (cons (cons 'seven 'sept) words))

;; This is to remove an entry
(remove 'three words :key 'car)
```

- **Efficiency of a-lists**
	- lookup and remove operations search the list sequentially, hence are O(n)
	- add (via push or cons) is O(1)

- **Hash tables**
	- Offer the same functionality as an association list
		- a key is given and the table gives back the item associated with that key
	- very fast item look up and deletion compared to a-lists
	- implemented using arrays rather than lists
	- unlike a-lists, hash table elements are not visible to the other user at the REPl
	- **Interface**
		- Create hash-table: *MAKE-HASH-TABLE* by default eq is used to compare keys
		- Look up an item: *GETHASH* the key can be any object
		- Store an item: *SETF* and *GETHASH*
		- Remove a key: *REMHASH*
		- Inspect Table: *INSPECT*
	- **Iterating through a hash table**
```lisp
;;; This is how to setup a hash table
(defparameter h (make-hash-table))
(loop for (key value) in '((superman 1938)
						   (donald-duck 1934)
						   (batman 1939))
		do (setf (gethash key h) value))

;;; MAPHASH solution
(let ((min 2015)
	  (oldest nil))
	  (maphash (lambda (hero year) (when (< year min) (setf min year oldest hero))) h)
	oldest)
```

# Hash table motivation

- If key are integers, we can explore the efficiency of arrays and key can be index of an array O(1). (this is called direct addressing)
- Define a function h called the hash function, that is given an arbitrary key k, returns an integer h(k), such that: 
	- 0 <= h(k) < n
	- h(k) should be uniformly distributed in that interval
	- minimizes the number of collisions h(k1) != h(k2), k1 != k2
- We can say that **k hashes to slot h(k)**
- we also say that **h(k) is the hash value of k**
- **Collision resolution: chaining**
	- When two keys hash to the same integer, store both items in a list
	- **Drawback**:
		- mixing arrays and lists complicates the implementation
		- requires more memory than the underlying array
		- poor performance due to the linked structure
- **Collision resolution: open addressing with linear probing**
	- All keys are stored in the hash table itself (each table is either a key or nil)
	- h'(k) be an auxiliary hash function. The i-th probe position for key k in a hash table is given by the hash function. **h(k,i) = (h'(k) + i) mod M (size of the hash table)**
	- Inserting, searching and deleting a key K:
		- **Insert**: if K gets hashed into an already occupied slot s, put it in the next empty slot
		- **Search**: if k is not in the hashed slot s then look after and wrap around
		- **Delete**: search for the hashed slot s then mark the slot s as deleted
	- **Clustering**
		- A disadvantage of linear probing is the tendency of clustering
		- Both tables below are 50% full
		- insert and search are closer to O(n) than O(1) 
	- **Handling clustering: quadratic probing**
		- Let h'(k)=k mod M
		- In quadratic probing the i-th probe position for key k is given by the function 
			- **h(k,i) = (h'(k) + i + i^2) mod M**

# Implementation of a Hash Table
```lisp
(defstruct ht
	array ; Where the key-values are stored
	(count 0) ; The count of entries currently in the table
	comp) ; The relational operation used to compare the keys

(defparameter size 16)
(defun ht-create (kvs ; Key values 
				  &key (test 'eql))
	(let ((res (make-ht :array (make-array size :initial-element nil) :comp test)))
    (loop for (k v) in kv ;; loop through each kv paid and add it to the array
          do (ht-add k v res)))))
```
- **Getting key from a hash table**
	- This is an implementation of open addressing with linear probing
	- If the value associated with the key is **NIL** or **count is equal to the size of the array**, then ht-get returns NIL
	- If the key is found, then its associated value and its position in the array are returned.
```lisp
(defun ht-get (key ht)
  (let* ((size (length (ht-array ht)))
;; Hash the key then get the remainder based on the size
;; start = gethash(key)%size
         (start (rem (sxhash key) size)))
    (do* ((count 0 (1+ count)) ;; count is the offset from the hashed position
;; count + start ensures that we are ready to go by an offset from the
;; initial hashed value in case there is a collision
;; (start+offset)%size is responsible for looping the index so we don't go out of range
          (i start (rem (+ start count) size))
          ;; ^ is Linear Probing or v Quadratic Probing
          (i start (rem (+ start count (* count count)) size)) 
;; item is the current key.value pair I am looking at
          (item (aref (ht-array ht) start) (aref (ht-array ht) i)))
;; if item is empty or count = size exit
         ((or (null item) (= count size))) 
;; When key (default =) current key, return value
      (when (funcall (ht-comp ht) key (car item)) 
        (return (values (cdr item) i))))))
```
- Sets the value of a certain key to the given value
```lisp
(defun ht-set (k v ht)
  (multiple-value-bind (value index) (ht-get k ht) ;; Get value and index from the get function
    (when value ; if the value exists
      (setf (cdr (aref (ht-array ht) index)) v)))) ; set the value to the new value
```
- Adding a key to a hash table
```lisp
(defun ht-add (key val ht)
  (let* ((temp (ht-array ht)) ;; Create a temp array to store the hashmap
         (size (length temp))) ;; Store the size of the temp as well
    ; FLET defines function add-item locally (function within a function)
    (flet ((add-item (k v) ; In other words, function let
             (do* ((count 0 (1+ count)) 
                  (start (rem (sxhash k) size))
                  (i start (rem (+ start count) size)))
                 ((null (aref (ht-array ht) i)) ; loop stop test
                  (setf (aref (ht-array ht) i) (cons k v)))))) ; result form
      (when (= (ht-count ht) size)
                                        ; when the backing array is full
                                        ; expand it to have the length equal to the next power of 2
        (setf size (expt 2 (ceiling (log (1+ (ht-count ht)) 2)))
              (ht-array ht) (make-array size :initial-element nil))
        ;; and re-add its contents
        (dotimes (i size)
          (let ((item (aref temp i)))
            (add-item (car item) (cdr item)))))
                                        ; Adds the new item
      (incf (ht-count ht))
      (add-item key val))))
```
- Iterating over all the entries in a hash table
	- Hash-tables are unordered collections of items
	- There are times where we might want to walk a hash table in a specific order
```lisp
(defun ht-print(ht)
	(let* ((kvs (ht-array ht))
		   (size (length kvs)))
		(dotimes (i size)
			(let ((kv (aref kvs i)))
				(when kv (format t "Key: ~s ~a Value: ~s~%" (car kv) #\tab (cdr kv)))
			)
		)
	)
)
```
- We can also use lisps maphash function to map a function to the entire hashtable
```lisp
; Implementing the map hash function
(defun ht-map (f ht)
	(let* ((kvs (ht-array ht)))
		   (size (length kvs)))
		(dotimes (i size)
			(let ((kv (aref kvs i)))
				(when kv (funcall f (car kv) (cdr kv)))
			)
		)
	)
```
- This way our ht-print function can just become this
```lisp
(defun ht-print(ht)
	(ht-map #'(lambda (k v) (print (list k v))) ht)
```
# Practice Programs
```lisp
(defparameter h (make-hash-table))
(defun epoch (b e)
	(loop for (key value) in '((superman 1938)
							   (donald-duck 1934)
							   (batman 1939))
		do (setf (gethash key h) value))
	(let ((res ()))
		(maphash (lambda (hero year) 
					(when (and (<= year e) (>= year b))
						(push (cons hero year) res))) h) res))
```
