# Trees
- It is a data structure used in many areas of computer science
	- Programming languages and compilers
		- the source code we and the compiler operate with
		- the class structure in OOP
	- **Operating Systems** - the file directory structure
	- **Algorithms** - search trees
	- **Gaming** - game trees - keep possible next moves in a tree
	- **Data Compression** - Huffman coding
- **Vocabulary and tree properties**
	- **Vocabulary**: Node, Edge, Root, Path, Children, Parent, Sibling, Level and Height
	- **Definition**: A tree consists of a list of **nodes** connected by **edges**. A tree has the following properties:
		- One node of the tree is designated as the **root** node
		- Every node n, except the root node, is connected by an edge from exactly one other node p, where p is the **parent** of n.
		- A unique **path** traverses from the root to each node
		- If each node in the tree has a maximum of **two children**, we say that the tree is a **binary tree**
		- A tree is either empty or consists of a root and zero or more sub-trees, each of which is also a tree
# Implementing trees: representation
- To implement trees, we first have to decide on the data structure we will use
- We will start with a simple binary tree representation
```lisp
(defstruct (tree-node (:conc-name nil)) ; Automatic prefixing of accessors
		   key ; The root node of the branch, where data about the node's key is stored
		   (left-child nil) ; Left child branch
		   (right-child nil)) ; Right Child Branch
```
![[Pasted image 20231210124806.png]]
```lisp
(make-tree-node :key 'a
	:lc (make-tree-node :key 'b)
	:rc (make-tree-node :key 'c
						:lc (make-tree-node :key 'd)
						:rc (make-tree-node :key 'e)))
```
# Operations on a tree
- Although a tree sometimes implies a notion of ordering among the values in its nodes, the functions we will write do not take into account such ordering. They are just to help us manipulate the data structure.
- **Creating a tree node**: Since we defined the data structure using **DEFSTRUCT**, we get the **MAKE-TREE-NODE** constructor
```lisp
(make-tree-node :key 5)
```
- **Modifying the root value (destructively)**
```lisp
(defun set-root (node item)
	(setf (key node) item))
```
# Inserting a node on a tree
![[Pasted image 20231210133833.png]]
```lisp
(defun insert-left (node item)
	(if (null (lc node))
		(setf (lc node) (make-tree-node :key item))
		(setf (lc node) (make-tree-node :key item :lc (lc node))))
)
(defun insert-right (node item)
	(if (null (rc node))
		(setf (rc node) (make-tree-node :key item))
		(setf (rc node) (make-tree-node :key item :rc (rc node))))
)
```
# Representing any tree (binary, ternary, Quaternary, ...)
- It uses the concept of children which is simply a list of tree nodes
```lisp
(defstruct (tree-node (:conc-name nil)) ; Automatic prefixing of names and accessors
		    key ; the root node of the tree branch
		    children ; a list of children nodes
)
```
- Searching an item in a tree using the **SEARCH-TREE** function
```lisp
;;; Tail Recursion
(defun search-tree (e node &optional (found nil))
	(cond ((null node) found)
		  (found e)
		  ((listp node) (search-tree e (cdr node) (search-tree e (car node))))
		  ((equal e (key node)) e)
		  (t (search-tree e (children node))))
)
;;; Using do-list
(defun search-tree (e node)
	(when node
		(if (equal e (key node)) e
			(dolist (b (children node))
				(let ((res (search-tree e b)))
					(when res (return res))))))
)
```
- **REPLACE-ROOT** function replaces in the tree all key values that match with the new given value
```lisp
(defun replace-root (value newvalue node)
  (labels ((replace-in-children (value newvalue c)
             (if (null c) nil
                 (cons (replace-root value newvalue (car c))
                       (replace-in-children value newvalue (cdr c))))))
    (if (equal value (key node))
        (make-tree-node :key newvalue
                        :children (replace-in-children value newvalue (children node)))
        (make-tree-node :key (key node)
                        :children (replace-in-children value newvalue (children node)))))                      
  )   
```

# Tree Traversal
- **Depth-first search**
	- Pre-order traversal is root->left->right : Used for creating the copy of a tree
	- Post-order traversal is left->right->root : Used to delete a tree from leaf to root
	- In-order traversal is left->root->right : Binary search tree traversal, visiting in an increasing order
- **Breadth-first search**

# DFS
- DFS pre-order algorithm
	- if current node is empty then return
	- execute the 3 operations
		- visit current node
		- recursively traverse current nodes left subtree
		- recursively traverse current nodes right subtree
```lisp
(defun pre-dfs (fn root)
	(when root ; If root exists
		(funcall fn (key root)) ; Mark the root as visited
		(dolist (child (children root))
			(pre-dfs fn child)))
)
(pre-dfs 'princ tree)
```
- DFS post-order algorithm
	- if current node is empty return 
	- execute the following 3 operations
		- recursively traverse the current nodes left subtree
		- recursively traverse the current nodes right subtree
		- visit the current node
```lisp
(defun post-dfs (fn root)
	(when root
		(dolist (child (children root))
			(post-dfs fn child))
		(funcall fn (key root)))
)
(post-dfs 'princ tree)
```
# BFS
- We visit every node on every level before going to a lower level
```lisp
(defun bfs (fn nodes)
	(let ((next-lvl ())) ; This is the list that stores the information for the next level
		(dolist (node (rtl:mklist nodes)) ; mklist : (if (listp nodes) nodes (list nodes))
			(funcall fn (key node)) ; Print the current node
			(dolist (child (children node)) 
				(push child next-lvl))); push all the children of the current node to the stack
		(when next-lvl ; NOTE: REVERSING A STACK TURNS IT INTO A QUEUE
			(bfs fn (reverse next-lvl)))  ; recursively call bfs on the queue
	)
)
(bfs 'princ tree)
```
- Problem: What kind of traversal algorithm needed to create the count-elem function
	- Idea is to traverse to max depth first, then start counting from there
	- So DFS
```lisp
(defun count-elem (node)
	(labels ((count-aux (c)
				(if (null c) 0
				(+ (count-elem (car c)) (count-aux (cdr c)))
			)
			(if (null node) 0
				(1+ (count-aux (children node))))
		)
	)
)
```
- Problem: What kind of traversal algorithm needed to create the verif-tree function that checks whether a binary tree is valid or not
	- Idea: visit root, check if its a sign, then check left and right
	- Pre-order dfs
```lisp
(defun verif-tree (tree)
	(cond ((null tree) tree) ; If empty then return
		  ((member (key tree) '(+ - / *)) ; If its a sign
				  (and (verif-tree (lc tree)) ; Check the left side of the tree
					   (verif-tree (rc tree)))) ; Check the right side of the tree
		  ((numberp (key tree)) ; If its a number
				    (and (null (lc tree)) ; Then make sure there are no edges from a number
						 (null (rc tree))))))
```
# Expression evaluation - A tree application
- Parse (syntax) Trees are constructs that reflect the syntactical structure of an **input language**
- in lisp, an expression is already represented as a syntax tree
- an eval function traverses the expression's syntax tree to evaluate it
![[Pasted image 20231210173555.png]]
...... MORE ON THIS LATER

# Binary Search Trees
- Benefits
	- maintains a sorted collection of objects
	- guarantees O(log n) for all main operations: search, insertion, modification and deletion
- For that to happen, keys have to be ordered and tress have to be balanced
- **Ordering Property**: Keys smaller than root to the left, keys greater the root are to the right
- **BST Implementation**
	- **node** data structure
	- A **BST** is either NIL or a NODE whose L and R fields are BSTs
	- BSTs will be built using successive calls to BST insert
```lisp
(defstruct node (:conc-name nil)
		   key
		   (lc nil)
		   (rc nil))
(defun bst-insert (obj bst comp)
	(if (null bst) (make-node :key obj) ; if bst is empty then just make the obj root
		(let ((root (key bst)) ; Set the root, left and right values
			  (left (lc bst)) ; based on the current bst
			  (right (rc bst)))
			;; If the obj is smaller, then start traversing the left recursively
			;; If the obj is greater, then start traversing the right recursively
			;; If its equal just return the tree
			(cond ((funcall comp obj root) (setf (lc bst) (bst-insert obj left comp)))
				  ((funcall comp root obj) (setf (rc bst) (bst-inser obj right comp)))
				  (t bst)
			)
		bst)
	)
)
(defun bst-find (obj bst comp)
	(when bst
		(let ((key (key bst)))
			(cond ((funcall comp obj key) (bst-find obj (lc bst) comp))
			      ((funcall comp key obj) (bst-find obj (rc bst) comp))
			      (t bst)
			)
		)
	)
)
(defun bst-min (bst)
	(if (not (null bst))
		(or (node-lc bst) bst))
)
(defun bst-path-to-node (obj root comp &optional path)               
  (when root ; When the root exists
    (let ((key (node-key root)) ; Set key  
          (lc (node-lc root)) ; Set left child
          (rc (node-rc root)) ; Set right child
          (newpath (cons (node-key root) path))) ; Keep updating the path as new values arise
      (cond ((funcall comp obj key) (bst-path-to-node obj lc comp newpath)) ; Left if small
            ((funcall comp key obj) (bst-path-to-node obj rc comp newpath)) ; Right if big  
            (t (reverse newpath))) ; Return the reverse of the newpath list
      )
    )           
  )
(defun bst-print (root)                
  (when root ;; When the root exists
    (append (bst-print (node-lc root)) ; In Order Algorithm      
            (list (node-key root))
            (bst-print (node-rc root)))
    )
  )
```
- **BST-REMOVE**
	- Removes an item from a BST
	- Steps:
		- Traverse to the node that needs to be deleted
		- Two cases to consider
			- The node to be deleted has only one child or no children
			- The node to be deleted has two children
![[Pasted image 20231210211323.png]]
```lisp
(defun bst-join (l r comp)
	(cond ((null l) r)
		  ((null r) l)
		  ((zerop (random 2))
			  (let ((root (node-key (bst-max l))))
				  (make-node :key root :lc (bst-remove root l comp) :rc r)))
		  (t (let ((root (node-key (bst-min r))))
				  (make-node :key root :lc l :rc (bst-remove root r comp))))
	)
)
(defun bst-remove (obj bst comp)
	(when bst
		(let ((key (node-key bst))
			  (l (node-lc bst))
			  (r (node-rc bst)))
			(cond ((funcall comp obj key) (make-node :key key :lc (bst-remove obj l comp) :rc r))
				  ((funcall comp key obj) (make-node :key key :lc l :rc (bst-remove obj r comp)))
				  (t (bst-join l r comp))
			)
		)
	)
)
```
- **BST-INSERT** analysis
	- searching for the appropriate place to insert a node into the tree,  will need at most one comparison at each level 
	- therefore the height of the tree is the limiting factor
	- What is the height of a binary tree likely to be?
		- if the keys are randomly added, then the height is around (log n 2) where n is the number of nodes in the tree
	- Therefore in a perfectly balanced tree, the worst case is (log n 2)
- BSTs are balanced trees
	- A tree is balanced if any pair of paths from the root to a leaf have lengths that can differ at most by **some predefined quantity**
	- Each variant of of BSTs adopt different balance criteria and approaches to tree rebalancing
# **Splay (BST) Trees**
- Recently accessed elements occur near the root
- "self balancing" BST, where balanced ,eams recently accessed elements are near the root
- Insert and lookup operations are O(log n)
- Useful for cache algorithms
- Performance depends on order of insertion/removal of items.
- Tree is constantly restructured. Therefore, unsuitable for many storage options.
- **Splaying**: When a node is accessed, a splay operation is performed on it to move it to the root via a sequence of sub-tree notation
![[Pasted image 20231210214549.png]]
- Interface
	- ST-Search function
		- If the item is in the tree, it splays the node containing the item and returns the new tree
```lisp
(defstruct (node (:conc-name nil)
                 (:print-object (lambda (node out)
                                  (format out "[~a-~@[~a~a]-~@[~a]]"
                                          (key node)
                                          (lc node)
                                          (rc node)))))
  key
  lc  
  rc)
(defun tree-rotate (node parent grandparent)
  (cond ; update the structure of the parent node
  ; if node is equal to the left child of the parent
  ; Then set the left child of the parent to the right child of the node
  ; and set the right child of the node to parent
    ((eql node (lc parent)) (setf (lc parent) (rc node) 
                                  (rc node) parent)) ;EQL compares pointers
  ; If node is equal to the right child of the parent
  ; Then set the right child of the parent to the left child of the node
  ; and set the left child of the node to the parent
    ((eql node (rc parent)) (setf (rc parent) (lc node)
                                  (lc node) parent))
  ; Otherwise throw an error
    (t (error "NODE (~A) is not the child of PARENT (~A)" node parent)))
  (cond ;update structure of the grandparent node
	; If empty just return the node
    ((null grandparent) node)
    ; If parent is eql to the left child of grandparent set the left child to the node
    ((eql parent (lc grandparent)) (setf (lc grandparent) node))
    ; If parent is eql to the right child of grandparent set the right child to the node
    ((eql parent (rc grandparent)) (setf (rc grandparent) node))
    (t (error "PARENT (~A) is not the child of GRANDPARENT (~A)" parent grandparent))  
    )  
  )
  (defun node-chain (item root &optional chain)      
  (if root ; if root exists
      (let ((key (key root)) ; set key lc and rc
            (lc (lc root))
            (rc (rc root))
            (chain (cons root chain))) ; set the chain to a cons of the root and the chain itself
        (cond ((< item key) (node-chain item lc chain)) ; item is smaller than key then chain left
              ((> item key) (node-chain item rc chain)) ; item is greater than the key then chain right
              (t (values root chain))) ; otherwise return the values of the root and chain
        (values nil chain))) ; if the root is nil then return the chain
  ) 
  (defun splay (node chain)
   ; Loop though the parent and grandparent nodes on the chain
   ; And rotate them
  (loop :for (parent grandparent) :on chain :do
    (tree-rotate node parent grandparent)) 
  node    
  )
  ; get the node and the chain from the node-chain function using the item and the root
  ; if node exists then splay the car and cdr
(defun st-search (item root)
  (multiple-value-bind (node chain) (node-chain item root)
    (when node (splay (car chain) (cdr chain))))   
  ) 
(defun st-insert (obj root)
  ; If root doesnt exist then just create the object and return
  (if (null root) (make-node :key obj)
	  ; 
      (multiple-value-bind (node chain) (node-chain obj root)
        (unless node
          (let ((new (make-node :key obj))
                (parent (car chain)))
            (if (< obj (key parent))
                (setf (lc parent) new)
                (setf (rc parent) new))
              (splay new chain))
          ))))  
```