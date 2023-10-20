# Searching
- **Sequential search**: Searches over all elements of the vector starting from the first.
	- *Complexity*
		- Unordered (item is present) -> Best Case 1 Worst Case n
		- Unordered (item is not present) -> Best Case n worst case n
		- Ordered (item is present) -> Best case 1 Worst case n
		- Ordered (item is not present) -> Best case 1 worst case n
- **Binary Search**: Needs a sorted array. Start from the middle and go by halfs by checking whether the right side or the left side has the result. (Divide and conquer strategy)
	- *Complexity*: O(log n)
```lisp
if the number is smaller than the middle number
    search in the first half
if the number is greater that the middle number
    search in the other half
if the number is equal to the middle number
    found
else not found
```

# Sorting
- Place all elements of a sequence in a certain order determined by a comparison predicate
- Aspects that differentiate sorting algorithms
	- **in-place**: done in the original data structure itself. The alternative of copying-sort
	- **stable**: whether two elements considered equal maintain their original relative positions
	- **online**: whether the algorithm has to see the whole sequence, or can work on a sorted prefix

#selection-sort
- **Selection sort**: *In-place* sorting algorithm. Moves left-to-right as it builds the sorted prefix to the left of the *current element*. In a pass, it finds the *best* element to the right of the current element that swaps these two elements
- **Analysis** 
	- number of comparisons
		- Total number of comparisons ∑(n-1)(i=1) i = 0.5(n^2) - 0.5n
		- Therefore O(n^2) comparisons
	- number of exchanges
		- best case: no exchange if list is sorted
		- worst case: once exchange per pass, hence O(n)

#insertion-sort
- **Insertion sort**
	- it is *online*: the left part is already sorted; it doesn't have to find the maximum element of the whole sequence in a pass
	- each new item is then *inserted* back into the previous such that the sorted sublist is one item larger.
	- starts from index one checks if all values before the current index is smaller or larger, if so then shift the values to the right and then put the current position where it belongs
```lisp
(defun insertion-sort (ls comp)
  (dotimes (i (1- (length ls)) ls) ; Create a forloop that ranges n-1
    (do ((j i (1- j))) ; Create a do loop that goes backwards
        ((< j 0)) ; Exits when j is negative
      (when (funcall comp (aref ls (+ 1 j)) (aref ls j)) ; if j+1 'comp' j then swap
        (rotatef (aref ls (+ 1 j)) (aref ls j))
        )
      )
    )
  )
```
- i is the index of the leftmost unsorted element
- j is initially i and j+1 is index of the current element being inserted into the sorted part
- if the comparison between the current element and its left neighbour is true, the values are swapped
- j is then decremented by one and current element and another attempt to insert current element in the sorted part takes place
- if no swap takes place, i is incremented
- **Analysis**
	- number of comparisons
		- total number of comparisons ∑(n)(i=1)  = (n (n+1))/2, the sum of the arithmetic progression from 1 to n, because at each step, it may need to fully examine the sorted prefix to find the maximum and the prefix's size varies from 1 to n.
		- O(n^2) comparisons

#quicksort
- **Quicksort**: Works in O(n log n) time. It relies on the divide-and-conquer approach: divides the sequence and recursively sorts its segments. The idea:
	- A pivot value that helps us split the vector in two parts: one containing elements smaller than the pivot, and another containing those that are greater
	- The position where the pivot actually belongs in the sorted sequence is called ppvt.
	- The goal is to move items that are on the wrong side with respect to the pivot value while also converging on ppvt.
```lisp
;;; My way of solving it
(defun quicksort (ls comp)
  (quicksort-recursion ls 0 (- (length ls) 1) comp) ; Call the quicksort function 
  ls                                           ; return the list
  )

(defun quicksort-recursion (ls low high comp)
  (if (> high low) ; If the high index > low index
      (let ((pivot (partition ls low high comp))) ; Define pivot index use the partition method to find its designated spots
        (quicksort-recursion ls low (- pivot 1) comp) ; Call the recursive algorithm for sorting left side
        (quicksort-recursion ls (+ pivot 1) high comp) ; Call the recursive algorithm for sorting right side
        )
      )
  )

(defun partition (ls low high comp)
  (let ((pivot_value (aref ls high)) ; set a pivot value
        (i low)) ; set the i value as the lowest index that's 
    (do ((j low (1+ j))) ; j ranges from low to high because of the exit condition
        ((>= j high))
      (when (funcall comp (aref ls j) pivot_value) ; When the current value is less than the pivot
        (rotatef (aref ls i) (aref ls j)) ; Swap i and j then increment i
        (setf i (+ i 1))
        )
      )
    (rotatef (aref ls i) (aref ls high)) ; Ones the pivot destination is found, swap the values of i with pivot
    i ; Return the index of current pivot
    )
  )
```
- In-place sorting
	- *RTL:SLICE* works like python string slicing takes in input start stop. Default *start* = 0 and default *stop* = end 
- **Analysis**
	- Best Case:
		- The pivot is in the middle of the vector
		- On every iteration (n comparisons + n/2 swaps + n/2 increments) = 2n operations. We will need to do that log n times
		- Hence T(n) = 2nlogn which is O(nlogn)
	- Worst Case:
		- Split point skewed to the left or right
		- Sublists to be sorted will have 0 items and n - 1 items
		- Hence O(n^2)