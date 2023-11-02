- Function calls
- Arithmetic operations like  + - // / % * ** ()
- # Remember that Strings and Tuples are immutable but lists aren't
- Remember these built in functions:
```python
	break -> # Breaks through a loop
	continue -> # Skips that specific iteration
	round(number, # of decimal places you want)
	abs(number)
	min(list), min(num1, num2, ...)
	max(list), max(num1, num2, ...)
	len(list, strings, tuples)
	str -> generally used for converting some other data type to a string
	int -> generally used for converting some other data type to an int
	float -> generally used for converting some other data type to a float
	bool -> generally used for converting some other data type to a bool
	input(always takes input as a string unless specified to convert to something else)
	range(start, stop(excluding), step)
	sorted(takes in a list and sorts it)
	type(checks the type of some value)
	enumerate(list) -> # returns a list of tuples (index, value). Generally for forloops
	print()
```
# Math methods to remember

```python
math.sqrt() -> Square root of any float or int
math.sin(takes a value in radians)
math.cos(takes a value in radians)
math.tan(takes a value in radians)
math.pow(number, exponent)
math.factorial(value)
math.floor() -> Rounds down a value. 3.75 becomes 3
math.ceil() -> Rounds up a value. 3.1 becomes 4
math.exp(power). # Same as e^x from calculus
math.pi # this is not a method and thats why it does not have the brackets
```
# String methods to remember

- string.replace(string to replace, what to replace with, how many times to replace) -> If the number of times is not given, it will replace all of them
```python
string = "Hello, my name is Jude"
# Let's remove all the spaces from this string
string = string.replace(' ', '')
# ' ' means space and '' this means empty quotes so basically nothing
# Final string is
print(string)
'Hello,mynameisJude'
```
- string.strip() -> Gets rid of all the white spaces on the left and the right side of the string
```python
string = "    Hello   "
string = string.strip()
print(string)
"Hello"
```
- string.split(string to split by) -> whenever it sees that string, it will turn that into a separate element
```python
string = "1 2 3 4 5 6" # Everything is separated by spaces
string = string.split(" ")
print(string)
# ['1', '2', '3', '4', '5', '6']
```
- string.find(strings to find) -> return the index
```python
string = "hello my name is Jude"
print(string.find('my'))
# Result is 6 as the substring 'my' starts from index 6
# If in case the string is not found, then it will return -1
```
- string.capitalize() -> Capitalizes the first letter of the first word
```python
string = 'hello'
print(string.capitalize())
# Prints Hello
```
- string.title() -> Capitalizes the first letter of every word (title case)
```python
string = "hello world"
print(string.title())
# Prints 'Hello World'
```
- These few functions are generally used for each character at a time
```python
string.isalpha() -> # checks if the character is an alphabet
string.isdigit() -> # checks if the character is a digit
string.islower() -> # checks if the character is lowercase
string.isupper() -> # checks if the character is uppercase
string.count() -> # counts the number of times a substring was repeated
'a' is a lowercase letter
'A' is an uppercase letter
'1' is a digit
'c' is an alphabet
```
- string.endswith(substring goes here) -> checks if the strings ends with that particular substring
```python
string = "hello"
print(string.endswith('lo'))
# Prints True
```
- ''.join(list)->Concatenates all elements inside a list. Works only with a list of strings.
```python
["Hello", "my", "name", "is", "Jude"] -> HellomynameisJude
```
- string.upper()->Turns all characters to uppercase 
- string.lower()-> Turns all characters to lowercase
# Note:
- Remember that while working with strings you can also use fstrings.
```python
name = "Jude"
age = 18
print(f"My name is {name} and my age is {age}.") # Better method than using + to print. Readable
```
- There are also raw strings which basically output exactly what is written inside the string
```python
print(r"Hello \n, my name is Jude\t. Nice to meet you all.")
# This prints
"Hello \n, my name is Jude\t. Nice to meet you all."
instead of 
"Hello"
", my name is Jude    . Nice to meet you all."
```
# List functions to remember

**Let's say the variable name is ls**
```python
ls.index(value) -> Get the index of value the first time it appears
ls.count(value) -> Similar to string, returns the count of how many times the value appears
ls.sort() -> Sort the list ascending order
ls.sort(reverse=True) -> Sort the list descending order
```
# Note that lists are mutable. So ls.sort() automatically changes the original list. You don't have to set the value to the sorted list unlike strings where would have to set the value to the sorted list. For example:
```python
# Thats how sort would work for list
ls = [1, 3, 2, 10, 9, 4]
ls.sort()
print(ls)

# This is how uppercase would work for string
string = "hello"
string = string.upper()
print(string)

# Notice how for string you have to do string = then whatever u want to change it to.
# But for lists, u just say list.sort() and it will sort the original list
# So strings are immutable and lists are mutable

ls.reverse() -> # Returns the same list but reading backwards. Similar to ls[::-1]. It DOES NOT SORT.
ls.append(value) -> # Append a value to the end of the list
ls.pop(index) -> # Removes a value at a particular index and returns that value as well
ls.remove(value) -> # Removes the first instance of the value in the list

ls = [1, 3, 2, 10, 9, 4]
ls.pop(3) # This would remove the element at index 3. So 10 would be removed
print(ls) # [1, 3, 2, 9, 4]

ls = [1, 3, 2, 9, 10, 9, 4]
ls.remove(9) # This would remove first instance of the value from the list
print(ls) # [1, 3, 2, 10, 9, 4]
```
- ls.insert(index, value) -> Append a value in the list before a particular index
```python
ls = [1, 3, 2, 9, 10, 4]
ls.insert(2, 5) # Inserting the value 5 before index 2
print(ls) # [1, 3, 5, 2, 9, 10, 4]
```

# Tuples 
- There are only two functions to remember for tuples
- Let the variable name be tuple
```python
tuple.count(value) -> # counts how many times a value appeared in the tuple
tuple.index(value) -> # Returns the index of the value in the first time it appeared in the tuple
```

# Set
- **This will remove all duplicates from the list**
```python
lst = [1, 3, 3, 5, 18]
lst = list(set(lst))
print(lst) # This prints [1, 18, 3, 5]
```
# Note that this won't work if you need the list in a certain order as it randomizes the list.
# Tips - Based on Questions
- Always double check what data type you are working with
- When using for loops, make sure you know whether you are working with indexes or values
- For example: It says add every element in the odd index then you know you have to work with indexes. So you would have to use ```
```python
variable_name[index] 
```
- If you are working with just the values then 
```python
for value in list_name/string_name/tuple_name:
	do something with value
```
- If you have a list of lists or a list of strings and you need to go through each character at a times. Generally you would do this
- List of strings, list of lists, list of tuples (That is if you do not know the size)
```python
list_of_strings = ["Hello", "my", "name"]
for word in list_of_strings:
	for character in word:
		do something
```
- List comprehension Syntax - This is the general syntax
- Value_to_append can be anything you want
```python
[value_to_append for i in range()/list/string/tuples]
[value_to_append for i in range()/list/string/tuples if condition]
[value_to_append if condition else value_to_append for i in range()/list/string/tuples]

# See below to see how things are formatted. Round brackets rememble the divide
[(value_to_append if condition) (else value_to_append) for i in range()/list/string/tuples]

# Chained List Comprehension
[value_to_append for i in range(any number) for j in range(any number)...]
# This would run (number * number) times.

# Creating a list of list using list comprehension -> Called Nested List Comprehension
[[value_to_append for j in range(num)] for i in range(num)]
# Lets do this one for example
[[j for j in range(5)] for i in range(3)]
# This would create this list
[[0,1,2,3,4], [0,1,2,3,4], [0,1,2,3,4], [0,1,2,3,4], [0,1,2,3,4]]
```
- Whenever you are creating a variable, be aware of the scope
```python
if x > 5:
	w = 20
	z = 50
print(w, z)
# Here the print function will have an error as the values dont exist outside of the if condition that is why they are indented
# Instead do this
if x > 5:
	w = 20
	z = 50
	print(w, z)
# This way you wont get an error
# You can also swap the print with a return and it will work just fine
```
- If a question says use some type of default values inside a function. Then do this
```python
def function(a=20, b=30)
	return a+b
# function() would return 50
# function(10, 20) would return 30
# function(a=10) would return 40

Lets say we needed a function that needs to have only one default value
Then this is how it would work

def function(a, b=30)
	return a + b

# function() this would give an error as 'a' does not have a value
# function(10) would return 40
# function(10, 20) would return 30
```
- if you ever need to use **global variables** inside a function this is how you would do it.
- Global variables are variables that are not written inside a function
```python
nums = 10
def function():
	global nums
	nums = 20

print(nums) # This would print 10
function()
print(nums) # This would print 20 as we changed the value of the nums variable using the function
```
- Remember that **lists** can be sliced the sam way **srings** can be sliced
```python
string[start:stop:step] # This is the general format
string = "Hello"
print(string[2:]) -> # Prints 'llo'
print(string[1:4]) -> # Prints 'ell'

# List slicing has the same format
list[start:stop:step]
list = [1,2,3,4,5,6,7,8]
print(list[2:6]) -> # Prints [3, 4, 5, 6]
```
- Always check if the order of operations make sense
- Remember that any value in python other than an **empty string, empty list or 0** in python is **True**
	- Not the most important point but just a thing to remember incase it pops up
```python
if 1 # is the same as writing
if True
# Same way
if 0/""/[] # is the same as writing
if False
```
- Some things with tuples
```python
# When given a tuple you can use something called **De-structuring** of a tuple
# When we create a variable to represent a whole tuple like this
tup = (1, 2, 3, 4)
# We have to print each of them separately by doing this
tup[0], tup[1], tup[2], tup[3]
# Instead we can destructure the tuple by doing this instead if we know the size of the tuple
a,b,c,d = (1,2,3,4)
# Then we can just put
a, b, c, d # and it will print the values normally

# There is another way to do this as well lets say you want only the values for the first two elements and want to leave the rest of the tuple as it is
a, b, *c = (1, 2, 3, 4)
# Here
a = 1
b = 2
c = (3, 4)

# You can also do this
a, *b ,c = (1, 2, 3, 4)
a = 1
b = (2, 3)
c = 4

# When creating a tuple with one element remember to put the comma at the end
# Otherwise it will just be a regular integer
tuple = (2,)
```
- This is how you would generally use enumerate
```python
list = ["Jude", "Loves", "Sara", "A", "Lot"]
for index,value in enumerate(list):
	print(f"{index}:{value}")
# This would print
0:"Jude"
1:"Loves"
2:"Sara"
3:"A"
4:"Lot"
```

# While loops
```python
# This is the general syntax for while loop
while condition:
	do something
# Remember that the condition needs to be true for the while loop to continue looping

# Example
list = [1, 3, 4, 5]
i = 0
while i < len(list):
	print(list[i])
	i+=1
# This would print 
1
3
4
5
```


# This is when to use for loop or while loop
![[Pasted image 20231023152402.png]]
- Writing **else** after a loop
```python
while condition:
	do something
else:
	do something # NOTE: THIS WILL ONLY EXECUTE WHEN THE LOOP DID NOT BREAK OR RETURN
```

# Random library
```python
import random
# To generate a random number within a certain range, do this
random.randint(1, 5) -> # This will generate a random integer between 1 to 5 both inclusive
random.random() -> # This will generate a random value between 0 to 1 without including one and will include decimals
```

# Bisection Search (Memorize)
```python
# This is a basic square root function
x = number
low = 0
high = x
eps = 0.001 # Delta Value
guess = (low + high)/2
while abs(guess ** 2 - x) > eps:
	if guess**2 > x:
		high = guess
	else:
		low = guess
	guess = (low + high)/2
```
- A test question would be to check if your string or list given is a palindrome
```python
# Simplest way to solve that is checking if the forward is the same as the backward
# We will do that using string slicing
string = "racecar"
if string == string[::-1] # string[::-1] reverses the string
	return True
else: 
	return False
```

# Hints
- Integers or floats do not have any index
- # Remember these
- n%10 gives you the last digit
- n//10 gets rid of the last digit
```python
# This is the only odds example from the pdf
if n == 0:
	return False
while n != 0
	if (n%10)%2 == 1: # Gets 
		n = n//10 # Gets rid of the last digit
		continue
	else:
		return False
return True
```

