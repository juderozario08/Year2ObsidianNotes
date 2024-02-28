# NOTES: MAKE SURE TO STORE CHANGES IN A VARIABLE WHENEVER YOU ARE MAKING CHANGES. THE COMPUTER WONT REMEMBER CHANGES FOR YOU UNLESS YOU TELL IT TO DO SO.

# NOTES: MAKE SURE THAT IN THE CLASS THAT CONTAINS YOUR MAIN METHOD. EVERY METHOD IS A STATIC METHOD
## Math Libary
- Check out more methods in this link [Java Math Methods | Programiz](https://www.programiz.com/java-programming/library/math)

```java
Math.sqrt(number) -> // square root of any number >= 0
Math.pow(num1, num2) -> // num1 is the base and num2 is the power
Math.sin(num) -> // sin(number in radians)
Math.cos(num) -> // cos(number in radians)
Math.tan(num) -> // tan(number in radians)
Math.toRadians(degrees) -> // convers degrees to radians
Math.toDegrees(radians) -> // convers radians to degrees
Math.exp(number) -> // e^(number)
Math.log(number) -> // Natural log i.e ln(number) make sure that the number is greater than 0
```

## Integer Class
- The **Integer** class is a class that consists of a bunch of important methods that can be helpful for working with integers
```java

/* This function converts a string to a number if and only if the entire string is a number
This is equivalent to writing int(string) in python */
Integer.parseInt(string) 

```

## String Class
- This is the String datatype which is also a class and that's why it has so many methods
- Please try each of these methods out in your IDE so that you can see how it works. You would remember them the best that way
- Use this link to access more details and examples[Java String Methods | Programiz](https://www.programiz.com/java-programming/library/string)
```java
String str1 = "I Love Sara";
String str2 = "i love sara";
str.charAt(index) -> // Returns a Char at a particular index of the String
String.valueOf(character) // Converts a char to a String
// The following method compares 2 strings
// Returns a number less than 0 if str1 is smaller
// Returns a number greater than 0 if str1 is larger
// Returns a number equal to 0 if str1 is equal to str2
str1.compareTo(str2)
// The following method compares 2 strings but ignores the cases
// Returns a number less than 0 if str1 is smaller
// Returns a number greater than 0 if str1 is larger
// Returns a number equal to 0 if str1 is equal to str2
str1.compareToIgnoreCase(str2)
str1.contains("substring") -> // Checks if str1 contains a substring and returns True or False
str1.endsWith("substring") -> // Checks if str1 ends with a certain substring similar to Python
str1.equals(str2) -> // Checks if str1 and str2 are equal
str1.equalsIgnoreCase(str2) -> // Checks if str1 and str2 are equal but ignores the case
str1.indexOf("substring") -> // Returns the starting index of that substring in str1
str1.isEmpty() -> // Checks if the String is empty or not 
str1.length() -> // Returns the length of the string
str1.replace(replace,replaceWith) // Replaces a given substring with another given substring
str1.split(substring) // Same as python .split and it returns an array of Strings
str1.toLowerCase() // Converts to lower case
str1.toUpperCase() // Converts to upper case
str1.trim() // Gets rid of whitespace in the beginning and end of the string
str1.substring(start, stop) // Gets a substring from str1. Similar to the python colon method. str1[start:stop]
```

## ArrayList Class
- This is the ArrayList datatype which is also a class
- Access this link for extra details [Java ArrayList Methods | Programiz](https://www.programiz.com/java-programming/library/arraylist)
```java
ArrayList<Integer> list = new ArrayList<Integer>(); // How to instantiate
list.add(element) -> // Adds an element to the list
list.addAll(anotherList) // Adds everything from anotherList inside the list
list.clear() -> // Empties the whole list
list.clone() -> // Creates a copy of the arrayList. This is so that you are not working with references. Instead it creates a whole new list for you
list.contains(element) -> // Checks if the element is present inside a list
list.get(index) -> // Get an element at the given index
list.indexOf(element) -> // Get the index of an element
list.removeAll(element) -> // Remove all occurences of an element
list.remove(element) -> // Remove the first occurence of an element
list.size() -> // Returns the length of the list
list.isEmpty() -> // Checks if the list is empty or not
list.subList(start, stop) -> // Returns a subList similar to how substring works on Strings
list.sort(Comparator.naturalOrder() or Comparator.reverseOrder()) -> // Sorts the list based on the order specified
list.lastIndexOf(element) -> // Returns the last occurence of an element
list.containsAll(subList) -> // Checks if all elements within a subList are present within the list
```