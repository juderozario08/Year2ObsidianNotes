# How to setup a Java file for coding
```java
public class FileName // or you can write (class FileName) or (private class FileName). I will explain the difference later
{
	public static void main(String[] args){
		System.out.println("Hello World"); // This is the print statement for Java
	}
}
```

# Data Types in Java
```java
int i = 10; // Integer
long l = 1000000000000000000; // Long is used for storing big integers
float f = 1.5f; // More Precise
double d = 3.0; // For super big decimal points
boolean b = true; // True or False values
String str = "I love SARA"; // Strings
char c = 'a'; // Only a single character
```

# Some simple codes
```java
System.out.println("Hello World"); // This prints Hello World with a new line at the end
System.out.println(i); // This prints 30 or the value of the variable
// You can do math inside the brackets as well, just make sure to wrap the math in brackets
System.out.println((i+f)); // Prints 11.5

// This is the print statement without a new line \n end
System.out.print("Hello World"); 
System.out.print(i+f); // This prints 11.5 without the new line

// Dw too much about this one
System.out.printf("%i", i); // This is the way to print a formatted String.
```

# Functions
```java
public void functionName(/*Pass the arguments here*/)
{
	// Do something within this function
	System.out.println();
}
// Notice how the previous function does not have a return statement
// This is because of the KEYWORD VOID at the declaration of the function
// There are more like these. For example
public int returnsInt(){
	return 10; // This function must return a integer
}
public long returnsLong(){
	return 1000000000000; // This function must return a long
}
public float returnsFloat(){
	return 10.1f; // This function must return a float
}
public double returnsDouble(){
	return 100000000000.1651; // This function must return a double
}
public String returnsString(){
	return "Hello World"; // This function must return a String
}
public char returnsCharacter(){
	return 'a'; // This function must return a character
}
public boolean returnsTrueOrFalse(){
	return true; // This function must return a boolean
}

// Example function
// Lets say you want to add to numbers together using a function. How woud you do that by creating a function
public add(int num1, int num2){
	return num1+num2;
	// You can also do
	/*
		int result = num1+num2;
		return result;
	*/
}
```
# Conditional Statements
```java
/*
	In python
	if condition1:
		do something
	elif condition2:
		do something
	else:
		do something
*/
// In Java you have
if (condition1)
{
	// Do something
}
else if (condition2)
{
	// Do something
}
else 
{
	// Do something
}

// If you have just one statement in your condition, you don't have to include the curly braces
if (condition)
	do one thing;
```
# Loops
```java
// Creating a very basic for loop in Java
// Think of it as python, very very similar to it
// In python you have { for i in range(start, stop, step) }
// In java you have the same
//     start;    stop;  step  
for (int i = 0; i < 10; i+=1){
	// Do Something within this function
	System.out.println("Hello"); // This will print Hello 10 times
}

// What if you want a forloop like this
// Lets say you have a list in python
// lst = [1,2,3,4,5] and you want to print all of them
// In python you can do
// for i in lst: # The i represents each value in the lst
//     print(i)
// So how would you do it in Java

// This is called a foreach loop and it goes through each element inside the list automatically for you
for (int i: lst){ 
	System.out.println(i);
}

// While loop in Java
// In python you have 
/*
	i = 0
	while i < 10:
		print(i)
		i+=1
*/
// How would you do this in Java
int i = 0;
while (i < 10):
	System.out.println(i);
	i+=1;
```

# Built in methods
```java
// I WILL BE ADDING THESE AS WE GO ALONG THE COURSE

```