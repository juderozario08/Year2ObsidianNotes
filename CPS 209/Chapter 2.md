- Unlike Python, in Java, you can declare a variable without an initial value. (You can assign the value of it later)
	- Like this
```java
String name;
name = "Sara";
```

- **Dynamically Typed**: You do not have to tell the computer that the variable you are assigning is a String or int
- Java is **statically typed**
	- **Statically typed**: You have to tell the computer what type of a variable it is

![[Pasted image 20240119143844.png]]
![[Pasted image 20240119144359.png]]
## Math Libary

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