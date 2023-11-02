# How to initialize a pointer
- To create a pointer, we would use a star before a new variable name
- A pointer always has to be initialized before it is used
```c
int *pX;
```
- To initialize a pointer we have to create another variable that we can point to
- In this case, lets create a *variable* x.
```c
int x = 10;
int *pX = &x; // This line says pX is a pointer that points to the address of x;
```

# How to get the value of a pointer
- In other words **Dereferencing a pointer**
- To *dereference* a pointer you put a star before the pointer variable.
```c
int x = 10; // create x
int *pX = &x; // Point pX to address of x
printf("%d\n", *pX); // The * says, go to the address of pX and then point to the address that is stored inside pX.
```
![![CPS393/#^Table3]]
- Dereferencing a pointer would look at the address of pX then dereference that to get the value at that address. Which would be the value of x.
- It can also be read as **value of *variable name*** 

# 