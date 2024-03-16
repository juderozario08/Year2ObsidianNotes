## Some functions to be mindful of
```c
#include <unistd.h>
fork() -> Parent Process has a pid > 0. Child process has a pid = 0. Failure has a pid < 0
```
## OS Management of Application Execution
- Executes and provides resources
- Processor keeps on switching so it seems like all applications appear to be in progress
- Any executable is a process
## Process Elements
- Two essential elements of a process
	- Program Code
	- Data associated with the code
- **Process**: Processor executing a code
![[Pasted image 20240305201120.png|635]]
- *P1* and *P2* have the share code but different set of data
- During program execution, a process can be characterized by a number of elements
	- *Identifier*: Example is a Process ID
	- *State*: Running or Not Running
	- *Priority*: Which one should be executed first
	- *Program counter*: Required to know which instruction to execute
	- *Memory pointers*: Accessing data and code and access to the stack
	- *Context data*: Data within the registers
	- *I/O status information*: Open files/ print requests
	- *Accounting information*: Statistics about process for example: time, space etc
## Process Control Block
- Contains process elements
- interrupt and resume can happen seamlessly
- handled by the OS
- Key to multi-processing
## Process and Processor Behaviours
- **Trace** -> Just a sequence of instructions that are being executed in the memory
	- ***behaviour of an individual process*** by listing the sequence of instructions that execute for that process
	- ***behaviour of the processor*** characterized by showing how the traces of the various processes are interleaved
- **Dispatcher**
	- small program that switches the processor from one process to another 
## Process Execution and Traces of Processes

![[Pasted image 20240309173827.png|353]]
![[Pasted image 20240309173912.png|381]]

## Combined Trace of Processes
![[Pasted image 20240309174251.png|381]]
*What is happening here?*
- Process A = 5000, Process B = 8000, Process C = 12000, Dispatcher = 100
- Process A starts. Then after 6 instructions, dispatcher is released to switch to Process B
- Now Process B executes 4 instructions then an I/O request was made so the dispatcher is released again to cater to the I/O request being made
- Then Process C starts and executes 6 instructions and the dispatcher switches again to Process A to continue from where it left off
- After a few instructions it switches to Process C where it left off and continues to execute the process
## Two-State Process Model
- A process has 2 states: 
	- Running: 
	- Not Running:
		- Model is inadequate
		- Timeout
		- Waiting for I/O
![[Chapter 3-20240309175116488.webp|381]]
## Queuing Diagram

![[Chapter 3-20240309175252164.webp|381]]
- Think of it as a linked list of pointers that points to a block of memory and it does *FIFO*
## Reasons for Process Creation
- *Batch System*: Operator batched a bunch of jobs and when the OS is ready to take a new work, it will read the next job control commands aka the dispatcher
- *Interactive Logon*: User at a terminal logs on to the system
- *Created by OS to provide a service*:  OS can create a process for the user to do something like controlling a print while letting other programs run seamlessly
- *Spawned by existing process*: One process can spawn another process to exploit parallelism
## Process Creation
- *Process Spawning*: OS creating a process at the explicit request of another process
	- *Parent*: The original process
	- *Child*: The spawned process.
	- They have the same set of instructions but they have their own program counter and they run on their own pace. They have their own stack but they share the same code
	- The moment the fork hits, they get their own process control block
## Process Termination
- Every program must have a have a completion
- A batch should stop include a STOP instruction
- For interactive applications, the user can dictate when the process should stop
## Reasons for Process Termination
- *Normal Termination:* The process is complete
- *Time limit exceeded*: Total time limit for cases where the program runs for too long. Then the OS exits the program automatically
- *Memory unavailable:* Requires more memory than the system has
- *Bound violation:* Accessing something that should not be accessed
- *Protection error*: Opening a file for read then attempting to write
- *Arithmetic error*: Divide by 0
- *Time overrun*: If getting some information from a process take too long then the program would be killed
- *IO Failure*: Not finding the file
- *Invalid Instruction*: Executing non existent instructions
- *Privileged Instruction*: Using a valid instruction but not with the right permissions
- *Data misuse*: Wrong type of data
- *OS Intervention*: Deadlocks are killed by OS
- *Parent Termination*: Terminating a parent process can kill all children process 
- *Parent Request*: Parent process can terminate children process
## Five-State Process Model
![[Chapter 3-20240309185712356.webp|381]]
## Process States for Trace

![[Chapter 3-20240309190331233.webp|600]]
## Using Two Queues
![[Chapter 3-20240309191114551.webp]]
- Dispatcher now only works with programs that are ready to run
- The blocked queue contains all programs that stay there until an event happens that it is waiting for
- Ready queue contains all the processes that are ready to run
- Downside is that the dispatcher has to do a linear search each time an event happens and that makes it really slow.
## Multiple Blocked Queues
![[Chapter 3-20240309193545771.webp|650]]
- Here each event has it's own queue.
- Whenever an event occurs, the first blocked program in that queue gets released to enter the ready queue
- The ready queue then executes the process normally
- This process repeats for any event that occurs
## Suspended Processes
- Problem: Processes often wait for I/O
	- Expanding memory to allow more processes would cost memory and bigger processes
- **Swapping**
	- involves moving part or all of the process to disk
	- if there are no processes on the ready queue, it takes some of these processes and writes them to disk to free some memory to load more processes
## One Suspend State

![[Chapter 3-20240309203847992.webp|500]]
- When a program is in blocked for too long, it is suspended and written to disk
- If the event occurs and there is free space is memory then the process is activated again and put in the Ready queue
- Downside is that now there are processes in disk that are blocked but also processes that are ready to go but there is not enough space in the memory for it and so the program has to perform a linear search again to see which processes are ready and which ones are still blocked
## Two Suspend State
 
 ![[Chapter 3-20240309205755572.webp|450]]
- This introduces an additional Queue for *Blocked Suspended* and *Ready Suspended* separately
- Whenever an event occurs if the program is blocked and suspended, then it would be put to the ready queue
- So whenever there is space, the process will be written to memory again from the disk
- Sometimes the program have a higher order process that needs immediate access so in that case the process goes from *Blocked/Suspend to Blocked* from which it can become immediately ready if needed
- If the process was running and it needs a lot of memory then it would write a process to disk and free up some memory to use that for the process that requires it. In that case the running process or a ready process goes to Ready/Suspend
- From *Running to Ready/Suspend* happens when a process is switched with a higher priority process
## Characteristics of a Suspended Process
- Processes are not *immediately available for execution*
- Placed in *suspended state by an agent*: itself, parent or OS
- May or may not be *waiting on an event*
- If the agent suspends it then the *agent also has to unsuspend* it 
## Reasons for Process Suspension
- *Swapping*: OS needs to release enough memory to make room for another process
- *Other OS reason*: OS may suspend a process that is suspected of causing a problem
- *Interactive User Request*: User wishes to suspend execution of a program for purposes of debugging or in connection with the use of resource
- *Timing*: A periodically executed process that is suspended while waiting for the next interval
- *Parent Process Request*: Parent process may suspend execution of child process to examine or change the process or for coordinating with other child processes
## Processes and Resources
![[Chapter 3-20240309220630714.webp|450]]
- P1 is currently running and has control over 2 I/O devices
- P2 is currently blocked and waiting on one of the I/O and is loaded into main memory
- P3 is suspended as it is already in the main memory
## OS Control Tables

## Typical Elements of a Process Image

## Process Identification

## Processor State Information

## X86 EFLAGS Register

## Pentium EFLAGS Register Bits

## Process Control Information

## Structure of Process Images in Virtual Memory

## Process List Structures

## Role of the Process Control Block

## Modes of Execution

## Typical Functions of an Operating System Kernel

## Process Creation

## Process Switching

## Mode VS Process Switch
## Mode Switch
## Process Switch
## The Operating System and Processes
## Summary
