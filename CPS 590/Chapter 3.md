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
- During program execution, a process can be characterised by a number of elements
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
	- Not Running: Model is inadequate, Timeout, Waiting for I/O
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
![[Chapter 3-20240309190331233.webp|459]]
## Using Two Queues
![[Chapter 3-20240309191114551.webp|458]]
- Dispatcher now only works with programs that are ready to run
- The blocked queue contains all programs that stay there until an event happens that it is waiting for
- Ready queue contains all the processes that are ready to run
- Downside is that the dispatcher has to do a linear search each time an event happens and that makes it really slow.
## Multiple Blocked Queues

![[Chapter 3-20240309193545771.webp|446]]
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
![[Chapter 3-20240309203847992.webp|425]]
- When a program is in blocked for too long, it is suspended and written to disk
- If the event occurs and there is free space is memory then the process is activated again and put in the Ready queue
- Downside is that now there are processes in disk that are blocked but also processes that are ready to go but there is not enough space in the memory for it and so the program has to perform a linear search again to see which processes are ready and which ones are still blocked
## Two Suspend State
 
 ![[Chapter 3-20240309205755572.webp|409]]
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
![[Chapter 3-20240309220630714.webp|418]]
- P1 is currently running and has control over 2 I/O devices
- P2 is currently blocked and waiting on one of the I/O and is loaded into main memory
- P3 is suspended as it is not even in the main memory
## OS Control Tables
![[Chapter 3-20240316172512192.webp|416]]
## Elements of a Process Image
- **User Program**: Program to be executed
- **User data**: modifiable part of the user space
- **Stack**: Parameters and calling addresses for procedure and system calls
- **Attributes**: For OS to control process. i.e. PID, Process State, Process Control
## Process Identification -> PID
- Each process has a unique number
- PID works as the index for cross referencing tables
- Memory tables provides map of the main memory with an indication where each process is assigned
- Process communication -> PID informs OS of the destination of the communication
- Process creation -> PID indicates the parent and descendants of each process
## Processor State Information
- **Consists of the contents of processor registers**
	- user-visible register
	- control and status register
	- stack pointers
- **Program Status Word**
	- condition codes or other info
	- *EFLAGS* is a register which is an example of *PSW*
## X86 EFLAGS Register
![[Chapter 3-20240316182231464.webp|500]]
## Pentium EFLAGS Register Bits
- Control Bits
	- *AC (Alignment Check)*: Set if a word or double word is addressed on a non word/doubleword boundary.
	- *ID (Identification Flag)*: If this bit can be set and cleared means the processor supports CPUID instruction. This instruction provides information about the vendor, family and model
	- *RF (Resume flag)*: Disable debug exceptions so that the instruction can be restarted after a debug exception without immediately causing another debug exception
	- *IOPL (I/O privilege level)*: Causes the processor to generate an exception on all accesses to I/O devices during protected mode operation
	- *DF (Direction Flag)*: string processing instructions increment or decrement the 16-bit half-registers *SI and DI* for 16 bit registers or the 32 bit registers *ESI(holds the source index)* and *EDI(holds the destination index)*
	- *IF (Interrupt enable flag)*: processor will recognize external interrupts
	- *TF (Trap Flag)*: Causes an interrupt after the execution of each instruction. Used for debugging
- Operating mode bits
	- *NT(Nested Task Flag)*: Indicates that current task is nested within another task in protected mode operation
	- *VM (Virtual 8086 mode*: Allows the programmer to enable or disable VM. Determines if the processor runs as 8086 machine.
	- *VIP (Virtual interrupt pending):* Used in VM to indicate that one or more interrupts are awaiting service
	- *VIF (Virtual Interrupt Flag)*: Used in VM mode instead of *IF*
- Condition Codes
	- *AF (Auxiliary carry flag)*: represents carrying or borrowing between half bytes of an 8-bit arithmetic unit or logic operations using *AL*
	- *CF (Carry Flag)*: carrying out or borrowing into the leftmost bit following a math operation. used by shift and rotate operations
	- *OF (Overflow flag)*: arithmetic overflow
	- *PF (Parity flag)*: parity of result after math or logic operation. 1 is even 0 is odd
	- *SF (Sign flag)*: sign of the result of a math or logic operation
	- *ZF (Zero flag)*: result of an arithmetic or logic operation is 0
## Process Control Information
- **Scheduling**: state of the process
- **Data Structuring**: links to child, parent, queues etc
- **IPC**: shared among processes for communication
- **Privileges**: limits to machine instruction or memory
- **Memory Management**
- **Resources**: Open files/pipes/logs
## Structure of Process Images in Virtual Memory
![[Chapter 3-20240316191846257.webp|467]]
## Role of the Process Control Block
- The most important data structure in an OS
	- contains all of the information about a process 
	- blocks are read and or modified by virtually every module in the OS
	- defines the state of the OS
- Protection is hard
	- a big in a single runtime could damage it which can destroys the systems ability to manage the affected processes
	- design change in the structure or semantics could affect a number of modules in the OS
## Modes of Execution
- User Mode
	- less privileged mode
	- user programs typically execute in this mode
- System Mode
	- more privileged mode
	- also referred to as control mode or kernel mode
	- kernel of the operating system
## Typical Functions of an Operating System Kernel
- *Process Management*
	- Process creation
	- Scheduling and dispatching
	- Switching
	- Synchronization and support for interprocess communication
	- management of process control blocks
- *Memory Management*
	- Allocation of address space to processes
	- Swapping 
	- Page and segment management
- *I/O Management*
	- Buffer management
	- Allocation of I/O channels and devices to processes
- *Support Functions*
	- Interrupt handling
	- Accounting
	- Monitoring
## Process Creation
- Assigns a unique PID to the new process
- allocates space for process
- initializes the process control block
- sets the appropriate linkages
- creates or expands other data data structure
## Process Switching
- *Interrupt*: External to the execution of the current instruction. Reaction to an asynchronous external event
- *Trap*: Associated with the execution of the current instruction. Handling of an error or exception
- *Supervisor call*: Explicit request. Call to an operating system function

## System Interrupts
- Due to some sort of event that is external to and independent of the currently running process
	- Clock interrupt
	- I/O interrupt
	- Memory Fault
- *Time Slice* = max amount of time that a process can execute before being interrupted
- Trap
	- An error condition generated within the currently running process
	- OS determines if the condition is fatal
		- yes: exit state and switch process
		- no: action depends on nature of the error
## Mode VS Process Switch
![[Chapter 3-20240316233734326.webp|200]]
- No interrupts pending the processor
	- Proceeds to the fetch state and fetches the next instruction in the current process
- If an interrupt is pending the processor
	- sets the program counter to the starting address of an interrupt handler
	- switches from user to kernel mode so that the interrupt handler can execute privileged instructions
![[Chapter 3-20240316234312471.webp|250]]
## Mode Switch
- OS saves P1's processor state info(context)
	- Program Counter
	- Other registers
	- Stack information
- OS Handles interrupts
- OS restores P1's context
- P1 continues
## Process Switch
- Steps in a full process
	- save the context of current processor
	- update the PCB of the process in the running state
	- move the PCB of the process to the appropriate queue
	- select another process for execution
	- update the process control block of the process selected
	- update memory management data structures
	- restore context of the processor to that which existed at the time the selected process was last switched out
## The Operating System and Processes
![[Chapter 3-20240316235334218.webp|400]]