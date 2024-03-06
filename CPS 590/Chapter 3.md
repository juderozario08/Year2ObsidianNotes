## OS Management of Application Execution
- Executes and provides resources
- Processor keeps on switching so it seems like all applications appear to be in progress
- Any executable is a process
## Process Elements
- Two essential elements of a process
	- Program Code
	- Data associated with the code
- Process: Processor begins to execute the code
![[Pasted image 20240305201120.png]]
- *P1* and *P2* have the share code but different set of data
- During program execution, a process can be characterized by a number of elements
	- Identifier
	- State
	- Priority
	- Program counter
	- Memory pointers
	- Context data
	- I/O status information
	- Accounting information
## Process Control Block
- Contains process elements
- interrupt and resume can happen seamlessly
- handled by the OS
- Key to multi-processing
## Process and Processor Behaviours
- **Trace**
	- ***behaviour of an individual process*** by listing the sequence of instructions that execute for that process
	- ***behaviour of the processor*** characterized by showing how the traces of the various processes are interleaved
- **Dispatcher**
	- small program that switches the processor from one process to another
## Process Execution


## Traces of Processes


## Combined Trace of Processes


## Queuing Diagram


## Reasons for Process Creation


## Process Creation


## Process Termination


## Reasons for Process Termination


## Two-State Process Model


## Five-State Process Model


## Process States for Trace


## Using Two Queues


## Multiple Blocked Queues


## Suspended Processes


## One Suspend State


## Two Suspend State


## Characteristics of a Suspended Process


## Reasons for Process Suspension


## Processes and Resources


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


## System Interrupts


## Mode VS Process Switch


## Mode Switch


## Process Switch


## The Operating System and Processes


## Summary
