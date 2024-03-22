## Multiple Processes
- OS design is concerned with the management of processes and threads
	- *Multi-programming*
	- *Multiprocessing*
	- *Distributed Processing*
- This happens when
	- *Multiple Applications* => Invented to allow processing time to be shared among active applications
	- *Structured Applications* => Extension of modular design and structured programming
		- 1 application
		- N processes/threads
	- *Operating System Structure* => OS itself implemented as a set of processes or threads
## Key Terms
- **Atomic Operation** => Function or action implemented as a sequence that no other process can see an intermediate state or interrupt the operation. It is guaranteed to execute or not execute and is a way to isolate the action from the rest of the system.
- **Critical Section** => Section of code that requires access to shared resources and it must not be executes if another process is using this section
- **Deadlock** => processes are unable to proceed because each is waiting for one of the others to do something
- **Livelock** => processes continuously change states in response to changes in their processes without doing useful work
- **Mutual Exclusion** => Only 1 process can be in the critical section at a time which is using the shared resources
- **Race Condition** => multiple threads r/w a shared data item and the result depends on the relative time of their execution
- **Starvation** => runnable process is overlooked indefinitely by the scheduler
## Principles of Concurrency
- *Interleaving and overlapping*
- *Uniprocessor* => The relative speed of execution of processes cannot be predicted
	- depends on activities of other processes
	- the way the OS handles interrupts
	- scheduling policies of the OS
## Difficulties of Concurrency
- Sharing of global resources
- Difficult for the OS to manage the allocation of resources optimally
- Difficult to locate programming errors as results are not deterministic and reproducible
## Race Condition
- When multiple processes or threads read and write shared data items
- Final result depends on the order of execution => loser determines the final value
- Solution is to control access to shared data items
## Operating System Concerns
- Design and management issues raised by the existence of concurrency
	- OS helps ensure the functioning of a processes, and output it produces, are independent of the speed of its execution relative to the speed of other concurrent processes
## Process Interaction
![[Chapter 5-20240318135854924.webp|450]]
## Resource Competition
- Processes come into conflict when they are competing for use of the same resource(I/O, memory, processor time)
- Needs mutual exclusion
- Possible deadlock
- Possible starvation
## Cooperation among Processes by Sharing
- they interact with other processes without being explicitly aware of them
- they may use and update shared data without reference to other processes but know other processes may access the same data
- they must cooperate to ensure the shared data is properly managed
- control mechanisms must ensure the integrity of the shared data, which are held on resources
- need mutual exclusion. possible deadlock or starvation
## Cooperation among processes by Communication
- processes participate in a common effort
- must sync or coordinate the various activities
	- communication messages
	- message send/receive primitives by OS kernel or programming language
- Mutual exclusion is not a control requirement for this sort of cooperation
- Problems of deadlock and starvation are still present
## Mutual Exclusion
![[Chapter 5-20240318141917223.webp|500]]
e.g. critical section: send lines to printer. update shared variables "a" and "b"
## Requirements for Mutual Exclusion
- must be enforced
- halts must not interfere with other processes
- no deadlock or starvation
- must not be denied access to a critical section when there is no other process using it
- no assumptions are made about relative process speeds or number of processes
- process remains inside its critical section for a finite time only
## Mutual Exclusion: Hardware Support
- **Interrupt Disabling** => uniprocessor system. disabling interrupts guarantees mutual exclusion 
- **Disadvantages** => efficiency of execution could be noticeably degraded. won't work on a multiprocessor system
- **Special Machine Instructions**
	- *Compare and Swap Instructions*
		- *compare* is made between memory value and test value
		- *swap* occurs if the values are the same
		- e.g. can access gcc built-in: \_\_sync_val_compare_and_swap(memory_value, test_value, new_value)
## Compare and Swap Instruction Behaviour
```c
compare_and_swap(word, testval, newval)
	oldval = word
	if(word == testval) word = newval
	return oldval
```
## Compare and Swap for Critical Section
```c
/*program mutualexclusion*/ 
const int n = /* number of processes */
int bolt;
void P(int i)
{
	while (true)
	{
		while(compare_and_swap(bolt, 0, 1) == 1)
			/* do nothing */
		/* critical section */
		bold = 0;
		/* remainder */;
	}
}
void main()
{
	bolt = 0;
	parbegin(P(1), P(2), ... , P(n));
}
```
## Exchange Instruction Behaviour
```c
exchange(register, memory)
	temp = memory
	memory = register
	register = temp
```
## Exchange Instruction
```c
const int n = /* number of processes */
int bolt;
void P(int i)
{
	int keyi = 1;
	while (true)
	{
		do exchange (keyi, bolt)
		while(keyi != 0)
		/* critical section */
		bold = 0;
		/* remainder */;
	}
}
void main()
{
	bolt = 0;
	parbegin(P(1), P(2), ... , P(n));
}
```
## Special Machine Instruction
- **Advantages**
	- Applicable to any number of processes on sharing main memory
	- simple and easy to verify
	- used to support multiple critical sections; each critical section defined by its own variable
- **Disadvantages**
	- *Busy-waiting* => while a process is waiting for access to a critical section it continues to consume processor time
	- *starvation* is possible when a process leaves a critical section and more than one process is waiting
	- *deadlock* is possible
## Mutual Exclusion : Software Approaches
- for concurrent processes that execute on a single-processor or a multiprocessor machine with shared main memory
- assume elementary mutual exclusion at the memory access level
	- simultaneous accesses to the same location in main memory are seralised by some sort of memory arbiter, although the order of access granting is not specified ahead of time
	- no support in the hardware, operating system or programming language is assumed
- Algorithms include those by Dekker and Peterson
- **PETERSON's ALGORITHM FOR 2 PROCESSES**
```c
boolean flag[2];
int turn;
void P0(){
	while (true){
		flag[0] = true;
		turn = 1;
		while(flag[1] && turn == 1) /* do nothing */;
		// critical section
		flag[0] = false;
		// remainder
	}
}
void P1(){
	while(true){
		flag[1] = true;
		turn = 0;
		while(flag[0] && turn == 0) /* do nothing */;
		// critical section
		flag[1] = false;
		// remainder
	}
}
void main(){
	flag[0] = false;
	flag[1] = false;
	parbegin(P0, P1);
}
```
## Concurrency Review
![[Chapter 5-20240318150146807.webp|400]]
## Common Concurrency Mechanisms
![[Chapter 5-20240318150334680.webp|400]]
## Semaphore
- A variable that has an integer value upon which only three operations are defined
	- may be *initialised* to a nonnegative integer value
	- *semWait* operation decrements the value
	- *semSignal* operation increments the value
- no way to inspect or manipulate semaphores other than these 3 operations
## Semaphore Operations
- **semInit** => to any non-negative integer value
- **semWait** => decrement value. if value < 0 process doing wait BLOCKS else process continues execution
- **semSignal** => increment value. if value <- 0, process that was block by *semWait* is UNBLOCKED