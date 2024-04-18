## Deadlock
- *Permanent* blocking of a set of processes that either compete for system resource or communicate with each other
- Awaiting an event that can only be triggered by another blocked process in set
- No efficient solution
## Join Progress Diagram
![[Chapter 6-20240417085534392.webp|464]]
## No Deadlock
![[Chapter 6-20240417085613300.webp|459]]
## Resource Categories
- ***Reusable***
	- safely used by 1 process at a time
	- not depleted by the use
	- processors, IO channels, main and secondary memory, devices, and data structures such as files, db and semaphores
- ***Consumable***
	- can be produced and consumed
	- interrupts, signals, messages, and information in IO buffers
## Reusable Resources

![[Chapter 6-20240417091547660.webp|460]]
## Memory Request
- Space is available for allocation of 200kb and the following event sequence occurs
![[Chapter 6-20240417091833472.webp|457]]
- Deadlock occurs if both processes progress to their 2nd request
## Consumable Resources
- Each process attempts to receive a message from the other process and then send a message to the other process
- *Deadlock* happens if the *Receive* is blocked
![[Chapter 6-20240417092013712.webp|309]]
## Deadlock Detection, Prevention and Avoidance
![[Chapter 6-20240417092135735.webp|450]]
## Resource Allocation Graphs
![[Chapter 6-20240417092233100.webp|448]]
## Resource Allocation Graphs
![[Chapter 6-20240417092308943.webp|449]]
## Conditions for Deadlock
- **Mutual Exclusion**
	- Only one process at a time
	- No process that can use the resource that is allocated for another one
- **Hold and Wait**
	- Hold allocated resources while awaiting assignment of others
- **No Pre-emption**
	- can't be forcibly removed from a process holding it
- **Circular Wait**
	- each process holds at least one resource needed by the next process in the chain
## Dealing with Deadlock
- **Prevent Deadlock**
	- policy that eliminates one of the conditions
- **Avoid Deadlock**
	- make appropriate dynamic choices based on the current state of resource allocation
- **Detect Deadlock**
	- attempt to detect the present of a deadlock and take action to recover
## Deadlock Prevention Strategy
- **Indirect**
	- prevent occurrence of one of the 3 necessary conditions: (Mutual Exclusion, Hold and Wait, No Pre-Emption)
- **Direct**
	- prevent the occurrence of a circular wait
## Deadlock Condition Prevention
- **Eliminate Mutual Exclusion**
	- if access to a resource requires mutual exclusion, it must be supported by OS
- **Eliminate Hold and Wait**
	- a process requests all of its required resources at once and block the process until all requests can be granted simultaneously
- **Eliminate No Pre-emption** -> (Simply allowing preemption)
	- if a process is denied a further request, then it must release original resources and request them again long with the additional resource (*ASKER* preempted)
	- OS may preempt 2nd process and require it to release its resource (*HOLDER* preempted)
- **Eliminate Circular Wait**
	- define a linear ordering of resources
	- processes must request in that order
	- can be inefficient
## Deadlock Avoidance
- Estimate whether the current resource allocation request will, if granted potentially lead to a deadlock
- Requires future insights into resource requests
## 2 Approaches to Deadlock Avoidance
- **Deadlock Avoidance**
	- *Resource Allocation Denial*
		- do not grant an incremental resource request to a process if this allocation might lead to a deadlock
	- *Process Initiation Denial*
		- do not start process if its demands might lead to a deadlock
## Process Initiation Denial
- System has 21M of memory
- P1 requests initiation -- allowed \[16 < 21M\]
- P2 requests initiation -- allowed \[(16+4) < 21M\]
- P3 requests initiation -- denied \[(16 + 4 + 2) > 21M\]
![[Chapter 6-20240417113911030.webp|467]]
## Resource Allocation Denial
- Referred to as the banker's algorithm
- *State* - reflects current allocation of resources to processes
- *Safe State* - at least one sequence of resource allocations to processes that does not result in a deadlock
- *Unsafe State* - not safe
