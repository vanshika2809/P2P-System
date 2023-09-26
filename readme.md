# Project 3

**Group Members:**
1. Vaishnavi Chilakamarthi, UFID: 99936597, vchilakamarthi@ufl.edu 
2. Vanshika Mehrotra, UFID: 77239277 , vmehrotra@ufl.edu

**Description**
In this project we aim to design, build and analyze the implementation of CHORD algorithm using AKKA actor framework.CHORD is a simple Peer to Peer protocol which implements a Distributed Hash Table detailed as per the paper - Stoica, Ion, Robert Morris, David Karger, M. Frans Kaashoek, and Hari Balakrishnan. "Chord: A scalable peer-to-peer lookup service for internet applications." ACM SIGCOMM Computer Communication Review 31, no. 4 (2001): 149-160.A Chord is one of the original Distributed Hash Table projects from the MIT PDOS group at Computer Science and AI Laboratory, MIT. A Distributed hash table(DHT) is a hash table distributed across various nodes in the network. The system allows any node that is part of the network to lookup location (i.e IP Address, in most cases) of the node containing the value of the key (not the value of the key itself).


## Project Structure
The project contains the following 3 files:

# main.erl
This file contains the logic to take input from the user. The main:run() function is the entry point to the project and this function takes the number of nodes to be created and the number of lookup requests to performed from the user. This file also contains the spawned function trackHops() which stores all the values of hops taken for each lookup performed. It computes and prints the average number of hops taken for all the lookup requests when it does not receive any hop values for a fixed duration of 300ms (Timeout).

# storage.erl
This file is used to store the key value pairs of all the finger tables of all the nodes. It has a functions to create, store and lookup for data.

# node.erl
This file contains the logic to create nodes, connect them in a ring structure and allocate finger tables to each of it. The This file also contains the logic to compute the number of hops performed in order to search for a key and sends this value to the trackHops() pid.


## Execution steps
1. cd into the project3 folder
2. Start erl shell and compile main.erl, storage.erl and node.erl: 'c(main).','c(storage)' and 'c(nide).'
3. Run run in main.erl with NunNodes and NumRequests as arguments.
For eg.- main:run(10000,10000).
4. Ignore the warning and wait for the program to exit. Once it is exited, you can see the average number of hops required to complete searching all the keys.


**What is working?**

We are able to implement chord distributed hash functions. The nodes are connected in a ring structure arranged according to node ids.
We form a network of nodes entered by the user and assign ids to the nodes. After the intial network is setup five new nodes join the network. The incoming node is assigned a node from the existing network which is taken to be as the known node of the network. The finger table of the incoming node is updated and its successors and predecessors are set.
Key-value pairs are stored in nodes and average hops required to retreive a key-value pair are calculated.

**Average number of hops**
The average number of hops is defined as total number of hops/total lookups performed.
Average lookup calculation : where we calculate the average number of hops by performing lookups of all the keys on all the nodes. This measures the performance and the time complexity of chord algorithm.

![Fig-1](https://github.com/vanshika2809/dosp_project1/blob/main/project3.png)

*Fig.1 Output displaying the average number of hops required to perform lookup on all the keys using Chord*

**Largest Network Achieved**

The largest network that we managed to achieve was 10000 nodes and we performed 10000 lookup operations on the network.Requests per node.Normally, the number of hops grows linearly with the number of nodes, but if nodes fail, the number of hops increases even more.


