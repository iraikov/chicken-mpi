# mpi

Chicken Scheme bindings for the Message Passing Interface (MPI).

## Documentation

MPI (http://www-unix.mcs.anl.gov/mpi/) is a popular library for
distributed-memory parallel programming. It offers both point-to-point
message passing and group communication operations (broadcast,
scatter/gather, etc).

Open MPI (http://www.open-mpi.org/) is an implementation of the MPI
standard that combines technologies and resources from several other
projects (FT-MPI, LA-MPI, LAM/MPI, and PACX-MPI) in order to build the
best MPI library available.

The Chicken MPI egg provides a Scheme interface to a large subset of
the MPI 1.2 procedures for communication.  It is based on the
[[http://pauillac.inria.fr/~xleroy/software.html#ocamlmpi|Ocaml MPI]]
library by Xavier Leroy. The {{mpi}} library has been tested with Open
MPI versions 1.2.4 - 1.6.5.

### Initialization and time procedures


<procedure>MPI:init :: [ARG1 ...] -> UNDEFINED</procedure>

Initializes the MPI execution environment. This routine must be called
before any other MPI routine. MPI can be initialized at most once.



<procedure>MPI:spawn :: COMMAND * ARGUMENTS *  MAXPROCS * LOCATIONS * ROOT * COMM -> (COMM * S32VECTOR)</procedure>

Spawns {{MAXPROCS}} identical copies of the MPI program specified by
{{COMMAND}} and returns an intercommunicator and a vector of status
values. {{ARGUMENTS}} is a list of command-line
arguments. {{LOCATIONS}} is a list of string pairs {{(HOST * WDIR)}}
that tell MPI the host and working directory where to start processes.

<procedure>MPI:finalize</procedure>

Terminates the MPI execution environment. 

<procedure>MPI:wtime :: VOID -> SECONDS</procedure>

Returns the number of seconds representing elapsed wall-clock time on
the calling process.


### Handling of communicators


<procedure>MPI:comm? :: OBJ -> BOOL</procedure>

Returns true if {{OBJ}} is an MPI communicator object, false otherwise. 

<procedure>MPI:get-comm-world:: VOID -> COMM</procedure>

Returns the default communicator created by {{MPI_Init}}; the group
associated with this communicator contains all processes.

<procedure>MPI:comm-size :: COMM -> INTEGER</procedure>

Returns the size of the group associated with communicator {{COMM}}. 


<procedure>MPI:comm-rank :: COMM -> INTEGER</procedure>

Returns the rank of the calling process in communicator {{COMM}}. 


<procedure>MPI:comm-equal? :: COMM1 * COMM2 -> BOOL</procedure>

Returns true if the two given communicators are for identical groups, false otherwise. 


<procedure>MPI:comm-split :: COMM * COLOR * KEY -> COMM</procedure>

Creates new communicators based on colors and keys. 


<procedure>MPI:comm-create :: COMM * GROUP -> COMM</procedure>

Creates a new communicator with communication group that spans all
processes in {{GROUP}} and a new context. See the procedures in
subsection ''Handling of communication groups'' for information on how
to create process group objects.



<procedure>MPI:make-cart :: COMM * DIMS * PERIODS * REORDER -> COMM</procedure>

Creates a new communicator with Cartesian topology
information. Argument {{DIMS}} is an SRFI-4 s32vector that contains
the number of dimensions of the Cartesian grid. Argument {{PERIODS}}
is an SRFI-4 s32vector of the same length as {{DIMS}} that indicates
if the grid is periodic (1) or not (0) in each dimension. Argument
{{REORDER}} is a boolean value that indicates whether process ranking
may be reordered.



<procedure>MPI:make-dims :: NNODES * NDIMS -> DIMS</procedure>

Creates a division of processes in a Cartesian grid. Argument
{{NNODES}} is the number of nodes in the grid. Argument {{NDIMS}} is
the number of Cartesian dimensions. The return values is an SRFI-4
s32vector.



<procedure>MPI:cart-coords :: COMM * RANK -> COORDS</procedure>

Determines process coordinates in Cartesian topology, given a rank in
the group. The return value is an SRFI-4 s32vector of length {{NDIMS}}
(the number of dimensions in the Cartesian topology).



### Handling of communication groups


<procedure>MPI:group? :: OBJ -> BOOL</procedure>

Returns true if {{OBJ}} is an MPI group object, false otherwise. 



<procedure>MPI:comm-group :: COMM -> GROUP</procedure>
Returns the group associated with the given communicator. 


<procedure>MPI:group-size :: GROUP -> INTEGER</procedure>
Returns the size of the group {{GROUP}}. 


<procedure>MPI:group-rank :: GROUP -> INTEGER</procedure>
Returns the rank of the calling process in the given group. 


<procedure>MPI:group-translate-ranks :: GROUP1 * RANKS * GROUP2 -> RANKS2</procedure>

Translates the ranks of processes in one group to those in another
group. The return value is an SRFI-4 s32vector.



<procedure>MPI:group-union :: GROUP1 * GROUP2 -> GROUP</procedure>



<procedure>MPI:group-difference :: GROUP1 * GROUP2 -> GROUP</procedure>



<procedure>MPI:group-intersection :: GROUP1 * GROUP2 -> GROUP</procedure>



<procedure>MPI:group-incl :: GROUP * RANKS -> GROUP</procedure>

Produces a group by reordering an existing group and taking only
members with the given ranks. Argument {{RANKS}} is an SRFI-4
s32vector.



<procedure>MPI:group-excl :: GROUP * RANKS -> GROUP</procedure>

Produces a group by reordering an existing group and taking only
members that do not have the given ranks. Argument {{RANKS}} is an
SRFI-4 s32vector.



### Point-to-point communication


Most communication procedures in this library come in several flavors,
for fixnums, integers, floating point numbers, bytevectors, and for
each of the SRFI-4 homogeneous vector types.

<procedure>MPI:send-TYPE :: DATA * DEST * TAG * COMM -> UNDEFINED</procedure>

Performs a standard-mode blocking send. Argument {{DEST}} is the rank
of the destination process. Argument {{TAG}} is integer message
tag. {{TYPE}} is one of the following: {{fixnum, int, flonum,
bytevector, s8vector, u8vector, s16vector, u16vector, s32vector,
u32vector, f32vector, f64vector}}



<procedure>MPI:receive-TYPE :: SOURCE * TAG * COMM -> DATA</procedure>



<procedure>MPI:receive-TYPE :: LENGTH * SOURCE * TAG * COMM -> DATA</procedure>

Performs a standard-mode blocking receive. Argument {{DEST}} is the
rank of the destination process. Argument {{TAG}} is integer message
tag. Argument {{LENGTH}} is present only in the vector
procedures. {{TYPE}} is one of the following: {{fixnum, int, flonum,
bytevector, s8vector, u8vector, s16vector, u16vector, s32vector,
u32vector, f32vector, f64vector}}



<procedure>MPI:probe :: SOURCE * TAG * COMM -> (COUNT * SOURCE * TAG)</procedure>

Check for an incoming message. This is a blocking call that returns
only after a matching message is found. Argument {{SOURCE}} can be
{{MPI:any-source}}. Argument {{TAG}} can be {{MPI:any-tag}}.


### Group communication


<procedure>MPI:barrier :: COMM -> UNDEFINED</procedure>
Barrier synchronization. 


<procedure>MPI:broadcast-TYPE :: DATA * ROOT * COMM -> UNDEFINED</procedure>

Broadcasts a message from the process with rank root to all other
processes of the group. {{TYPE}} is one of the following: {{fixnum,
int, flonum, bytevector, s8vector, u8vector, s16vector, u16vector,
s32vector, u32vector, f32vector, f64vector}}



<procedure>MPI:scatter-TYPE :: DATA * SENDCOUNT * ROOT * COMM -> DATA</procedure>

Sends data from the root process to all processes in a group, and
returns the data received by the calling process. Argument
{{SENDCOUNT}} is the number of elements sent to each process. Argument
{{DATA}} is only required at the root process. All other processes can
invoke this procedure with (void) as {{DATA}}. {{TYPE}} is one of the
following: {{int, flonum, bytevector, s8vector, u8vector, s16vector,
u16vector, s32vector, u32vector, f32vector, f64vector}}



<procedure>MPI:scatterv-TYPE :: DATA * ROOT * COMM -> DATA</procedure>

Sends variable-length data from the root process to all processes in a
group, and returns the data received by the calling process.  Argument
{{DATA}} is only required at the root process, and is a list of values
of type {{TYPE}}, where each element of the list is sent to the
process of corresponding rank. All other processes can invoke this
procedure with (void) as {{DATA}}. {{TYPE}} is one of the following:
{{int, flonum, bytevector, s8vector, u8vector, s16vector, u16vector,
s32vector, u32vector, f32vector, f64vector}}



<procedure>MPI:gather-TYPE :: DATA * SENDCOUNT * ROOT * COMM -> DATA</procedure>

Gathers data from a group of processes, where each process send data
of the same length.  Argument {{SENDCOUNT}} is the number of data
elements being sent by each process. {{TYPE}} is one of the following:
{{int, flonum, bytevector, s8vector, u8vector, s16vector, u16vector,
s32vector, u32vector, f32vector, f64vector}}



<procedure>MPI:gatherv-TYPE :: DATA * ROOT * COMM -> DATA</procedure>

Gathers data from a group of processes, where each process can send
data of variable length. {{TYPE}} is one of the following: {{int,
flonum, bytevector, s8vector, u8vector, s16vector, u16vector,
s32vector, u32vector, f32vector, f64vector}}



<procedure>MPI:allgather-TYPE :: DATA * ROOT * COMM -> DATA</procedure>

Gathers data of variable length from all processes and distributes it
to all processes. {{TYPE}} is one of the following: {{int, flonum,
bytevector, s8vector, u8vector, s16vector, u16vector, s32vector,
u32vector, f32vector, f64vector}}



<procedure>MPI:reduce-TYPE :: DATA * OP * ROOT * COMM -> DATA</procedure>

Reduces values on all processes within a group, using a global reduce
operation, and return the result at the root process. {{OP}} is one of
the following: {{MPI:i_max, MPI:i_min, MPI:i_sum, MPI:i_prod,
MPI:i_land, MPI:i_lor, MPI:i_xor}} (integer operations); and
{{MPI:f_max, MPI:f_min, MPI:f_sum, MPI:f_prod}} (floating point
operations). {{TYPE}} is one of the following: {{int, flonum,
bytevector, s8vector, u8vector, s16vector, u16vector, s32vector,
u32vector, f32vector, f64vector}}



<procedure>MPI:allreduce-TYPE :: DATA * OP * COMM -> DATA</procedure>

Reduces values on all processes within a group, using a global reduce
operation, and return the result at each process. {{OP}} is one of the
following: {{MPI:i_max, MPI:i_min, MPI:i_sum, MPI:i_prod, MPI:i_land,
MPI:i_lor, MPI:i_xor}} (integer operations); and {{MPI:f_max,
MPI:f_min, MPI:f_sum, MPI:f_prod}} (floating point
operations). {{TYPE}} is one of the following: {{int, flonum,
bytevector, s8vector, u8vector, s16vector, u16vector, s32vector,
u32vector, f32vector, f64vector}}



<procedure>MPI:scan-TYPE :: DATA * OP * COMM -> DATA</procedure>

Computes a partial reduction across the processes in a group. {{OP}}
is one of the following: {{MPI:i_max, MPI:i_min, MPI:i_sum,
MPI:i_prod, MPI:i_land, MPI:i_lor, MPI:i_xor}} (integer operations);
and {{MPI:f_max, MPI:f_min, MPI:f_sum, MPI:f_prod}} (floating point
operations). {{TYPE}} is one of the following: {{int, flonum,
bytevector, s8vector, u8vector, s16vector, u16vector, s32vector,
u32vector, f32vector, f64vector}}



=== Round-robin routines


The following variants of {{fold}}, {{map}} and {{for-each}} process
lists in round-robin fashion on MPI nodes: for a given node {{n}},
only list elements whose index is a modulo of n will be processed on
this node.

<procedure>MPI-rr-fold :: FN * INITIAL * XS -> RESULT</procedure>

<procedure>MPI-rr-map :: FN * XS -> RESULT</procedure>

<procedure>MPI-rr-for-each :: FN * XS -> VOID</procedure>



## Examples


### Master/worker example

```scheme
;; Simple master/worker example
;; Can be run as follows: mpirun -np 4 csi -s master-worker.scm
;; where -np # indicates the number of processes

(use srfi-4 mpi)

(MPI:init)

;; MPI uses objects called communicators to define how processes
;; communicate with each other.  Almost all MPI routines require a
;; communicator as an argument.  

;; `MPI:get-comm-world' returns the communicator object which can send
;; messages to all running MPI processes

(define comm-world  (MPI:get-comm-world))

;; `MPI:comm-size' returns the number of running MPI processes
;;  (including the current one)

(define size        (MPI:comm-size comm-world))

;; `MPI:comm-rank' returns the rank of the calling MPI process

(define myrank      (MPI:comm-rank comm-world))

;; We assign rank 0 to be the master process, and the rest will be
;; worker processes

(if (zero? myrank)
    (begin
      (printf "[~a/~a]: I am the master\n" myrank size)
      (let recur ((i 1))
        (if (< i size)
            (begin
              ;; Send Hello message to process of rank i
              (MPI:send (string->blob (sprintf "Hello ~a..." i)) i 0 comm-world)
              (recur (+ 1 i)))
            ))

      (let recur ((i 1))
        (if (< i size)
             ;; Wait for a response from process of rank i
            (let ((n (blob->string (MPI:receive i MPI:any-tag comm-world))))
              (printf "[~a/~a]: received: ~a~%" myrank size n)
              (recur (+ 1 i)))
            ))
      )
    (begin
      (printf "[~a/~a]: I am a worker\n" myrank size)
      ;; Wait for a message from the master (process 0)
      (let ((n (blob->string (MPI:receive 0 MPI:any-tag comm-world))))
        (printf "[~a/~a]: received: ~a\n" myrank size n)
        ;; Send a response back to the master
        (MPI:send (string->blob (sprintf "Processor ~a reporting!" myrank))
                     0 0 comm-world))
      )
    )
```

### Master/worker implemented with collective operations

```scheme
;; Master/worker example implemented with collective operations
;; Can be run as follows: mpirun -np 4 csi -s master-worker.scm

(use srfi-1 srfi-4 mpi)

(MPI:init)

;; MPI uses objects called communicators to define how processes
;; communicate with each other.  Almost all MPI routines require a
;; communicator as an argument.  

;; `MPI:get-comm-world' returns the communicator object which can send
;; messages to all running MPI processes

(define comm-world  (MPI:get-comm-world))

;; `MPI:comm-size' returns the number of running MPI processes
;;  (including the current one)

(define size        (MPI:comm-size comm-world))

;; `MPI:comm-rank' returns the rank of the calling MPI process

(define myrank      (MPI:comm-rank comm-world))

;; We assign rank 0 to be the master process, and the rest will be
;; worker processes

(if (zero? myrank)
    (begin
      (printf "[~a/~a]: I am the master\n" myrank size)
      
      ;; data is a list of vectors to be sent to each process.  The
      ;; master process sends element i from the list to process i
      ;; (including itself). Note that each process must call scatterv
      ;; in order to receive its data. In this example, the master
      ;; ignores the result to its call to scatterv.

      (let ((data (list-tabulate size (lambda (i) (string->blob (sprintf "Hello ~a..." i))))))
        (MPI:scatterv-bytevector data 0 comm-world))
   
      ;; With gatherv, each process (master process included) sends
      ;; the contents of its send buffer to the master process. The
      ;; master process receives the messages and stores them in rank
      ;; order.

      (let ((v (MPI:gatherv-bytevector (string->blob "I am the master!") 0 comm-world)))
        (printf "[~a/~a]: received: ~a\n" myrank size (map blob->string v))
        ))
    (begin
      (printf "[~a/~a]: I am a worker\n" myrank size)

      ;; The worker collects its data via a call to scatterv. The data
      ;; argument is #f because the worker is not sending anything,
      ;; just receiving.

      (let ((n (blob->string (MPI:scatterv-bytevector #f 0 comm-world))))
        (printf "[~a/~a]: received: ~a\n" myrank size n)

        ;; The worker sends its result back to the master via a call to gatherv.
        (MPI:gatherv-bytevector (string->blob (sprintf "Processor ~a reporting!" myrank))
                                0 comm-world))
      )
    )

(MPI:finalize)
```


## Version history

; 1.14 : Added simple round-robin routines
; 1.12 : Fixes to allgather-int and allgather-flonum (thanks to Peter Bex)
; 1.11 : Test script fixes
; 1.9 : Ensure test script returns non-zero on error (thanks to mario)
; 1.7 : Switched to wiki documentation
; 1.6 : Ported to Chicken 4
; 1.5 : Added a binding for MPI:spawn
; 1.3 : Bug fix in MPI:scatter-int
; 1.2 : Bug fix in the meta file
; 1.1 : Bug fixes and improvements to the regression tests
; 1.0 : Initial release

## License

>
> Copyright 2007-2015 Ivan Raikov
> 
> Based on the Ocaml MPI library by Xavier Leroy. 
> 
> This program is free software: you can redistribute it and/or modify
> it under the terms of the GNU General Public License as published by
> the Free Software Foundation, either version 3 of the License, or (at
> your option) any later version.
> 
> This program is distributed in the hope that it will be useful, but
> WITHOUT ANY WARRANTY; without even the implied warranty of
> MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
> General Public License for more details.
> 
> A full copy of the GPL license can be found at
> <http://www.gnu.org/licenses/>.
>
