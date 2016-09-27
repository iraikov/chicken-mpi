# mpi

Chicken Scheme bindings for the Message Passing Interface (MPI).

## Documentation

MPI (http://www.mpi-forum.org) is a popular standard for
distributed-memory parallel programming. It offers both point-to-point
message passing and group communication operations (broadcast,
scatter/gather, etc).

Open MPI (http://www.open-mpi.org/) is an implementation of the MPI
standard that combines technologies and resources from several other
projects (FT-MPI, LA-MPI, LAM/MPI, and PACX-MPI) in order to build the
best MPI library available.

The Chicken MPI egg provides a Scheme interface to a large subset of
the MPI 1.2 procedures for communication.  It is based on the Ocaml
MPI library by Xavier Leroy
(http://forge.ocamlcore.org/projects/ocamlmpi/). The mpi library has
been tested with Open MPI versions 1.2.4 - 1.10.1 and MPICH version
3.2.

### Initialization and time procedures


`MPI:init :: [ARG1 ...] -> UNDEFINED`

Initializes the MPI execution environment. This routine must be called
before any other MPI routine. MPI can be initialized at most once.



`MPI:spawn :: COMMAND * ARGUMENTS *  MAXPROCS * LOCATIONS * ROOT * COMM -> (COMM * S32VECTOR)`

Spawns `MAXPROCS` identical copies of the MPI program specified by
`COMMAND` and returns an intercommunicator and a vector of status
values. `ARGUMENTS` is a list of command-line
arguments. `LOCATIONS` is a list of string pairs `(HOST * WDIR)`
that tell MPI the host and working directory where to start processes.

`MPI:finalize`

Terminates the MPI execution environment. 

`MPI:wtime :: VOID -> SECONDS`

Returns the number of seconds representing elapsed wall-clock time on
the calling process.


### Handling of communicators


`MPI:comm? :: OBJ -> BOOL`

Returns true if `OBJ` is an MPI communicator object, false otherwise. 

`MPI:get-comm-world:: VOID -> COMM`

Returns the default communicator created by `MPI_Init`; the group
associated with this communicator contains all processes.

`MPI:comm-size :: COMM -> INTEGER`

Returns the size of the group associated with communicator `COMM`. 


`MPI:comm-rank :: COMM -> INTEGER`

Returns the rank of the calling process in communicator `COMM`. 


`MPI:comm-equal? :: COMM1 * COMM2 -> BOOL`

Returns true if the two given communicators are for identical groups, false otherwise. 


`MPI:comm-split :: COMM * COLOR * KEY -> COMM`

Creates new communicators based on colors and keys. 


`MPI:comm-create :: COMM * GROUP -> COMM`

Creates a new communicator with communication group that spans all
processes in `GROUP` and a new context. See the procedures in
subsection ''Handling of communication groups'' for information on how
to create process group objects.



`MPI:make-cart :: COMM * DIMS * PERIODS * REORDER -> COMM`

Creates a new communicator with Cartesian topology
information. Argument `DIMS` is an SRFI-4 s32vector that contains
the number of dimensions of the Cartesian grid. Argument `PERIODS`
is an SRFI-4 s32vector of the same length as `DIMS` that indicates
if the grid is periodic (1) or not (0) in each dimension. Argument
`REORDER` is a boolean value that indicates whether process ranking
may be reordered.



`MPI:make-dims :: NNODES * NDIMS -> DIMS`

Creates a division of processes in a Cartesian grid. Argument
`NNODES` is the number of nodes in the grid. Argument `NDIMS` is
the number of Cartesian dimensions. The return values is an SRFI-4
s32vector.



`MPI:cart-coords :: COMM * RANK -> COORDS`

Determines process coordinates in Cartesian topology, given a rank in
the group. The return value is an SRFI-4 s32vector of length `NDIMS`
(the number of dimensions in the Cartesian topology).



### Handling of communication groups


`MPI:group? :: OBJ -> BOOL`

Returns true if `OBJ` is an MPI group object, false otherwise. 



`MPI:comm-group :: COMM -> GROUP`
Returns the group associated with the given communicator. 


`MPI:group-size :: GROUP -> INTEGER`
Returns the size of the group `GROUP`. 


`MPI:group-rank :: GROUP -> INTEGER`
Returns the rank of the calling process in the given group. 


`MPI:group-translate-ranks :: GROUP1 * RANKS * GROUP2 -> RANKS2`

Translates the ranks of processes in one group to those in another
group. The return value is an SRFI-4 s32vector.



`MPI:group-union :: GROUP1 * GROUP2 -> GROUP`



`MPI:group-difference :: GROUP1 * GROUP2 -> GROUP`



`MPI:group-intersection :: GROUP1 * GROUP2 -> GROUP`



`MPI:group-incl :: GROUP * RANKS -> GROUP`

Produces a group by reordering an existing group and taking only
members with the given ranks. Argument `RANKS` is an SRFI-4
s32vector.



`MPI:group-excl :: GROUP * RANKS -> GROUP`

Produces a group by reordering an existing group and taking only
members that do not have the given ranks. Argument `RANKS` is an
SRFI-4 s32vector.



### MPI datatypes

`MPI:datatype? :: OBJ -> BOOL`

Returns true if `OBJ` is an MPI datatype object, false otherwise. 

`MPI:type-extent :: DATATYPE -> (EXTENT LB)`

Returns the extent and lower bound of an MPI data type.

`MPI:type-size :: DATATYPE -> INT`

Returns the size of a datatype.


`MPI:type-char`

`MPI:type-int`

`MPI:type-fixnum`

`MPI:type-flonum`

`MPI:type-byte`

`MPI:type-s8`

`MPI:type-u8`

`MPI:type-s16`

`MPI:type-u16`

`MPI:type-s32`

`MPI:type-u32`

`MPI:type-f32`

`MPI:type-f64`

Predefined MPI data types.

`MPI:make-type-struct :: FIELD-COUNT * BLOCK-LENS * FIELDTYS -> DATATYPE`

Given a gield count, field lengths and field types, creates and
returns a new MPI structure data type with the given fields.



### Point-to-point communication


Most communication procedures in this library come in several flavors,
for derived datatypes, fixnums, integers, floating point numbers,
bytevectors, and for each of the SRFI-4 homogeneous vector types.

`MPI:send :: DATATYPE * DATA * DEST * TAG * COMM -> UNDEFINED`
`MPI:send-TYPE :: DATA * DEST * TAG * COMM -> UNDEFINED`

Performs a standard-mode blocking send. Argument `DEST` is the rank of
the destination process. Argument `TAG` is integer message
tag. Argument `DATATYPE` is an MPI datatype object. `TYPE` is one of
the following: `fixnum, int, flonum, bytevector, s8vector, u8vector,
s16vector, u16vector, s32vector, u32vector, f32vector, f64vector`



`MPI:receive :: DATATYPE * SOURCE * TAG * COMM -> DATA`
`MPI:receive-TYPE :: LENGTH * SOURCE * TAG * COMM -> DATA`

Performs a standard-mode blocking receive. Argument `DEST` is the rank
of the destination process. Argument `TAG` is integer message
tag. Argument `LENGTH` is present only in the vector
procedures. Argument `DATATYPE` is an MPI datatype object. `TYPE` is
one of the following: `fixnum, int, flonum, bytevector, s8vector,
u8vector, s16vector, u16vector, s32vector, u32vector, f32vector,
f64vector`


`MPI:probe :: DATATYPE * SOURCE * TAG * COMM -> (COUNT * SOURCE * TAG)`

Check for an incoming message of the given type. This is a blocking
call that returns only after a matching message is found. Argument
`SOURCE` can be `MPI:any-source`. Argument `TAG` can be `MPI:any-tag`.


### Group communication


`MPI:barrier :: COMM -> UNDEFINED`
Barrier synchronization. 


`MPI:broadcast :: DATATYPE * DATA * ROOT * COMM -> UNDEFINED`
`MPI:broadcast-TYPE :: DATA * ROOT * COMM -> UNDEFINED`

Broadcasts a message from the process with rank root to all other
processes of the group. Argument `DATATYPE` is an MPI datatype
object. `TYPE` is one of the following: `fixnum, int, flonum,
bytevector, s8vector, u8vector, s16vector, u16vector, s32vector,
u32vector, f32vector, f64vector`



`MPI:scatter :: DATATYPE * DATA * SENDCOUNT * ROOT * COMM -> DATA`
`MPI:scatter-TYPE :: DATA * SENDCOUNT * ROOT * COMM -> DATA`

Sends data from the root process to all processes in a group, and
returns the data received by the calling process. Argument `SENDCOUNT`
is the number of elements sent to each process. Argument `DATA` is
only required at the root process. All other processes can invoke this
procedure with (void) as `DATA`. Argument `DATATYPE` is an MPI
datatype object. `TYPE` is one of the following: `int, flonum,
bytevector, s8vector, u8vector, s16vector, u16vector, s32vector,
u32vector, f32vector, f64vector`



`MPI:scatterv :: DATATYPE * DATA * ROOT * COMM -> DATA`
`MPI:scatterv-TYPE :: DATA * ROOT * COMM -> DATA`

Sends variable-length data from the root process to all processes in a
group, and returns the data received by the calling process.  Argument
`DATA` is only required at the root process, and is a list of values
of type `TYPE`, where each element of the list is sent to the process
of corresponding rank. All other processes can invoke this procedure
with (void) as `DATA`. Argument `DATATYPE` is an MPI datatype
object. `TYPE` is one of the following: `int, flonum, bytevector,
s8vector, u8vector, s16vector, u16vector, s32vector, u32vector,
f32vector, f64vector`



`MPI:gather :: DATATYPE * DATA * SENDCOUNT * ROOT * COMM -> DATA`
`MPI:gather-TYPE :: DATA * SENDCOUNT * ROOT * COMM -> DATA`

Gathers data from a group of processes, where each process send data
of the same length.  Argument `SENDCOUNT` is the number of data
elements being sent by each process. Argument `DATATYPE` is an MPI
datatype object. `TYPE` is one of the following: `int, flonum,
bytevector, s8vector, u8vector, s16vector, u16vector, s32vector,
u32vector, f32vector, f64vector`



`MPI:gatherv-TYPE :: DATATYPE * DATA * ROOT * COMM -> DATA`
`MPI:gatherv-TYPE :: DATA * ROOT * COMM -> DATA`

Gathers data from a group of processes, where each process can send
data of variable length. Argument `DATATYPE` is an MPI datatype
object. `TYPE` is one of the following: `int, flonum, bytevector,
s8vector, u8vector, s16vector, u16vector, s32vector, u32vector,
f32vector, f64vector`



`MPI:allgather :: DATATYPE * DATA * ROOT * COMM -> DATA`
`MPI:allgather-TYPE :: DATA * ROOT * COMM -> DATA`

Gathers data of variable length from all processes and distributes it
to all processes. Argument `DATATYPE` is an MPI datatype
object. `TYPE` is one of the following: `int, flonum,
bytevector, s8vector, u8vector, s16vector, u16vector, s32vector,
u32vector, f32vector, f64vector`

`MPI:alltoall :: DATATYPE * DATA * SIZE * COMM -> DATA`
`MPI:alltoall-TYPE :: DATA * SIZE * COMM -> DATA`

Collects data of size `SIZE` from all processes and distributes it to
all processes. Argument `DATATYPE` is an MPI datatype object. `TYPE`
is one of the following: `int, flonum, bytevector, s8vector, u8vector,
s16vector, u16vector, s32vector, u32vector, f32vector, f64vector`

`MPI:alltoallv :: DATATYPE * DATA * SIZEVEC * COMM -> DATA`
`MPI:alltoall-TYPE :: DATA * SIZEVEC  * COMM -> DATA`

Collects variable length data from all processes and distributes it to
all processes. Argument `DATATYPE` is an MPI datatype object. `TYPE`
is one of the following: `int, flonum, bytevector, s8vector, u8vector,
s16vector, u16vector, s32vector, u32vector, f32vector, f64vector`


`MPI:reduce-TYPE :: DATA * OP * ROOT * COMM -> DATA`

Reduces values on all processes within a group, using a global reduce
operation, and return the result at the root process. `OP` is one of
the following: `MPI:i_max, MPI:i_min, MPI:i_sum, MPI:i_prod,
MPI:i_land, MPI:i_lor, MPI:i_xor` (integer operations); and
`MPI:f_max, MPI:f_min, MPI:f_sum, MPI:f_prod` (floating point
operations). `TYPE` is one of the following: `int, flonum,
bytevector, s8vector, u8vector, s16vector, u16vector, s32vector,
u32vector, f32vector, f64vector`



`MPI:allreduce-TYPE :: DATA * OP * COMM -> DATA`

Reduces values on all processes within a group, using a global reduce
operation, and return the result at each process. `OP` is one of the
following: `MPI:i_max, MPI:i_min, MPI:i_sum, MPI:i_prod, MPI:i_land,
MPI:i_lor, MPI:i_xor` (integer operations); and `MPI:f_max,
MPI:f_min, MPI:f_sum, MPI:f_prod` (floating point
operations). `TYPE` is one of the following: `int, flonum,
bytevector, s8vector, u8vector, s16vector, u16vector, s32vector,
u32vector, f32vector, f64vector`



`MPI:scan-TYPE :: DATA * OP * COMM -> DATA`

Computes a partial reduction across the processes in a group. `OP`
is one of the following: `MPI:i_max, MPI:i_min, MPI:i_sum,
MPI:i_prod, MPI:i_land, MPI:i_lor, MPI:i_xor` (integer operations);
and `MPI:f_max, MPI:f_min, MPI:f_sum, MPI:f_prod` (floating point
operations). `TYPE` is one of the following: `int, flonum,
bytevector, s8vector, u8vector, s16vector, u16vector, s32vector,
u32vector, f32vector, f64vector`



### Round-robin routines


The following variants of `fold`, `map` and `for-each` process
lists in round-robin fashion on MPI nodes: for a given node `n`,
only list elements whose index is a modulo of n will be processed on
this node.

`MPI-rr-fold :: FN * INITIAL * XS -> RESULT`

`MPI-rr-map :: FN * XS -> RESULT`

`MPI-rr-for-each :: FN * XS -> VOID`



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
              (MPI:send-bytevector (string->blob (sprintf "Hello ~a..." i)) i 0 comm-world)
              (recur (+ 1 i)))
            ))

      (let recur ((i 1))
        (if (< i size)
             ;; Wait for a response from process of rank i
            (let ((n (blob->string (MPI:receive-bytevector i MPI:any-tag comm-world))))
              (printf "[~a/~a]: received: ~a~%" myrank size n)
              (recur (+ 1 i)))
            ))
      )
    (begin
      (printf "[~a/~a]: I am a worker\n" myrank size)
      ;; Wait for a message from the master (process 0)
      (let ((n (blob->string (MPI:receive-bytevector 0 MPI:any-tag comm-world))))
        (printf "[~a/~a]: received: ~a\n" myrank size n)
        ;; Send a response back to the master
        (MPI:send-bytevector (string->blob (sprintf "Processor ~a reporting!" myrank))
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

- 2.0 : Support for MPI alltoall / alltoallv operations
- 2.0 : Support for MPI derived datatypes
- 1.14 : Added simple round-robin routines
- 1.12 : Fixes to allgather-int and allgather-flonum (thanks to Peter Bex)
- 1.11 : Test script fixes
- 1.9 : Ensure test script returns non-zero on error (thanks to mario)
- 1.7 : Switched to wiki documentation
- 1.6 : Ported to Chicken 4
- 1.5 : Added a binding for MPI:spawn
- 1.3 : Bug fix in MPI:scatter-int
- 1.2 : Bug fix in the meta file
- 1.1 : Bug fixes and improvements to the regression tests
- 1.0 : Initial release

## License

>
> Copyright 2007-2016 Ivan Raikov
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
