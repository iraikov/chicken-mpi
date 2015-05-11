;; Master/worker example implemented with collective operations
;; Can be run as follows: mpirun -np 4 csi -s master-worker.scm

(use srfi-1 srfi-4 mpi)

(MPI:init)


(define comm-world  (MPI:get-comm-world))
(define size        (MPI:comm-size comm-world))
(define myrank      (MPI:comm-rank comm-world))

(if (zero? myrank)
    (begin
      (printf "[~a/~a]: I am the master\n" myrank size)
      
      ;; The root sends the contents of its send buffer to each process (including itself). 
      (let ((data (list-tabulate size (lambda (i) (string->blob (sprintf "Hello ~a..." i))))))
        (MPI:scatterv-bytevector data 0 comm-world))
   
      ;; Each process (root process included) sends the contents of its send
      ;; buffer to the root process. The root process receives the messages
      ;; and stores them in rank order.
      (let ((v (MPI:gatherv-bytevector (string->blob "I am the master!") 0 comm-world)))
        (printf "[~a/~a]: received: ~a\n" myrank size (map blob->string v))
        ))
    (begin
      (printf "[~a/~a]: I am a worker\n" myrank size)
      (let ((n (blob->string (MPI:scatterv-bytevector #f 0 comm-world))))
        (printf "[~a/~a]: received: ~a\n" myrank size n)
        (MPI:gatherv-bytevector (string->blob (sprintf "Processor ~a reporting!" myrank))
                                0 comm-world))
      )
    )

(MPI:finalize)
