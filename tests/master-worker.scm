;; Simple master/worker example
;; Can be run as follows: mpirun -np 4 csi -s master-worker.scm

(use srfi-4 mpi)

(MPI:init)


(define comm-world  (MPI:get-comm-world))
(define size        (MPI:comm-size comm-world))
(define myrank      (MPI:comm-rank comm-world))

(if (zero? myrank)
    (begin
      (printf "[~a/~a]: I am the master\n" myrank size)
      (let recur ((i 1))
        (if (< i size)
            (begin
              (MPI:send (string->blob (sprintf "Hello ~a..." i)) i 0 comm-world)
              (recur (+ 1 i)))
            ))

      (let recur ((i 1))
        (if (< i size)
            (let ((n (blob->string (MPI:receive i MPI:any-tag comm-world))))
              (printf "[~a/~a]: received: ~a~%" myrank size n)
              (recur (+ 1 i)))
            ))
      )
    (begin
      (printf "[~a/~a]: I am a worker\n" myrank size)
      (let ((n (blob->string (MPI:receive 0 MPI:any-tag comm-world))))
        (printf "[~a/~a]: received: ~a\n" myrank size n)
        (MPI:send (string->blob (sprintf "Processor ~a reporting!" myrank))
                     0 0 comm-world))
      )
    )

(MPI:finalize)
