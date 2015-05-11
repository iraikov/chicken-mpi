;; Number sorting example implemented with collective operations
;; Can be run as follows: mpirun -np 4 csi -s numsort.scm

(use srfi-1 srfi-4 srfi-4-utils extras mpi)

(MPI:init)

;; maximum number to generate
(define N 1000)

;; size of vector to be sent to each node
(define M 10)

(define comm-world  (MPI:get-comm-world))
(define size        (MPI:comm-size comm-world))
(define myrank      (MPI:comm-rank comm-world))

(define elt< (lambda (i ai j bj) (< ai bj)))

(if (zero? myrank)
    (begin
      (randomize)

      (printf "[~a/~a]: I am the master\n" myrank size)
      
      ;; The root sends the contents of its send buffer to each process (including itself). 
      (let ((data (list-tabulate size (lambda (i) (list->f64vector (list-tabulate M (lambda (j) (random N))))))))
        (let ((mydata (MPI:scatterv-f64vector data 0 comm-world)))
          (f64vector-quick-sort! elt< mydata)
          ;; Each process (root process included) sends the contents of its send
          ;; buffer to the root process. The root process receives the messages
          ;; and stores them in rank order.
          (let ((vs (MPI:gatherv-f64vector mydata 0 comm-world)))
            (printf "[~a/~a]: received: ~a\n" myrank size vs)

            (print (fold (lambda (x ax) (f64vector-merge! elt< x 0 m n ax 0)) result vs))

            ))
        ))
    (begin
      (printf "[~a/~a]: I am a worker\n" myrank size)
      (let ((mydata (MPI:scatterv-f64vector #f 0 comm-world)))
        (printf "[~a/~a]: received: ~a\n" myrank size mydata)
        (f64vector-quick-sort! elt< mydata)
        (MPI:gatherv-f64vector mydata 0 comm-world))
      )
    )

(MPI:finalize)
