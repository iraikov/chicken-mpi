
(require-extension posix srfi-1 srfi-4 srfi-13 srfi-14 mpi test)

(MPI:init)

(print "Host " (get-host-name))

(define comm-world  (MPI:get-comm-world))
(define size        (MPI:comm-size comm-world))
(define myrank      (MPI:comm-rank comm-world))

(define nflds 2)
(define blocklens '(1 10))
(define displs '(0 1))
(define fieldtys `(,MPI:type-char ,MPI:type-int))

(print MPI:type-int)
(print MPI:type-char)

(print (MPI:datatype? MPI:type-int))
(print (MPI:datatype? MPI:type-char))

(print (MPI:type-extent MPI:type-int))
(print (MPI:type-extent MPI:type-char))

(print (MPI:type-size MPI:type-int))
(print (MPI:type-size MPI:type-char))


(define newty (MPI:make-type-struct nflds blocklens displs fieldtys))
(print newty)

(print (MPI:datatype? newty))
(print (MPI:type-extent newty))
(print (MPI:type-size newty))


(MPI:finalize)
