
(import scheme (chicken base) (chicken blob) (chicken memory) (chicken pretty-print)
        srfi-1 srfi-4 srfi-13 srfi-14 mpi test)

(define (blob-range x i j) 
  (string->blob (string-copy (blob->string x) i j)))

;; (move-memory! FROM TO [BYTES [FROM-OFFSET [TO-OFFSET]]])
(define (blob-concatenate data extent)
  (let ((buf (make-blob (* extent (length data)))))
    (fold
     (lambda (b i) 
       (move-memory! b buf (blob-size b) 0 (* i extent)) (+ i 1))
     0 data)
    buf))

(MPI:init)

;(print "Host " (get-host-name))

(define comm-world  (MPI:get-comm-world))
(define size        (MPI:comm-size comm-world))
(define myrank      (MPI:comm-rank comm-world))

(define nflds 3)
(define blocklens '(10 1 1))
(define fieldtys `(,MPI:type-char ,MPI:type-u32 ,MPI:type-f64))

(if (zero? myrank) 
    (begin
      (print "size of MPI char type is " (MPI:type-size MPI:type-char))
      (print "extent of MPI char type is " (MPI:type-extent MPI:type-char))
      ))

(define newty (MPI:make-type-struct nflds blocklens fieldtys))
(define tysize (MPI:type-size newty))

(if (zero? myrank) 
    (begin
      (print "newty: ")
      (print newty)
      (print (MPI:datatype? newty))
      (print "extent: " (MPI:type-extent newty))
      (print "size: " (MPI:type-size newty))
      ))

;; (move-memory! FROM TO [BYTES [FROM-OFFSET [TO-OFFSET]]])
(define (make-test-struct-blob str i d)
  (let ((v (make-blob tysize)))
    (move-memory! str v 10 0 0)
    (move-memory! (u32vector i) v 4 0 10)
    (move-memory! (f64vector d) v 8 0 14)
    v))

(define structdata
  (list-tabulate size
                 (lambda (i) 
                   (let ((k 0))
                     (make-test-struct-blob
                      (list->string (list-tabulate 10 (lambda (j) (integer->char (+ i 97)))))
                      (+ (* 20 i) k) 
                      (+ (* 0.1 i) k))))))

(if (zero? myrank)
    (begin
      (print "structdata pack size is " (MPI:pack-size 1 newty comm-world))
      (pp structdata)))

(if (zero? myrank)
    (begin
      (print "structdata:")
      (pp structdata)))



(test-group "MPI test scatter/scatterv with datatypes"
    ;; Scatter
    (let ((data (blob-concatenate structdata (car (MPI:type-extent newty)))))
      (if (zero? myrank)
          (print myrank ": scatter " data))
      (let ((res (MPI:scatter newty data 1 0 comm-world)))
        (print myrank ": received (scatter) " res " (size " (blob-size res) " bytes)")
        (test-assert
         (equal? res (list-ref structdata myrank)))
        (MPI:barrier comm-world))))
  

(MPI:finalize)
