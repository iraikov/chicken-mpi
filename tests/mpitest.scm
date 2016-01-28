;;
;;
;; Chicken MPI test 
;;
;; Based on the Caml/MPI interface by Xavier Leroy.
;;
;; Copyright 2007-2015 Ivan Raikov.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.
;;

(require-extension posix srfi-1 srfi-4 srfi-13 srfi-14 mpi test)

(define (land . args)
  (if (null? args) #t
      (and (car args) (apply land (cdr args)))))

(define (lor . args)
  (if (null? args) #f
      (or (car args) (apply lor (cdr args)))))

(define (eval-op op args)
  (apply 
   (cond ((= op MPI:i_max)   max)
	 ((= op MPI:i_min)   min)
	 ((= op MPI:i_sum)   +)
	 ((= op MPI:i_prod)  *)
	 ((= op MPI:i_land)  land)
	 ((= op MPI:i_lor)   lor)
	 ((= op MPI:i_xor)   fxxor)
	 ((= op MPI:f_max)   max)
	 ((= op MPI:f_min)   min)
	 ((= op MPI:f_sum)   +)
	 ((= op MPI:f_prod)  *)
	 (else (error 'eval-op "unknown op " op)))
   args))

(define (blob-range x i j) 
  (string->blob (string-copy (blob->string x) i j)))

(define (make-srfi4-vector-map makev vlen vset! vref)
  (lambda (v f)
    (let loop ((v v) (newv (makev (vlen v))) (n (- (vlen v) 1)))
      (if (>= n 0)
	  (let ((x (f (vref v n))))
	    (vset! newv n x)
	    (loop v newv (- n 1)))
	  (begin
	    newv)))))

(define (make-srfi4-vector-range makev vlen vset! vref)
  (lambda (v i j)
    (and (and (positive? j) (or (zero? i) (positive? i)) (< i j) (< (- j i) (vlen v)))
	 (let loop ((v v) (newv (makev (- j i))) (n 0) (i i))
	   (if (< i j)
	       (let ((x (vref v i)))
		 (vset! newv n x)
		 (loop v newv (+ n 1) (+ i 1)))
	       newv)))))


(define u8vector-map (make-srfi4-vector-map make-u8vector
					    u8vector-length
					    u8vector-set!
					    u8vector-ref))

(define s8vector-map (make-srfi4-vector-map make-s8vector
					    s8vector-length
					    s8vector-set!
					    s8vector-ref))

(define u16vector-map (make-srfi4-vector-map make-u16vector
					     u16vector-length
					     u16vector-set!
					     u16vector-ref))

(define s16vector-map (make-srfi4-vector-map make-s16vector
					     s16vector-length
					     s16vector-set!
					     s16vector-ref))

(define u32vector-map (make-srfi4-vector-map make-u32vector
					     u32vector-length
					     u32vector-set!
					     u32vector-ref))

(define s32vector-map (make-srfi4-vector-map make-s32vector
					     s32vector-length
					     s32vector-set!
					     s32vector-ref))

(define f32vector-map (make-srfi4-vector-map make-f32vector
					     f32vector-length
					     f32vector-set!
					     f32vector-ref))

(define f64vector-map (make-srfi4-vector-map make-f64vector
					     f64vector-length
					     f64vector-set!
					     f64vector-ref))



(define u8vector-range (make-srfi4-vector-range make-u8vector
						u8vector-length
						u8vector-set!
						u8vector-ref))

(define s8vector-range (make-srfi4-vector-range make-s8vector
						s8vector-length
						s8vector-set!
						s8vector-ref))

(define u16vector-range (make-srfi4-vector-range make-u16vector
						 u16vector-length
						 u16vector-set!
						 u16vector-ref))

(define s16vector-range (make-srfi4-vector-range make-s16vector
						 s16vector-length
						 s16vector-set!
						 s16vector-ref))

(define u32vector-range (make-srfi4-vector-range make-u32vector
						 u32vector-length
						 u32vector-set!
						 u32vector-ref))


(define s32vector-range (make-srfi4-vector-range make-s32vector
						 s32vector-length
						 s32vector-set!
						 s32vector-ref))

(define f32vector-range (make-srfi4-vector-range make-f32vector
						 f32vector-length
						 f32vector-set!
						 f32vector-ref))

(define f64vector-range (make-srfi4-vector-range make-f64vector
						 f64vector-length
						 f64vector-set!
						 f64vector-ref))

(define (check-string rank n c size)
  (print "rank = " rank " n = " n " size = " size)
  (and (= (length (string->list n)) (+ 1 size))
       (every (lambda (x) (char=? x c)) (string->list n))))

(MPI:init)


(define comm-world  (MPI:get-comm-world))
(define size        (MPI:comm-size comm-world))
(define myrank      (MPI:comm-rank comm-world))

(printf "rank ~A: Host ~A size ~A~%" myrank (get-host-name) size)

(define vsize       3)
(define intdata     (list-tabulate size (lambda (i) (* 10 i))))
(define flodata     (list-tabulate size (lambda (i) (* 0.1 i))))
(define vsdata      (list-tabulate size (lambda (i) 
					  (list->string (list-tabulate vsize 
									  (lambda (j) (integer->char (+ i 97))))))))
(define vvsdata     (list-tabulate size (lambda (i) 
					  (list->string (list-tabulate (+ i 1) 
								       (lambda (j) (integer->char (+ i 97))))))))
(define vintdata    (list-tabulate size (lambda (i) (list-tabulate vsize (lambda (j) (+ (* 10 i) j))))))
(define vflodata    (list-tabulate size (lambda (i) (list-tabulate vsize (lambda (j) (+ i (* 0.1 j)))))))
(define vvintdata   (list-tabulate size (lambda (i) (list-tabulate (+ i 1) (lambda (j) (+ (* 10 i) j))))))
(define vvflodata   (list-tabulate size (lambda (i) (list-tabulate (+ i 1) (lambda (j) (+ i (* 0.1 j)))))))


(test-group "MPI test"

  (if (zero? myrank)
      (let ((data  "aa"))
	(print myrank ": sending " data)
	(MPI:send (string->blob data) 1 0 comm-world)
	(let ((n (blob->string (MPI:receive MPI:any-source MPI:any-tag comm-world))))
	  (print myrank ": received " n)
	  (test-assert (check-string myrank n #\a size))))
      (let* ((n   (blob->string (MPI:receive MPI:any-source MPI:any-tag comm-world)))
	     (n1  (string-append n "a")))
	(print myrank ": received " n ", resending " n1)
	(MPI:send (string->blob n1) (modulo (+ myrank 1) size) 0 comm-world)
	(test-assert (check-string myrank n #\a myrank))
	))

  ;; Barrier
  (MPI:barrier comm-world)
  
  (if (zero? myrank)
      (let ((data1  "aa")
	    (data2  "bb"))
	(print myrank ": sending (tag 0) " data1)
	(MPI:send (string->blob data1) 1 0 comm-world)
	(print myrank ": sending (tag 1) " data2)
	(MPI:send (string->blob data2) 1 1 comm-world)
	(let-values (((n src tag)  (MPI:receive-with-status MPI:any-source MPI:any-tag comm-world)))
		    (print myrank ": received " (blob->string n) " (tag " tag ")" " from " src)
		    (if (zero? tag) 
			(test-assert (check-string myrank (blob->string n) #\a size))
			(test-assert (check-string myrank (blob->string n) #\b size)))
         (let-values (((n src tag)  (MPI:receive-with-status MPI:any-source MPI:any-tag comm-world)))
		     (print myrank ": received " (blob->string n) " (tag " tag ")" " from " src)
		     (if (zero? tag) 
			 (test-assert (check-string myrank (blob->string n) #\a size))
			 (test-assert (check-string myrank (blob->string n) #\b size))))))
      (let-values (((n1 src tag1)  (MPI:receive-with-status MPI:any-source 0 comm-world)))
	  (let* ((n1   (blob->string n1))
		 (nn1  (if (zero? tag1) (string-append n1 "a") (string-append n1 "b"))))
	     (print myrank ": received " n1 " (tag " tag1 ")" " from " src
		    ", resending " nn1)
	     (if (zero? tag1)
		 (test-assert (check-string myrank n1 #\a myrank))
		 (test-assert (check-string myrank n1 #\b myrank)))
	     (let-values (((n2 src tag2)  (MPI:receive-with-status MPI:any-source MPI:any-tag comm-world)))
			 (let* ((n2   (blob->string n2))
				(nn2  (if (zero? tag2) (string-append n2 "a") (string-append n2 "b"))))
			   (if (zero? tag2)
			       (test-assert (check-string myrank n2 #\a myrank))
			       (test-assert (check-string myrank n2 #\b myrank)))
			   (print myrank ": received " n2 " (tag " tag2 ")" " from " src
				  ", resending " nn2)
			   (MPI:send (string->blob nn1) (modulo (+ 1 myrank) size) tag1 comm-world)
			   (MPI:send (string->blob nn2) (modulo (+ 1 myrank) size) tag2 comm-world))))))
  
  ;; Barrier
  (MPI:barrier comm-world)
   (let ((test-send-recv
	  (lambda (sendfun recvfun transf data)
	   (if (zero? myrank)
	       (begin
		 (print myrank ": test-send-recv: data = " data) 
		 (print myrank ": test-send-recv: size = " size) 
		 (let loop ((lst data) (i 1))
		   (if (and (not (null? lst)) (< i size))
		       (begin
			 (print myrank ": sending " (car lst) " to " i)
			 (sendfun (car lst) i 0 comm-world)
			 (loop (cdr lst) (+ 1 i)))))
		 (let loop ((i size))
		   (if (positive? (- i 1))
		       (let ((x (recvfun (- i 1) 0 comm-world)))
			 (print myrank ": received " x)
			 (test-assert (any (lambda (y) (equal? x y)) (map transf data)))
			 (loop (- i 1))))))
	       (let ((x (recvfun 0 0 comm-world)))
		 (print myrank ": received " x)
		 (test-assert (member x data))
		 (let ((y (transf x)))
		   (sendfun y 0 0 comm-world))))
	   (MPI:barrier comm-world))))
     (test-send-recv MPI:send-fixnum MPI:receive-fixnum (lambda (x) (+ 1 x)) intdata)
     (test-send-recv MPI:send-int MPI:receive-int (lambda (x) (+ 1 x)) intdata)
     (test-send-recv MPI:send-flonum MPI:receive-flonum (lambda (x) (* 2 x)) flodata)
    (let ((srfi4-test-send-recv
	   (lambda (len vsend vreceive vmap list->vector)
	     (lambda (data)
	       (test-send-recv vsend
			       (lambda (src tag comm) (vreceive len src tag comm))
			       (lambda (v) (vmap v (lambda (x) (+ 1 x))))
			       (map list->vector data))))))
      ((srfi4-test-send-recv vsize MPI:send-u8vector MPI:receive-u8vector u8vector-map list->u8vector)
       vintdata)
       ((srfi4-test-send-recv vsize MPI:send-s8vector MPI:receive-s8vector s8vector-map list->s8vector)
        vintdata)
       ((srfi4-test-send-recv vsize MPI:send-u16vector MPI:receive-u16vector u16vector-map list->u16vector)
        vintdata)
       ((srfi4-test-send-recv vsize MPI:send-s16vector MPI:receive-s16vector s16vector-map list->s16vector)
        vintdata)
       ((srfi4-test-send-recv vsize MPI:send-u32vector MPI:receive-u32vector u32vector-map list->u32vector)
        vintdata)
       ((srfi4-test-send-recv vsize MPI:send-s32vector MPI:receive-s32vector s32vector-map list->s32vector)
        vintdata)
       ((srfi4-test-send-recv vsize MPI:send-f32vector MPI:receive-f32vector f32vector-map list->f32vector)
        vflodata)
       ((srfi4-test-send-recv vsize MPI:send-f64vector MPI:receive-f64vector f64vector-map list->f64vector)
        vflodata)
      ))

   (begin
     (if (positive? myrank)
	 (sleep myrank))
     (print myrank ": hitting barrier")
     (MPI:barrier comm-world)
     (if (zero? myrank)
	 (print "jumped barrier")))

    ;;  Broadcast 
   (let* ((test-broadcast
	   (lambda (bcast data)
	     (if (zero? myrank)
		 (print myrank ": broadcasting " data))
	     (let ((res (bcast data 0 comm-world)))
	       (print myrank ": received " (if (blob? res) (blob->string res) res))
	       (test-assert (equal? res data))
	       (MPI:barrier comm-world)))))
     (test-broadcast MPI:broadcast-bytevector (string->blob "Hello!"))
     (test-broadcast MPI:broadcast-int 123456)
     (test-broadcast MPI:broadcast-flonum 3.141592654)
     (let ((intdata  (list 12 45 78))
	   (flodata  (list 3.14 2.718 0.578))
	   (srfi4-test-broadcast
	    (lambda (bcast list->vector data)
	      (test-broadcast bcast (list->vector data)))))
       (srfi4-test-broadcast MPI:broadcast-s8vector  list->s8vector  intdata)
       (srfi4-test-broadcast MPI:broadcast-u8vector  list->u8vector  intdata)
       (srfi4-test-broadcast MPI:broadcast-s16vector list->s16vector intdata)
       (srfi4-test-broadcast MPI:broadcast-u16vector list->u16vector intdata)
       (srfi4-test-broadcast MPI:broadcast-s32vector list->s32vector intdata)
       (srfi4-test-broadcast MPI:broadcast-u32vector list->u32vector intdata)
       (srfi4-test-broadcast MPI:broadcast-f32vector list->f32vector flodata)
       (srfi4-test-broadcast MPI:broadcast-f64vector list->f64vector flodata)))

  ;; Scatter
   (let* ((test-scatter
	   (lambda (scatter vrange data)
	     (if (zero? myrank)
		 (print myrank ": scatter " (if (blob? data) (blob->string data) data)))
	     (let ((res (scatter data 3 0 comm-world)))
	       (print myrank ": received (scatter) " (if (blob? res) (blob->string res) res))
		(test-assert
		 (equal? res (vrange data (* myrank vsize) (+ vsize (* myrank vsize))))))
	     (MPI:barrier comm-world))))
     (test-scatter MPI:scatter-bytevector blob-range (string->blob (string-concatenate vsdata)))

     (let ((srfi4-test-scatter
 	    (lambda (scatter vrange list->vector data)
 	      (test-scatter scatter vrange (list->vector (concatenate data))))))
        (srfi4-test-scatter MPI:scatter-s8vector  s8vector-range  list->s8vector  vintdata)
        (srfi4-test-scatter MPI:scatter-u8vector  u8vector-range  list->u8vector  vintdata)
        (srfi4-test-scatter MPI:scatter-s16vector s16vector-range list->s16vector vintdata)
        (srfi4-test-scatter MPI:scatter-u16vector u16vector-range list->u16vector vintdata)
        (srfi4-test-scatter MPI:scatter-s32vector s32vector-range list->s32vector vintdata)
        (srfi4-test-scatter MPI:scatter-u32vector u32vector-range list->u32vector vintdata)
        (srfi4-test-scatter MPI:scatter-f32vector f32vector-range list->f32vector vflodata)
        (srfi4-test-scatter MPI:scatter-f64vector f64vector-range list->f64vector vflodata)))


 ;;  Scatterv
   (let* ((test-scatterv
	   (lambda (scatterv data)
	     (if (zero? myrank)
		 (print myrank ": scatterv " data))
	     (let ((res (scatterv data 0 comm-world)))
	       (print myrank ": received (scatterv) " res)
		(test res (list-ref data myrank)))
	     (MPI:barrier comm-world))))
     (test-scatterv MPI:scatterv-bytevector (map string->blob vvsdata))
     (let ((srfi4-test-scatterv
	    (lambda (scatterv list->vector data)
	      (test-scatterv scatterv (map list->vector data)))))
       (srfi4-test-scatterv MPI:scatterv-s8vector   list->s8vector  vvintdata)
       (srfi4-test-scatterv MPI:scatterv-u8vector   list->u8vector  vvintdata)
       (srfi4-test-scatterv MPI:scatterv-s16vector  list->s16vector vvintdata)
       (srfi4-test-scatterv MPI:scatterv-u16vector  list->u16vector vvintdata)
       (srfi4-test-scatterv MPI:scatterv-s32vector  list->s32vector vvintdata)
       (srfi4-test-scatterv MPI:scatterv-u32vector  list->u32vector vvintdata)
       (srfi4-test-scatterv MPI:scatterv-f32vector  list->f32vector vvflodata)
       (srfi4-test-scatterv MPI:scatterv-f64vector  list->f64vector vvflodata)))

  ;; Gather
   (let* ((test-gather
	   (lambda (gather data total)
	     (print myrank ": gather " (if (blob? data) (blob->string data) data))
	     (let ((res (gather data 3 0 comm-world)))
	       (if (zero? myrank)
		   (begin
		     (print myrank ": received (gather) " (if (blob? res) (blob->string res) res))
		     (test res total))))
	       (MPI:barrier comm-world))))
     (test-gather MPI:gather-bytevector (string->blob (list-ref vsdata myrank)) 
		  (string->blob (string-concatenate vsdata)))
     (test-gather MPI:gather-s8vector   (list->s8vector (list-ref vintdata myrank))
		  (list->s8vector (concatenate vintdata)))
     (test-gather MPI:gather-u8vector   (list->u8vector (list-ref vintdata myrank))
		  (list->u8vector (concatenate vintdata)))
     (test-gather MPI:gather-s16vector  (list->s16vector (list-ref vintdata myrank))
		  (list->s16vector (concatenate vintdata)))
     (test-gather MPI:gather-u16vector  (list->u16vector (list-ref vintdata myrank))
		  (list->u16vector (concatenate vintdata)))
     (test-gather MPI:gather-s32vector  (list->s32vector (list-ref vintdata myrank))
		  (list->s32vector (concatenate vintdata)))
     (test-gather MPI:gather-u32vector  (list->u32vector (list-ref vintdata myrank))
		  (list->u32vector (concatenate vintdata)))
     (test-gather MPI:gather-f32vector  (list->f32vector (list-ref vflodata myrank))
		  (list->f32vector (concatenate vflodata)))
     (test-gather MPI:gather-f64vector  (list->f64vector (list-ref vflodata myrank))
		  (list->f64vector (concatenate vflodata))))


  ;; Gatherv
   (let* ((test-gatherv
	   (lambda (gatherv data total)
	     (print myrank ": gatherv " (if (blob? data) (blob->string data) data))
	     (let ((res (gatherv data 0 comm-world)))
	       (if (zero? myrank)
		   (begin
		     (print myrank ": received (gatherv) " 
			    (map (lambda (x) (if (blob? x) (blob->string x) x)) res))
		     (test res total))))
	       (MPI:barrier comm-world))))
     (test-gatherv MPI:gatherv-bytevector (string->blob (list-ref vvsdata myrank))
		   (map string->blob vvsdata))
     (test-gatherv MPI:gatherv-s8vector   (list->s8vector (list-ref vvintdata myrank))
		   (map list->s8vector vvintdata))
     (test-gatherv MPI:gatherv-u8vector   (list->u8vector (list-ref vvintdata myrank))
		   (map list->u8vector vvintdata))
     (test-gatherv MPI:gatherv-s16vector  (list->s16vector (list-ref vvintdata myrank))
		   (map list->s16vector vvintdata))
     (test-gatherv MPI:gatherv-u16vector  (list->u16vector (list-ref vvintdata myrank))
		   (map list->u16vector vvintdata))
     (test-gatherv MPI:gatherv-s32vector  (list->s32vector (list-ref vvintdata myrank))
		   (map list->s32vector vvintdata))
     (test-gatherv MPI:gatherv-u32vector  (list->u32vector (list-ref vvintdata myrank))
		   (map list->u32vector vvintdata))
     (test-gatherv MPI:gatherv-f32vector  (list->f32vector (list-ref vvflodata myrank))
		   (map list->f32vector vvflodata))
     (test-gatherv MPI:gatherv-f64vector  (list->f64vector (list-ref vvflodata myrank))
		   (map list->f64vector vvflodata)))


  ;; Gather to all 

   (let ((data (list-ref intdata myrank)))
     (print myrank ": allgather-int " data)
     (let ((res (MPI:allgather-int data 0 comm-world)))
       (print myrank ": received (allgather-int) " res)
       (test intdata (s32vector->list res))
       (MPI:barrier comm-world)
       ))

   (let ((data (list-ref flodata myrank)))
     (print myrank ": allgather-flonum " data)
     (let ((res (MPI:allgather-flonum data 0 comm-world)))
       (print myrank ": received (allgather-flonum) " res)
       (test flodata (f64vector->list res))
       (MPI:barrier comm-world)
       ))

   (let* ((test-allgather 
	   (lambda (allgather data total)
	     (print myrank ": allgather " data)
	     (let ((res (allgather data 0 comm-world)))
	       (print myrank ": received (allgather) " 
		      (map (lambda (x) (if (blob? x) (blob->string x) x)) res))
	       (test res total))
	     (MPI:barrier comm-world))))
     (test-allgather MPI:allgather-bytevector (string->blob (list-ref vvsdata myrank))
		   (map string->blob vvsdata))
     (test-allgather MPI:allgather-s8vector   (list->s8vector (list-ref vvintdata myrank))
		   (map list->s8vector vvintdata))
     (test-allgather MPI:allgather-u8vector   (list->u8vector (list-ref vvintdata myrank))
		   (map list->u8vector vvintdata))
     (test-allgather MPI:allgather-s16vector  (list->s16vector (list-ref vvintdata myrank))
		   (map list->s16vector vvintdata))
     (test-allgather MPI:allgather-u16vector  (list->u16vector (list-ref vvintdata myrank))
		   (map list->u16vector vvintdata))
     (test-allgather MPI:allgather-s32vector  (list->s32vector (list-ref vvintdata myrank))
		   (map list->s32vector vvintdata))
     (test-allgather MPI:allgather-u32vector  (list->u32vector (list-ref vvintdata myrank))
		   (map list->u32vector vvintdata))
     (test-allgather MPI:allgather-f32vector  (list->f32vector (list-ref vvflodata myrank))
		   (map list->f32vector vvflodata))
     (test-allgather MPI:allgather-f64vector  (list->f64vector (list-ref vvflodata myrank))
		   (map list->f64vector vvflodata)))


  ;; Reduce 
   (let* ((test-reduce 
	  (lambda (reducefun reduceops data)
	    (for-each (lambda (op)
			(print myrank ": reduce")
			(let ((res (reducefun data op 0 comm-world)))
			  (if (zero? myrank)
			      (begin
				(print myrank ": the result of reduction " op " is " res)
				(test-assert res)
				))
			  (MPI:barrier comm-world)
			  ))
		      reduceops)
	    (MPI:barrier comm-world))))
    (test-reduce MPI:reduce-int
		 (list MPI:i_max MPI:i_min MPI:i_sum MPI:i_prod )
		 (+ 1 myrank))
    (test-reduce MPI:reduce-flonum
		 (list MPI:f_max MPI:f_min MPI:f_sum MPI:f_prod )
		 (+ 1 myrank))
    (test-reduce MPI:reduce-s8vector 
  		 (list MPI:i_max MPI:i_min MPI:i_sum MPI:i_prod )
  		 (s8vector (* 2 myrank) (+ 1 (* 2 myrank)) (+ 2 (* 2 myrank))))
    (test-reduce MPI:reduce-u8vector 
  		 (list MPI:i_max MPI:i_min MPI:i_sum MPI:i_prod )
  		 (u8vector (* 2 myrank) (+ 1 (* 2 myrank)) (+ 2 (* 2 myrank))))
    (test-reduce MPI:reduce-s16vector 
  		 (list MPI:i_max MPI:i_min MPI:i_sum MPI:i_prod )
  		 (s16vector (* 2 myrank) (+ 1 (* 2 myrank)) (+ 2 (* 2 myrank))))
    (test-reduce MPI:reduce-u16vector 
  		 (list MPI:i_max MPI:i_min MPI:i_sum MPI:i_prod )
  		 (u16vector (* 2 myrank) (+ 1 (* 2 myrank)) (+ 2 (* 2 myrank))))
    (test-reduce MPI:reduce-s32vector 
  		 (list MPI:i_max MPI:i_min MPI:i_sum MPI:i_prod )
  		 (s32vector (* 2 myrank) (+ 1 (* 2 myrank)) (+ 2 (* 2 myrank))))
    (test-reduce MPI:reduce-u32vector 
  		 (list MPI:i_max MPI:i_min MPI:i_sum MPI:i_prod )
  		 (u32vector (* 2 myrank) (+ 1 (* 2 myrank)) (+ 2 (* 2 myrank))))
    (test-reduce MPI:reduce-f32vector 
  		 (list MPI:f_max MPI:f_min MPI:f_sum MPI:f_prod )
  		 (f32vector (* 2 myrank) (+ 0.1 (* 2 myrank)) (+ 0.2 (* 2 myrank))))
    (test-reduce MPI:reduce-f64vector 
  		 (list MPI:f_max MPI:f_min MPI:f_sum MPI:f_prod )
  		 (f64vector (* 2 myrank) (+ 0.1 (* 2 myrank)) (+ 0.2 (* 2 myrank))))
    )

  ;; Reduce all 
   (let* ((test-allreduce
 	  (lambda (allreducefun reduceop data)
	    (print myrank ": data is " data)
	    (let ((res (allreducefun data reduceop comm-world)))
	      (MPI:barrier comm-world)
	      (print myrank ": the result of reduction " reduceop " is " res)
	      (test-assert res)
	      (MPI:barrier comm-world)))))
    (test-allreduce MPI:allreduce-int MPI:i_sum (+ 1 myrank))
    (test-allreduce MPI:allreduce-flonum MPI:f_prod (+ 1.0 myrank))
    (test-allreduce MPI:allreduce-s8vector MPI:i_sum
		    (s8vector (* 2 myrank) (+ 1 (* 2 myrank)) (+ 2 (* 2 myrank))))
    (test-allreduce MPI:allreduce-u8vector MPI:i_sum
		    (u8vector (* 2 myrank) (+ 1 (* 2 myrank)) (+ 2 (* 2 myrank))))
    (test-allreduce MPI:allreduce-s16vector MPI:i_sum
		    (s16vector (* 2 myrank) (+ 1 (* 2 myrank)) (+ 2 (* 2 myrank))))
    (test-allreduce MPI:allreduce-u16vector MPI:i_sum
		    (u16vector (* 2 myrank) (+ 1 (* 2 myrank)) (+ 2 (* 2 myrank))))
    (test-allreduce MPI:allreduce-s32vector MPI:i_sum
		    (s32vector (* 2 myrank) (+ 1 (* 2 myrank)) (+ 2 (* 2 myrank))))
    (test-allreduce MPI:allreduce-u32vector MPI:i_sum
		    (u32vector (* 2 myrank) (+ 1 (* 2 myrank)) (+ 2 (* 2 myrank))))
    (test-allreduce MPI:allreduce-f32vector MPI:f_sum  
		    (f32vector (* 2 myrank) (+ 0.1 (* 2 myrank)) (+ 0.2 (* 2 myrank))))
    (test-allreduce MPI:allreduce-f64vector MPI:f_sum  
		    (f64vector (* 2 myrank) (+ 0.1 (* 2 myrank)) (+ 0.2 (* 2 myrank)))))
    
   ;; Scan
   (let* ((test-scan
	  (lambda (scanfun reduceop data)
	    (print myrank ": data is " data)
	    (let ((res (scanfun data reduceop comm-world)))
	      (MPI:barrier comm-world)
	      (print myrank ": the result of scan " reduceop " is " res)
	      (test-assert res))
	      (MPI:barrier comm-world))))
    (test-scan MPI:scan-int MPI:i_sum (+ 1 myrank))
    (test-scan MPI:scan-flonum MPI:f_prod (+ 1.0 myrank))
    (test-scan MPI:scan-s8vector MPI:i_sum
	       (s8vector (* 2 myrank) (+ 1 (* 2 myrank)) (+ 2 (* 2 myrank))))
    (test-scan MPI:scan-u8vector MPI:i_sum
	       (u8vector (* 2 myrank) (+ 1 (* 2 myrank)) (+ 2 (* 2 myrank))))
    (test-scan MPI:scan-s16vector MPI:i_sum
	       (s16vector (* 2 myrank) (+ 1 (* 2 myrank)) (+ 2 (* 2 myrank))))
    (test-scan MPI:scan-u16vector MPI:i_sum
	       (u16vector (* 2 myrank) (+ 1 (* 2 myrank)) (+ 2 (* 2 myrank))))
    (test-scan MPI:scan-s32vector MPI:i_sum
	       (s32vector (* 2 myrank) (+ 1 (* 2 myrank)) (+ 2 (* 2 myrank))))
    (test-scan MPI:scan-u32vector MPI:i_sum
	       (u32vector (* 2 myrank) (+ 1 (* 2 myrank)) (+ 2 (* 2 myrank))))
    (test-scan MPI:scan-f32vector MPI:f_sum  
	       (f32vector (* 2 myrank) (+ 0.1 (* 2 myrank)) (+ 0.2 (* 2 myrank))))
    (test-scan MPI:scan-f64vector MPI:f_sum  
	       (f64vector (* 2 myrank) (+ 0.1 (* 2 myrank)) (+ 0.2 (* 2 myrank)))))

  ;; Comm split
   (let ((send-in-comm
	 (lambda (c init incr)
	   (let ((rank-in-c (MPI:comm-rank c))
		 (size-of-c (MPI:comm-size c)))
	     (if (zero? rank-in-c)
		 (begin
		   (print rank-in-c "[" myrank "]: sending " init)
		   (MPI:send init 1 0 c)
		   (let ((n (MPI:receive MPI:any-source MPI:any-tag c)))
		     (print rank-in-c "[" myrank "]: received " n)))
		 (let ((n (MPI:receive MPI:any-source MPI:any-tag c)))
		   (let ((n1 (string->blob (string-append (blob->string n) incr))))
		     (print rank-in-c "[" myrank "]: received " n ", resending " n1)
		     (MPI:send n1 (modulo (+ 1 rank-in-c) size-of-c) 0 c))))
	     (MPI:barrier comm-world)))))
    (let* ((color (if (< size 4) 0 (modulo myrank 2)))
	   (c (MPI:comm-split comm-world color 0)))
      (if (zero? (modulo myrank 2))
	  (send-in-comm c (string->blob "aa") "a")
	  (send-in-comm c (string->blob "bb") "b"))))

  ;; Cartesian topology
   (if (>= size 4)
       (let ((cart (MPI:make-cart comm-world (u32vector 2 2) (u32vector 0 0) #t))
             (test-dims-create 
              (lambda (n hints)
                (print "make-dims " n " " hints " = " (MPI:make-dims n hints)))))
         (if (zero? myrank)
             (begin
               (print "ranks = " (map (lambda (x) (cons x (MPI:cart-rank cart x)))
                                      (list
                                       (u32vector 0 0) (u32vector 1 0)
                                       (u32vector 1 0) (u32vector 1 1))))
               (print "coords = " (list-tabulate (MPI:comm-size cart)
                                                 (lambda (n) (cons n (MPI:cart-coords cart n)))))
               (test-dims-create 60 (u32vector 0 0 0))
               (test-dims-create 60 (u32vector 3 0 0))
               (test-dims-create 60 (u32vector 0 4 0))
               (test-dims-create 60 (u32vector 3 0 5))
               ))
         ))

  (MPI:barrier comm-world)
  ;;   Wtime
  (print myrank ": wtime is "  (MPI:wtime))

  (print myrank ": rr-fold result is " (MPI-rr-fold (lambda (x ax) (cons x ax)) '() (list-tabulate (* 4 size) identity)))

  (MPI:finalize)
  )
(test-exit)
