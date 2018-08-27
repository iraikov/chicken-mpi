
;;
;; Chicken MPI interface. Based on the Caml/MPI interface by Xavier
;; Leroy.
;;
;;
;; Copyright 2007-2018 Ivan Raikov.
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


(module mpi

 (MPI:barrier

  MPI:broadcast-int 
  MPI:scatter-int 
  MPI:gather-int 
  MPI:allgather-int
  MPI:broadcast-flonum 
  MPI:scatter-flonum 
  MPI:gather-flonum 
  MPI:allgather-flonum
  MPI:broadcast-fixnum 
  
  MPI:broadcast
  MPI:scatter 
  MPI:scatterv
  MPI:gather 
  MPI:gatherv 
  MPI:allgather
  MPI:alltoall
  MPI:alltoallv
  
  MPI:broadcast-bytevector
  MPI:scatter-bytevector 
  MPI:scatterv-bytevector
  MPI:gather-bytevector 
  MPI:gatherv-bytevector 
  MPI:allgather-bytevector
  MPI:alltoall-bytevector
  MPI:alltoallv-bytevector
  
  MPI:broadcast-s8vector
  MPI:scatter-s8vector 
  MPI:scatterv-s8vector
  MPI:gather-s8vector 
  MPI:gatherv-s8vector 
  MPI:allgather-s8vector
  MPI:alltoall-s8vector
  MPI:alltoallv-s8vector
  
  MPI:broadcast-u8vector
  MPI:scatter-u8vector 
  MPI:scatterv-u8vector
  MPI:gather-u8vector 
  MPI:gatherv-u8vector 
  MPI:allgather-u8vector
  MPI:alltoall-u8vector
  MPI:alltoallv-u8vector

  MPI:broadcast-s16vector
  MPI:scatter-s16vector 
  MPI:scatterv-s16vector
  MPI:gather-s16vector 
  MPI:gatherv-s16vector 
  MPI:allgather-s16vector
  MPI:alltoall-s16vector
  MPI:alltoallv-s16vector
  
  MPI:broadcast-u16vector
  MPI:scatter-u16vector 
  MPI:scatterv-u16vector
  MPI:gather-u16vector 
  MPI:gatherv-u16vector 
  MPI:allgather-u16vector
  MPI:alltoall-u16vector
  MPI:alltoallv-u16vector
  
  MPI:broadcast-s32vector
  MPI:scatter-s32vector 
  MPI:scatterv-s32vector
  MPI:gather-s32vector 
  MPI:gatherv-s32vector 
  MPI:allgather-s32vector
  MPI:alltoall-s32vector
  MPI:alltoallv-s32vector
  
  MPI:broadcast-u32vector
  MPI:scatter-u32vector 
  MPI:scatterv-u32vector
  MPI:gather-u32vector 
  MPI:gatherv-u32vector 
  MPI:allgather-u32vector
  MPI:alltoall-u32vector
  MPI:alltoallv-u32vector
  
  MPI:broadcast-f32vector
  MPI:scatter-f32vector 
  MPI:scatterv-f32vector
  MPI:gather-f32vector 
  MPI:gatherv-f32vector 
  MPI:allgather-f32vector
  MPI:alltoall-f32vector
  MPI:alltoallv-f32vector
  
  MPI:broadcast-f64vector
  MPI:scatter-f64vector 
  MPI:scatterv-f64vector
  MPI:gather-f64vector 
  MPI:gatherv-f64vector 
  MPI:allgather-f64vector
  MPI:alltoall-f64vector
  MPI:alltoallv-f64vector
  
  MPI:reduce-int
  MPI:reduce-flonum
  MPI:allreduce-int
  MPI:allreduce-flonum
  MPI:scan-int
  MPI:scan-flonum
  
  MPI:reduce-s8vector
  MPI:allreduce-s8vector
  MPI:scan-s8vector
  
  MPI:reduce-u8vector
  MPI:allreduce-u8vector
  MPI:scan-u8vector
  
  MPI:reduce-s16vector
  MPI:allreduce-s16vector
  MPI:scan-s16vector
  
  MPI:reduce-u16vector
  MPI:allreduce-u16vector
  MPI:scan-u16vector
  
  MPI:reduce-s32vector
  MPI:allreduce-s32vector
  MPI:scan-s32vector
  MPI:reduce-u32vector
  MPI:allreduce-u32vector
  MPI:scan-u32vector
  
  MPI:reduce-f32vector
  MPI:allreduce-f32vector
  MPI:scan-f32vector
  
  MPI:reduce-f64vector
  MPI:allreduce-f64vector
  MPI:scan-f64vector
  
  MPI:comm? 
  MPI:get-comm-world
  MPI:comm-size
  MPI:comm-rank
  MPI:comm-equal? 
  MPI:comm-split
  MPI:comm-create
  MPI:undefined 
  MPI:make-cart
  MPI:make-dims
  MPI:cart-rank
  MPI:cart-coords

  MPI:group? 
  MPI:group-size
  MPI:group-rank
  MPI:group-translate-ranks
  MPI:comm-group 
  MPI:group-union
  MPI:group-difference
  MPI:group-intersection
  MPI:group-incl 
  MPI:group-excl 
  MPI:group-range-incl
  MPI:group-range-excl

  MPI:datatype?
  MPI:type-extent
  MPI:type-size
  MPI:type-char
  MPI:type-int
  MPI:type-fixnum
  MPI:type-flonum
  MPI:type-byte
  MPI:type-s8
  MPI:type-u8
  MPI:type-s16
  MPI:type-u16
  MPI:type-s32
  MPI:type-u32
  MPI:type-f32
  MPI:type-f64
  MPI:make-type-struct
  MPI:pack-size
  
  MPI:init
  MPI:spawn
  MPI:finalize 
  MPI:wtime 

  MPI:send
  MPI:send-fixnum
  MPI:send-int 
  MPI:send-flonum
  MPI:send-u8vector
  MPI:send-s8vector
  MPI:send-u16vector
  MPI:send-s16vector
  MPI:send-u32vector
  MPI:send-s32vector
  MPI:send-f32vector
  MPI:send-f64vector
  MPI:send-bytevector
  MPI:probe 
  MPI:receive 
  MPI:receive-with-status 
  MPI:receive-bytevector 
  MPI:receive-bytevector-with-status 
  MPI:receive-flonum 
  MPI:receive-fixnum
  MPI:receive-int
  MPI:receive-u8vector
  MPI:receive-s8vector
  MPI:receive-u16vector
  MPI:receive-s16vector
  MPI:receive-u32vector
  MPI:receive-s32vector
  MPI:receive-f32vector
  MPI:receive-f64vector

  MPI:any-tag
  MPI:any-source
  
  MPI:i_max
  MPI:i_min
  MPI:i_sum
  MPI:i_prod
  MPI:i_land
  MPI:i_lor
  MPI:i_xor
  
  MPI:f_max
  MPI:f_min
  MPI:f_sum
  MPI:f_prod

  MPI-rr-fold MPI-rr-foldi MPI-rr-map MPI-rr-for-each
  )
		   
 (import scheme (chicken base) (chicken foreign) (chicken blob)
         (only (chicken string) ->string)
         srfi-1 srfi-4)

#>
#include "chicken-mpi.h"


<#

(include "init")
(include "datatype")
(include "group")
(include "comm")
(include "msgs")
(include "collcomm")

;; MPI round-robin fold/map/for-each

(define (MPI-rr-fold fn initial xs #!key (comm (MPI:get-comm-world)))

  (let (
	(size        (MPI:comm-size comm))
	(myrank      (MPI:comm-rank comm))
	)

    (cdr 
     (fold
     
     (lambda (x myindex.ax)
       
       (let ((myindex (car myindex.ax))
	     (ax      (cdr myindex.ax)))

	 (cond 
	  ((= myindex myrank) 
	   (let ((ax1 (fn x ax))
		 (myindex1 (if (= myindex (- size 1)) 0 (+ 1 myindex))))
	     (cons myindex1 ax1)))
	  (else (cons (if (= myindex (- size 1)) 0 (+ 1 myindex)) ax))
	  )
	 ))
     
     (cons 0 initial) xs))
    ))


(define (MPI-rr-foldi fn initial is xs #!key (comm (MPI:get-comm-world)))

  (let (
	(size        (MPI:comm-size comm))
	(myrank      (MPI:comm-rank comm))
	)

    (cdr 
     (fold
     
     (lambda (x-i x myindex.ax)
       
       (let ((myindex (car myindex.ax))
	     (ax      (cdr myindex.ax)))

	 (cond 
	  ((= myindex myrank) 
	   (let ((ax1 (fn x-i x ax))
		 (myindex1 (if (= myindex (- size 1)) 0 (+ 1 myindex))))
	     (cons myindex1 ax1)))
	  (else (cons (if (= myindex (- size 1)) 0 (+ 1 myindex)) ax))
	  )
	 ))
     
     (cons 0 initial) is xs))
    ))


(define (MPI-rr-map fn xs #!key (comm (MPI:get-comm-world)))

  (let (
	 (size        (MPI:comm-size comm))
	 (myrank      (MPI:comm-rank comm))
	 )

    (cdr 
     (fold
      
      (lambda (x myindex.ax)
       
	(let ((myindex (car myindex.ax))
	     (ax      (cdr myindex.ax)))

	 (cond 
	  ((= myindex myrank) 
	   (let ((x1 (fn x))
		 (myindex1 (if (= myindex (- size 1)) 0 (+ 1 myindex))))
	     (cons myindex1 (cons x1 ax))))
	  (else (cons (if (= myindex (- size 1)) 0 (+ 1 myindex)) ax))
	  )
	 ))
     
     (cons 0 '()) xs))
    ))
	     

(define (MPI-rr-for-each fn xs #!key (comm (MPI:get-comm-world)))

  (let (
	(size        (MPI:comm-size comm))
	(myrank      (MPI:comm-rank comm))
	)
    
    (fold
     
     (lambda (x myindex)
       
       (cond 
	((= myindex myrank) 
	 (let ((myindex1 (if (= myindex (- size 1)) 0 (+ 1 myindex))))
	   (fn x)
	   myindex1))
	(else (if (= myindex (- size 1)) 0 (+ 1 myindex)))
	))
     
     0 xs)
    ))
	     
	     


)
