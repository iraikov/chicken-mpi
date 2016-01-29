
;;
;; Chicken MPI interface. Based on the Caml/MPI interface by Xavier
;; Leroy.
;;
;; Copyright 2007-2016 Ivan Raikov.
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


;; Group communication 

;; Barrier synchronization 

; Include into generated code, but don't parse:
#>

C_word MPI_barrier(C_word comm)
{
  MPI_check_comm (comm);

  MPI_Barrier(Comm_val(comm));
  C_return (C_SCHEME_UNDEFINED);
}
<#

(define MPI:barrier (foreign-lambda scheme-object "MPI_barrier" scheme-object))


;; Broadcast 


(define MPI:broadcast-fixnum 
    (foreign-primitive scheme-object ((integer data)
				      (integer root)
				      (scheme-object comm))
#<<END
  C_word result; int n; 

  MPI_check_comm(comm);

  n = data;

  MPI_Bcast(&n, 1, MPI_INT, root, Comm_val(comm));

  result = C_fix(n);

  C_return(result);
END
))


(define MPI:broadcast-int 
    (foreign-primitive scheme-object ((integer data)
				      (integer root)
				      (scheme-object comm))
#<<END
  C_word result;
  long n; C_word *ptr;

  MPI_check_comm(comm);

  n = data;

  MPI_Bcast(&n, 1, MPI_LONG, root, Comm_val(comm));

  ptr = C_alloc (C_SIZEOF_FLONUM);
  result = C_long_to_num (&ptr, n);

  C_return(result);
END
))


(define MPI:broadcast-flonum 
    (foreign-primitive scheme-object ((double data)
				      (integer root)
				      (scheme-object comm))
#<<END
  C_word result;
  double n; C_word *ptr;

  MPI_check_comm(comm);

  n = data;

  MPI_Bcast(&n, 1, MPI_DOUBLE, root, Comm_val(comm));

  ptr = C_alloc (C_SIZEOF_FLONUM);
  result = C_flonum (&ptr, n);

  C_return(result);
END
))

#>

C_word MPI_broadcast_bytevector(C_word data, C_word root, C_word comm)
{
  int vroot, len; char *vect;

  MPI_check_comm (comm);
  C_i_check_bytevector (data);

  vroot = (int)C_num_to_int (root);
  len   = C_bytevector_length(data);
  vect  = C_c_bytevector (data);

  MPI_Bcast(vect, len, MPI_BYTE, vroot, Comm_val(comm));

  C_return (data);
}

C_word MPI_broadcast_u8vector (C_word data, C_word root, C_word comm)
{
  unsigned char *vect; int len, vroot;

  MPI_check_comm(comm);

  vect  = C_c_u8vector(data);
  len   = C_8vector_length(data);
  vroot = (int)C_num_to_int (root);

  MPI_Bcast(vect, len, MPI_UNSIGNED_CHAR, vroot, Comm_val(comm));

  C_return(data);
}


C_word MPI_broadcast_s8vector (C_word data, C_word root, C_word comm)
{
  char *vect; int len, vroot;

  MPI_check_comm(comm);

  vect  = C_c_s8vector(data);
  len   = C_8vector_length(data);
  vroot = (int)C_num_to_int (root);

  MPI_Bcast(vect, len, MPI_SIGNED_CHAR, vroot, Comm_val(comm));

  C_return(data);
}


C_word MPI_broadcast_u16vector (C_word data, C_word root, C_word comm)
{
  unsigned short *vect; int len, vroot;

  MPI_check_comm(comm);

  vect  = C_c_u16vector(data);
  len   = C_16vector_length(data);
  vroot = (int)C_num_to_int (root);

  MPI_Bcast(vect, len, MPI_UNSIGNED_SHORT, vroot, Comm_val(comm));

  C_return(data);
}


C_word MPI_broadcast_s16vector (C_word data, C_word root, C_word comm)
{
  short *vect; int len, vroot;

  MPI_check_comm(comm);

  vect  = C_c_s16vector(data);
  len   = C_16vector_length(data);
  vroot = (int)C_num_to_int (root);

  MPI_Bcast(vect, len, MPI_SHORT, vroot, Comm_val(comm));

  C_return(data);
}


C_word MPI_broadcast_u32vector (C_word data, C_word root, C_word comm)
{
  unsigned int *vect; int len, vroot;

  MPI_check_comm(comm);

  vect  = C_c_u32vector(data);
  len   = C_32vector_length(data);
  vroot = (int)C_num_to_int (root);

  MPI_Bcast(vect, len, MPI_UNSIGNED, vroot, Comm_val(comm));

  C_return(data);
}


C_word MPI_broadcast_s32vector (C_word data, C_word root, C_word comm)
{
  int *vect; int len, vroot;

  MPI_check_comm(comm);

  vect  = C_c_s32vector(data);
  len   = C_32vector_length(data);
  vroot = (int)C_num_to_int (root);

  MPI_Bcast(vect, len, MPI_INT, vroot, Comm_val(comm));

  C_return(data);
}


C_word MPI_broadcast_f32vector (C_word data, C_word root, C_word comm)
{
  float *vect; int len, vroot;

  MPI_check_comm(comm);

  vect  = C_c_f32vector(data);
  len   = C_32vector_length(data);
  vroot = (int)C_num_to_int (root);

  MPI_Bcast(vect, len, MPI_FLOAT, vroot, Comm_val(comm));

  C_return(data);
}


C_word MPI_broadcast_f64vector (C_word data, C_word root, C_word comm)
{
  double *vect; int len, vroot;

  MPI_check_comm(comm);

  vect  = C_c_f64vector(data);
  len   = C_64vector_length(data);
  vroot = (int)C_num_to_int (root);

  MPI_Bcast(vect, len, MPI_DOUBLE, vroot, Comm_val(comm));

  C_return(data);
}
<#

(define MPI_broadcast_u8vector (foreign-lambda scheme-object "MPI_broadcast_u8vector" 
					       scheme-object scheme-object scheme-object ))
(define MPI_broadcast_s8vector (foreign-lambda scheme-object "MPI_broadcast_s8vector" 
					       scheme-object scheme-object scheme-object ))
(define MPI_broadcast_u16vector (foreign-lambda scheme-object "MPI_broadcast_u16vector" 
						scheme-object scheme-object scheme-object ))
(define MPI_broadcast_s16vector (foreign-lambda scheme-object "MPI_broadcast_s16vector" 
						scheme-object scheme-object scheme-object ))
(define MPI_broadcast_u32vector (foreign-lambda scheme-object "MPI_broadcast_u32vector" 
						scheme-object scheme-object scheme-object ))
(define MPI_broadcast_s32vector (foreign-lambda scheme-object "MPI_broadcast_s32vector" 
						scheme-object scheme-object scheme-object ))
(define MPI_broadcast_f32vector (foreign-lambda scheme-object "MPI_broadcast_f32vector" 
						scheme-object scheme-object scheme-object ))
(define MPI_broadcast_f64vector (foreign-lambda scheme-object "MPI_broadcast_f64vector" 
						scheme-object scheme-object scheme-object ))

(define MPI_broadcast_bytevector (foreign-lambda scheme-object "MPI_broadcast_bytevector" 
						 scheme-object scheme-object scheme-object ))

  
(define (make-bcast obj-size make-obj bcast)
  (lambda (v root comm)
    (let ((myself (MPI:comm-rank comm)))
      (if (= root myself)
	  ;; if this is the root process, broadcast the data
	  (begin
	    (MPI:broadcast-fixnum (obj-size v) root comm)
	    (bcast v root comm))
	  ;; Other processes receive the data length, allocate a buffer
	  ;; and receive the data
	  (let* ((len     (MPI:broadcast-fixnum 0 root comm))
		 (buffer  (make-obj len)))
	    (bcast buffer root comm))))))

(define MPI:broadcast-bytevector
  (make-bcast blob-size make-blob MPI_broadcast_bytevector))

(define-syntax define-srfi4-broadcast
  (lambda (x r c)
    (let* ((type (cadr x))
	   (%define (r 'define))
	   (vlen    (string->symbol (string-append (symbol->string type) "vector-length")))
	   (makev   (string->symbol (string-append "make-" (symbol->string type) "vector")))
	   (bcastv  (string->symbol (string-append "MPI_broadcast_" (symbol->string type) "vector")))
	   (name    (string->symbol (string-append "MPI:broadcast-" (symbol->string type) "vector"))))
      `(,%define ,name (make-bcast ,vlen ,makev ,bcastv))))) 


(define-srfi4-broadcast s8)
(define-srfi4-broadcast u8)
(define-srfi4-broadcast s16)
(define-srfi4-broadcast u16)
(define-srfi4-broadcast s32)
(define-srfi4-broadcast u32)
(define-srfi4-broadcast f32)
(define-srfi4-broadcast f64)

#>

// memcpy with destination offset
void *dimemcpy (void *dest, const void *src, size_t n, size_t i)
{
   return memcpy(dest+i, src, n);
}

// memcpy with destination offset -- 2 byte data sizes
void *dimemcpy2 (void *dest, const void *src, size_t n, size_t i)
{
   return memcpy(dest+(2*i), src, 2*n);
}


// memcpy with destination offset -- 4 byte data sizes
void *dimemcpy4 (void *dest, const void *src, size_t n, size_t i)
{
   return memcpy(dest+(4*i), src, 4*n);
}


// memcpy with destination offset -- 8 byte data sizes
void *dimemcpy8 (void *dest, const void *src, size_t n, size_t i)
{
   return memcpy(dest+(8*i), src, 8*n);
}


// memcpy with source offset
void *simemcpy (void *dest, const void *src, size_t n, size_t i)
{
   return memcpy(dest, src+i, n);
}

// memcpy with source offset -- 2 byte data sizes
void *simemcpy2 (void *dest, const void *src, size_t n, size_t i)
{
   return memcpy(dest, src+(2*i), 2*n);
}


// memcpy with source offset -- 4 byte data sizes
void *simemcpy4 (void *dest, const void *src, size_t n, size_t i)
{
   return memcpy(dest, src+(4*i), 4*n);
}


// memcpy with source offset -- 8 byte data sizes
void *simemcpy8 (void *dest, const void *src, size_t n, size_t i)
{
   return memcpy(dest, src+(8*i), 8*n);
}

static void MPI_counts_displs(int size,
			      int *lengths,
                              int *counts,
                              int *displs)
{
  int disp, i;

  if (size > 0) 
  {
    for (i = 0, disp = 0; i < size; i++) 
    {
      counts[i] = lengths[i];
      displs[i] = disp;
      disp += counts[i];
    }
  }  
}
<#


(define bytevector_dimemcpy  (foreign-lambda void  "dimemcpy"  blob blob integer integer))
(define u8vector_dimemcpy    (foreign-lambda void  "dimemcpy"  u8vector u8vector integer integer))
(define s8vector_dimemcpy    (foreign-lambda void  "dimemcpy"  s8vector s8vector integer integer))
(define s16vector_dimemcpy   (foreign-lambda void  "dimemcpy2" s16vector s16vector integer integer))
(define u16vector_dimemcpy   (foreign-lambda void  "dimemcpy2" u16vector u16vector integer integer))
(define s32vector_dimemcpy   (foreign-lambda void  "dimemcpy4" s32vector s32vector integer integer))
(define u32vector_dimemcpy   (foreign-lambda void  "dimemcpy4" u32vector u32vector integer integer))
(define f32vector_dimemcpy   (foreign-lambda void  "dimemcpy4" f32vector f32vector integer integer))
(define f64vector_dimemcpy   (foreign-lambda void  "dimemcpy8" f64vector f64vector integer integer))


(define bytevector_simemcpy  (foreign-lambda void  "simemcpy"  blob blob integer integer))
(define u8vector_simemcpy    (foreign-lambda void  "simemcpy"  u8vector u8vector integer integer))
(define s8vector_simemcpy    (foreign-lambda void  "simemcpy"  s8vector s8vector integer integer))
(define s16vector_simemcpy   (foreign-lambda void  "simemcpy2" s16vector s16vector integer integer))
(define u16vector_simemcpy   (foreign-lambda void  "simemcpy2" u16vector u16vector integer integer))
(define s32vector_simemcpy   (foreign-lambda void  "simemcpy4" s32vector s32vector integer integer))
(define u32vector_simemcpy   (foreign-lambda void  "simemcpy4" u32vector u32vector integer integer))
(define f32vector_simemcpy   (foreign-lambda void  "simemcpy4" f32vector f32vector integer integer))
(define f64vector_simemcpy   (foreign-lambda void  "simemcpy8" f64vector f64vector integer integer))

;; scatter & scatterv


(define MPI_scatter_int 
    (foreign-primitive scheme-object ((scheme-object data)
				      (integer root)
				      (scheme-object comm))
#<<END
  C_word result; int *vdata; 
  int n; C_word *ptr;

  MPI_check_comm(comm);

  if (data == C_SCHEME_UNDEFINED)
  {
    MPI_Scatter(NULL, 1, MPI_INT, &n, 1, MPI_INT, root, Comm_val(comm));
  }
  else
  {
    vdata  = C_c_s32vector(data);

    MPI_Scatter(vdata, 1, MPI_INT, &n, 1, MPI_INT, root, Comm_val(comm));
  }

  ptr = C_alloc (C_SIZEOF_FLONUM);
  result = C_long_to_num (&ptr, (long)n);

  C_return(result);
END
))


(define MPI_scatter_flonum 
    (foreign-primitive scheme-object ((scheme-object data)
				      (integer root)
				      (scheme-object comm))
#<<END
  C_word result; C_word *ptr;
  double n; double *vdata; 

  MPI_check_comm(comm);

  if (data == C_SCHEME_UNDEFINED)
  {
    MPI_Scatter(NULL, 1, MPI_DOUBLE, &n, 1, MPI_DOUBLE, root, Comm_val(comm));
  }
  else
  {
    vdata  = C_c_f64vector(data);

    MPI_Scatter(vdata, 1, MPI_DOUBLE, &n, 1, MPI_DOUBLE, root, Comm_val(comm));
  }

  ptr = C_alloc (C_SIZEOF_FLONUM);
  result = C_flonum (&ptr, n);

  C_return(result);
END
))

#>


C_word MPI_scatter_bytevector (C_word data, C_word sendcount, C_word recv, C_word root, C_word comm)
{
  unsigned char *vect, *vrecv; int  vroot, rlen, slen, status, vectlen;
  C_word result; C_word *ptr;

  MPI_check_comm(comm);
  C_i_check_bytevector (recv);

  vroot  = (int)C_num_to_int (root);
  vrecv  = C_c_bytevector(recv);
  rlen   = C_bytevector_length(recv);

  if (data == C_SCHEME_UNDEFINED)
  {
    status = MPI_Scatter(NULL, rlen, MPI_BYTE, vrecv, rlen, MPI_BYTE, vroot, Comm_val(comm));
  }
  else
  {
    C_i_check_bytevector (data);
    vect  = C_c_bytevector(data);
    vectlen   = C_bytevector_length(data);
    slen  = (int)C_num_to_int (sendcount);
    status = MPI_Scatter(vect, slen, MPI_BYTE, vrecv, rlen, MPI_BYTE, vroot, Comm_val(comm));
  }

  C_return (recv);
}



C_word MPI_scatter_u8vector (C_word data, C_word sendcount, C_word recv, C_word root, C_word comm)
{
  unsigned char *vect, *vrecv; int vroot, rlen, slen;
  C_word result; C_word *ptr;

  MPI_check_comm(comm);

  vroot  = (int)C_num_to_int (root);
  vrecv  = C_c_u8vector(recv);
  rlen   = C_8vector_length(recv);

  if (data == C_SCHEME_UNDEFINED)
  {
    MPI_Scatter(NULL, rlen, MPI_UNSIGNED_CHAR, vrecv, rlen, MPI_UNSIGNED_CHAR, vroot, Comm_val(comm));
  }
  else
  {
    vect  = C_c_u8vector(data);
    slen  = (int)C_num_to_int (sendcount);
    MPI_Scatter(vect, slen, MPI_UNSIGNED_CHAR, vrecv, rlen, MPI_UNSIGNED_CHAR, vroot, Comm_val(comm));
  }

  C_return (recv);
}


C_word MPI_scatter_s8vector (C_word data, C_word sendcount, C_word recv, C_word root, C_word comm)
{
  char *vect, *vrecv; int  vroot, rlen, slen;
  C_word result; C_word *ptr;

  MPI_check_comm(comm);

  vroot  = (int)C_num_to_int (root);
  vrecv  = C_c_s8vector(recv);
  rlen   = C_8vector_length(recv);

  if (data == C_SCHEME_UNDEFINED)
  {
    MPI_Scatter(NULL, rlen, MPI_SIGNED_CHAR, vrecv, rlen, MPI_SIGNED_CHAR, vroot, Comm_val(comm));
  }
  else
  {
    vect  = C_c_s8vector(data);
    slen  = (int)C_num_to_int (sendcount);
    MPI_Scatter(vect, slen, MPI_SIGNED_CHAR, vrecv, rlen, MPI_SIGNED_CHAR, vroot, Comm_val(comm));
  }

  C_return (recv);
}



C_word MPI_scatter_u16vector (C_word data, C_word sendcount, C_word recv, C_word root, C_word comm)
{
  unsigned short *vect, *vrecv; int  vroot, rlen, slen;
  C_word result; C_word *ptr;

  MPI_check_comm(comm);

  vroot  = (int)C_num_to_int (root);
  vrecv  = C_c_u16vector(recv);
  rlen   = C_16vector_length(recv);

  if (data == C_SCHEME_UNDEFINED)
  {
    MPI_Scatter(NULL, rlen, MPI_UNSIGNED_SHORT, vrecv, rlen, MPI_UNSIGNED_SHORT, vroot, Comm_val(comm));
  }
  else
  {
    vect  = C_c_u16vector(data);
    slen  = (int)C_num_to_int (sendcount);
    MPI_Scatter(vect, slen, MPI_UNSIGNED_SHORT, vrecv, rlen, MPI_UNSIGNED_SHORT, vroot, Comm_val(comm));
  }

  C_return (recv);
}


C_word MPI_scatter_s16vector (C_word data, C_word sendcount, C_word recv, C_word root, C_word comm)
{
  short *vect, *vrecv; int  vroot, rlen, slen;
  C_word result; C_word *ptr;

  MPI_check_comm(comm);

  vroot  = (int)C_num_to_int (root);
  vrecv  = C_c_s16vector(recv);
  rlen   = C_16vector_length(recv);

  if (data == C_SCHEME_UNDEFINED)
  {
    MPI_Scatter(NULL, rlen, MPI_SHORT, vrecv, rlen, MPI_SHORT, vroot, Comm_val(comm));
  }
  else
  {
    vect  = C_c_s16vector(data);
    slen  = (int)C_num_to_int (sendcount);
    MPI_Scatter(vect, slen, MPI_SHORT, vrecv, rlen, MPI_SHORT, vroot, Comm_val(comm));
  }

  C_return (recv);
}



C_word MPI_scatter_u32vector (C_word data, C_word sendcount, C_word recv, C_word root, C_word comm)
{
  unsigned int *vect, *vrecv; int  vroot, rlen, slen;
  C_word result; C_word *ptr;

  MPI_check_comm(comm);

  vroot  = (int)C_num_to_int (root);
  vrecv  = C_c_u32vector(recv);
  rlen   = C_32vector_length(recv);

  if (data == C_SCHEME_UNDEFINED)
  {
    MPI_Scatter(NULL, rlen, MPI_UNSIGNED, vrecv, rlen, MPI_UNSIGNED, vroot, Comm_val(comm));
  }
  else
  {
    vect  = C_c_u32vector(data);
    slen  = (int)C_num_to_int (sendcount);
    MPI_Scatter(vect, slen, MPI_UNSIGNED, vrecv, rlen, MPI_UNSIGNED, vroot, Comm_val(comm));
  }

  C_return (recv);
}


C_word MPI_scatter_s32vector (C_word data, C_word sendcount, C_word recv, C_word root, C_word comm)
{
  int *vect, *vrecv; int  vroot, rlen, slen;
  C_word result; C_word *ptr;

  MPI_check_comm(comm);

  vroot  = (int)C_num_to_int (root);
  vrecv  = C_c_s32vector(recv);
  rlen   = C_32vector_length(recv);

  if (data == C_SCHEME_UNDEFINED)
  {
    MPI_Scatter(NULL, rlen, MPI_INT, vrecv, rlen, MPI_INT, vroot, Comm_val(comm));
  }
  else
  {
    vect  = C_c_s32vector(data);
    slen  = (int)C_num_to_int (sendcount);
    MPI_Scatter(vect, slen, MPI_INT, vrecv, rlen, MPI_INT, vroot, Comm_val(comm));
  }

  C_return (recv);
}



C_word MPI_scatter_f32vector (C_word data, C_word sendcount, C_word recv, C_word root, C_word comm)
{
  float *vect, *vrecv; int  vroot, rlen, slen;
  C_word result; C_word *ptr;

  MPI_check_comm(comm);

  vroot  = (int)C_num_to_int (root);
  vrecv  = C_c_f32vector(recv);
  rlen   = C_32vector_length(recv);

  if (data == C_SCHEME_UNDEFINED)
  {
    MPI_Scatter(NULL, rlen, MPI_FLOAT, vrecv, rlen, MPI_FLOAT, vroot, Comm_val(comm));
  }
  else
  {
    vect  = C_c_f32vector(data);
    slen  = (int)C_num_to_int (sendcount);
    MPI_Scatter(vect, slen, MPI_FLOAT, vrecv, rlen, MPI_FLOAT, vroot, Comm_val(comm));
  }

  C_return (recv);
}



C_word MPI_scatter_f64vector (C_word data, C_word sendcount, C_word recv, C_word root, C_word comm)
{
  double *vect, *vrecv; int  vroot, rlen, slen;
  C_word result; C_word *ptr;

  MPI_check_comm(comm);

  vroot  = (int)C_num_to_int (root);
  vrecv  = C_c_f64vector(recv);
  rlen   = C_64vector_length(recv);

  if (data == C_SCHEME_UNDEFINED)
  {
    MPI_Scatter(NULL, rlen, MPI_DOUBLE, vrecv, rlen, MPI_DOUBLE, vroot, Comm_val(comm));
  }
  else
  {
    vect  = C_c_f64vector(data);
    slen  = (int)C_num_to_int (sendcount);
    MPI_Scatter(vect, slen, MPI_DOUBLE, vrecv, rlen, MPI_DOUBLE, vroot, Comm_val(comm));
  }

  C_return (recv);
}


C_word MPI_scatterv_bytevector (C_word sendbuf, C_word sendlengths, 
			        C_word recvbuf, C_word root, C_word comm,
			        C_word sendcounts, C_word displs)
{
  int len, vroot; int *vsendlengths, *vsendcounts, *vdispls;

  MPI_check_comm (comm);

  C_i_check_bytevector (recvbuf);

  vroot = (int)C_num_to_int (root);

  if (sendbuf == C_SCHEME_UNDEFINED)
  {
     MPI_Scatterv(NULL, NULL, NULL, MPI_BYTE,
                  C_c_bytevector(recvbuf), C_bytevector_length(recvbuf), MPI_BYTE,
                  vroot, Comm_val(comm));
  }
  else
  {
     C_i_check_bytevector (sendbuf);

     len           = C_32vector_length(sendlengths);
     vsendlengths  = C_c_s32vector(sendlengths);
     vsendcounts   = C_c_s32vector(sendcounts);
     vdispls       = C_c_s32vector(displs);

     MPI_counts_displs(len, vsendlengths, vsendcounts, vdispls);

     MPI_Scatterv(C_c_bytevector(sendbuf), vsendcounts, vdispls, MPI_BYTE,
                  C_c_bytevector(recvbuf), C_bytevector_length(recvbuf), MPI_BYTE,
                  vroot, Comm_val(comm));
  }

  C_return (recvbuf);
}



C_word MPI_scatterv_u8vector (C_word sendbuf, C_word sendlengths, 
			      C_word recvbuf, C_word root, C_word comm,
			      C_word sendcounts, C_word displs)
{
  int len, vroot; int *vsendlengths, *vsendcounts, *vdispls;

  MPI_check_comm (comm);

  vroot = (int)C_num_to_int (root);

  if (sendbuf == C_SCHEME_UNDEFINED)
  {
     MPI_Scatterv(NULL, NULL, NULL, MPI_UNSIGNED_CHAR,
                  C_c_u8vector(recvbuf), C_8vector_length(recvbuf), MPI_UNSIGNED_CHAR,
                  vroot, Comm_val(comm));
  }
  else
  {
     len           = C_32vector_length(sendlengths);
     vsendlengths  = C_c_s32vector(sendlengths);
     vsendcounts   = C_c_s32vector(sendcounts);
     vdispls       = C_c_s32vector(displs);

     MPI_counts_displs(len, vsendlengths, vsendcounts, vdispls);
  
     MPI_Scatterv(C_c_u8vector(sendbuf), vsendcounts, vdispls, MPI_UNSIGNED_CHAR,
                  C_c_u8vector(recvbuf), C_8vector_length(recvbuf), MPI_UNSIGNED_CHAR,
                  vroot, Comm_val(comm));
  }

  C_return (recvbuf);
}

C_word MPI_scatterv_s8vector (C_word sendbuf, C_word sendlengths, 
			      C_word recvbuf, C_word root, C_word comm,
			      C_word sendcounts, C_word displs)
{
  int len, vroot; int *vsendlengths, *vsendcounts, *vdispls;

  MPI_check_comm (comm);

  vroot = (int)C_num_to_int (root);

  if (sendbuf == C_SCHEME_UNDEFINED)
  {
     MPI_Scatterv(NULL, NULL, NULL, MPI_SIGNED_CHAR,
                  C_c_s8vector(recvbuf), C_8vector_length(recvbuf), MPI_SIGNED_CHAR,
                  vroot, Comm_val(comm));
  }
  else
  {
     len           = C_32vector_length(sendlengths);
     vsendlengths  = C_c_s32vector(sendlengths);
     vsendcounts   = C_c_s32vector(sendcounts);
     vdispls       = C_c_s32vector(displs);

     MPI_counts_displs(len, vsendlengths, vsendcounts, vdispls);
  
     MPI_Scatterv(C_c_s8vector(sendbuf), vsendcounts, vdispls, MPI_SIGNED_CHAR,
                  C_c_s8vector(recvbuf), C_8vector_length(recvbuf), MPI_SIGNED_CHAR,
                  vroot, Comm_val(comm));
  }

  C_return (recvbuf);
}



C_word MPI_scatterv_u16vector (C_word sendbuf, C_word sendlengths, 
			       C_word recvbuf, C_word root, C_word comm,
			       C_word sendcounts, C_word displs)
{
  int len, vroot; int *vsendlengths, *vsendcounts, *vdispls;

  MPI_check_comm (comm);

  vroot = (int)C_num_to_int (root);

  if (sendbuf == C_SCHEME_UNDEFINED)
  {
     MPI_Scatterv(NULL, NULL, NULL, MPI_UNSIGNED_SHORT,
                  C_c_u16vector(recvbuf), C_16vector_length(recvbuf), MPI_UNSIGNED_SHORT,
                  vroot, Comm_val(comm));
  }
  else
  {
     len           = C_32vector_length(sendlengths);
     vsendlengths  = C_c_s32vector(sendlengths);
     vsendcounts   = C_c_s32vector(sendcounts);
     vdispls       = C_c_s32vector(displs);

     MPI_counts_displs(len, vsendlengths, vsendcounts, vdispls);
  
     MPI_Scatterv(C_c_u16vector(sendbuf), vsendcounts, vdispls, MPI_UNSIGNED_SHORT,
                  C_c_u16vector(recvbuf), C_16vector_length(recvbuf), MPI_UNSIGNED_SHORT,
                  vroot, Comm_val(comm));
  }

  C_return (recvbuf);
}

C_word MPI_scatterv_s16vector (C_word sendbuf, C_word sendlengths, 
			       C_word recvbuf, C_word root, C_word comm,
			       C_word sendcounts, C_word displs)
{
  int len, vroot; int *vsendlengths, *vsendcounts, *vdispls;

  MPI_check_comm (comm);

  vroot = (int)C_num_to_int (root);

  if (sendbuf == C_SCHEME_UNDEFINED)
  {
     MPI_Scatterv(NULL, NULL, NULL, MPI_SHORT,
                  C_c_s16vector(recvbuf), C_16vector_length(recvbuf), MPI_SHORT,
                  vroot, Comm_val(comm));
  }
  else
  {
     len           = C_32vector_length(sendlengths);
     vsendlengths  = C_c_s32vector(sendlengths);
     vsendcounts   = C_c_s32vector(sendcounts);
     vdispls       = C_c_s32vector(displs);

     MPI_counts_displs(len, vsendlengths, vsendcounts, vdispls);
  
     MPI_Scatterv(C_c_s16vector(sendbuf), vsendcounts, vdispls, MPI_SHORT,
                  C_c_s16vector(recvbuf), C_16vector_length(recvbuf), MPI_SHORT,
                  vroot, Comm_val(comm));
  }

  C_return (recvbuf);
}



C_word MPI_scatterv_u32vector (C_word sendbuf, C_word sendlengths, 
			       C_word recvbuf, C_word root, C_word comm,
			       C_word sendcounts, C_word displs)
{
  int len, vroot; int  *vsendlengths, *vsendcounts, *vdispls;

  MPI_check_comm (comm);

  vroot = (int)C_num_to_int (root);

  if (sendbuf == C_SCHEME_UNDEFINED)
  {
     MPI_Scatterv(NULL, NULL, NULL, MPI_UNSIGNED,
                  C_c_u32vector(recvbuf), C_32vector_length(recvbuf), MPI_UNSIGNED,
                  vroot, Comm_val(comm));
  }
  else
  {
     len           = C_32vector_length(sendlengths);
     vsendlengths  = C_c_s32vector(sendlengths);
     vsendcounts   = C_c_s32vector(sendcounts);
     vdispls       = C_c_s32vector(displs);

     MPI_counts_displs(len, vsendlengths, vsendcounts, vdispls);
  
     MPI_Scatterv(C_c_u32vector(sendbuf), vsendcounts, vdispls, MPI_UNSIGNED,
                  C_c_u32vector(recvbuf), C_32vector_length(recvbuf), MPI_UNSIGNED,
                  vroot, Comm_val(comm));
  }

  C_return (recvbuf);
}

C_word MPI_scatterv_s32vector (C_word sendbuf, C_word sendlengths, 
			       C_word recvbuf, C_word root, C_word comm,
			       C_word sendcounts, C_word displs)
{
  int len, vroot; int *vsendlengths, *vsendcounts, *vdispls;

  MPI_check_comm (comm);

  vroot = (int)C_num_to_int (root);

  if (sendbuf == C_SCHEME_UNDEFINED)
  {
     MPI_Scatterv(NULL, NULL, NULL, MPI_INT,
                  C_c_s32vector(recvbuf), C_32vector_length(recvbuf), MPI_INT,
                  vroot, Comm_val(comm));
  }
  else
  {
     len           = C_32vector_length(sendlengths);
     vsendlengths  = C_c_s32vector(sendlengths);
     vsendcounts   = C_c_s32vector(sendcounts);
     vdispls       = C_c_s32vector(displs);

     MPI_counts_displs(len, vsendlengths, vsendcounts, vdispls);
  
     MPI_Scatterv(C_c_s32vector(sendbuf), vsendcounts, vdispls, MPI_INT,
                  C_c_s32vector(recvbuf), C_32vector_length(recvbuf), MPI_INT,
                  vroot, Comm_val(comm));
  }

  C_return (recvbuf);
}


C_word MPI_scatterv_f32vector (C_word sendbuf, C_word sendlengths, 
			       C_word recvbuf, C_word root, C_word comm,
			       C_word sendcounts, C_word displs)
{
  int len, vroot; int *vsendlengths, *vsendcounts, *vdispls;

  MPI_check_comm (comm);

  vroot = (int)C_num_to_int (root);

  if (sendbuf == C_SCHEME_UNDEFINED)
  {
     MPI_Scatterv(NULL, NULL, NULL, MPI_FLOAT,
                  C_c_f32vector(recvbuf), C_32vector_length(recvbuf), MPI_FLOAT,
                  vroot, Comm_val(comm));
  }
  else
  {
     len           = C_32vector_length(sendlengths);
     vsendlengths  = C_c_s32vector(sendlengths);
     vsendcounts   = C_c_s32vector(sendcounts);
     vdispls       = C_c_s32vector(displs);

     MPI_counts_displs(len, vsendlengths, vsendcounts, vdispls);
  
     MPI_Scatterv(C_c_f32vector(sendbuf), vsendcounts, vdispls, MPI_FLOAT,
                  C_c_f32vector(recvbuf), C_32vector_length(recvbuf), MPI_FLOAT,
                  vroot, Comm_val(comm));
  }

  C_return (recvbuf);
}

C_word MPI_scatterv_f64vector (C_word sendbuf, C_word sendlengths, 
			       C_word recvbuf, C_word root, C_word comm,
			       C_word sendcounts, C_word displs)
{
  int len, vroot; int *vsendlengths, *vsendcounts, *vdispls;

  MPI_check_comm (comm);

  vroot = (int)C_num_to_int (root);

  if (sendbuf == C_SCHEME_UNDEFINED)
  {
     MPI_Scatterv(NULL, NULL, NULL, MPI_DOUBLE,
                  C_c_f64vector(recvbuf), C_64vector_length(recvbuf), MPI_DOUBLE,
                  vroot, Comm_val(comm));
  }
  else
  {
     len           = C_32vector_length(sendlengths);
     vsendlengths  = C_c_s32vector(sendlengths);
     vsendcounts   = C_c_s32vector(sendcounts);
     vdispls       = C_c_s32vector(displs);

     MPI_counts_displs(len, vsendlengths, vsendcounts, vdispls);
  
     MPI_Scatterv(C_c_f64vector(sendbuf), vsendcounts, vdispls, MPI_DOUBLE,
                  C_c_f64vector(recvbuf), C_64vector_length(recvbuf), MPI_DOUBLE,
                  vroot, Comm_val(comm));
  }

  C_return (recvbuf);
}


<#

(define MPI_scatter_u8vector (foreign-lambda scheme-object "MPI_scatter_u8vector"
					     scheme-object scheme-object scheme-object scheme-object scheme-object))
(define MPI_scatter_s8vector (foreign-lambda scheme-object "MPI_scatter_s8vector"
					     scheme-object scheme-object scheme-object scheme-object scheme-object))

(define MPI_scatter_u16vector (foreign-lambda scheme-object "MPI_scatter_u16vector"
					      scheme-object scheme-object scheme-object scheme-object scheme-object))
(define MPI_scatter_s16vector (foreign-lambda scheme-object "MPI_scatter_s16vector"
					      scheme-object scheme-object scheme-object scheme-object scheme-object))

(define MPI_scatter_u32vector (foreign-lambda scheme-object "MPI_scatter_u32vector"
					      scheme-object scheme-object scheme-object scheme-object scheme-object))
(define MPI_scatter_s32vector (foreign-lambda scheme-object "MPI_scatter_s32vector"
					      scheme-object scheme-object scheme-object scheme-object scheme-object))


(define MPI_scatter_f32vector (foreign-lambda scheme-object "MPI_scatter_f32vector"
					      scheme-object scheme-object scheme-object scheme-object scheme-object))
(define MPI_scatter_f64vector (foreign-lambda scheme-object "MPI_scatter_f64vector"
					      scheme-object scheme-object scheme-object scheme-object scheme-object))


(define MPI_scatter_bytevector (foreign-lambda scheme-object "MPI_scatter_bytevector" 
					       scheme-object scheme-object scheme-object scheme-object scheme-object ))


(define (make-scatter make-obj obj-len scatter)
  (lambda (v sendcount root comm)
    (let ((myself (MPI:comm-rank comm))
	  (nprocs (MPI:comm-size comm)))
      (if (= root myself)
	  ;; If this is the root process, scatter the data
	  (if (<= (* nprocs sendcount) (obj-len v))
	      (let ((recv (make-obj sendcount)))
		(scatter (object-evict v) sendcount recv root comm))
	      (error 'MPI:scatter "send data length is less than n * sendcount"))
	  ;; Other processes allocate a buffer and receive the data
	  (let ((recv (make-obj sendcount)))
	    (scatter (void) sendcount recv root comm))))))

(define (MPI:scatter-int data root comm)
  (let ((nprocs (MPI:comm-size comm)))
    (if (< (s32vector-length data) nprocs)
	(error 'MPI:scatter-int "send data length is less than n "))
    (MPI_scatter_int data root comm)))

(define (MPI:scatter-flonum data root comm)
  (let ((nprocs (MPI:comm-size comm)))
    (if (< (f64vector-length data) nprocs)
	(error 'MPI:scatter-flonum "send data length is less than n "))
    (MPI_scatter_flonum data root comm)))

(define MPI:scatter-bytevector (make-scatter make-blob blob-size MPI_scatter_bytevector))
	  
(define-syntax define-srfi4-scatter
  (lambda (x r c)
    (let* ((type     (cadr x))
	   (%define  (r 'define))
	   (name     (string->symbol (string-append "MPI:scatter-" (symbol->string type) "vector")))
	   (makev    (string->symbol (string-append "make-" (symbol->string type) "vector")))
	   (vlen     (string->symbol (string-append (symbol->string type) "vector-length")))
	   (scatter  (string->symbol (string-append "MPI_scatter_" (symbol->string type) "vector"))))
       `(,%define ,name (make-scatter ,makev ,vlen ,scatter)))))

(define-srfi4-scatter s8)
(define-srfi4-scatter u8)
(define-srfi4-scatter s16)
(define-srfi4-scatter u16)
(define-srfi4-scatter s32)
(define-srfi4-scatter u32)
(define-srfi4-scatter f32)
(define-srfi4-scatter f64)


(define MPI_scatterv_bytevector (foreign-lambda scheme-object "MPI_scatterv_bytevector" 
						scheme-object scheme-object scheme-object 
						scheme-object scheme-object scheme-object 
						scheme-object ))

(define MPI_scatterv_u8vector (foreign-lambda scheme-object "MPI_scatterv_u8vector" 
						scheme-object scheme-object scheme-object 
						scheme-object scheme-object scheme-object 
						scheme-object ))
(define MPI_scatterv_s8vector (foreign-lambda scheme-object "MPI_scatterv_s8vector" 
						scheme-object scheme-object scheme-object 
						scheme-object scheme-object scheme-object 
						scheme-object ))


(define MPI_scatterv_u16vector (foreign-lambda scheme-object "MPI_scatterv_u16vector" 
						scheme-object scheme-object scheme-object 
						scheme-object scheme-object scheme-object 
						scheme-object ))
(define MPI_scatterv_s16vector (foreign-lambda scheme-object "MPI_scatterv_s16vector" 
						scheme-object scheme-object scheme-object 
						scheme-object scheme-object scheme-object 
						scheme-object ))


(define MPI_scatterv_u32vector (foreign-lambda scheme-object "MPI_scatterv_u32vector" 
						scheme-object scheme-object scheme-object 
						scheme-object scheme-object scheme-object 
						scheme-object ))
(define MPI_scatterv_s32vector (foreign-lambda scheme-object "MPI_scatterv_s32vector" 
						scheme-object scheme-object scheme-object 
						scheme-object scheme-object scheme-object 
						scheme-object ))


(define MPI_scatterv_f32vector (foreign-lambda scheme-object "MPI_scatterv_f32vector" 
						scheme-object scheme-object scheme-object 
						scheme-object scheme-object scheme-object 
						scheme-object ))
(define MPI_scatterv_f64vector (foreign-lambda scheme-object "MPI_scatterv_f64vector" 
						scheme-object scheme-object scheme-object 
						scheme-object scheme-object scheme-object 
						scheme-object ))



(define (make-scatterv vlen makev dimemcpy scatterv)
  (lambda  (data root comm)
    (let ((myself (MPI:comm-rank comm))
	  (nprocs (MPI:comm-size comm)))
      (if (= root myself)
	  (let ((data-len (length data)))
	    (if (not (= data-len nprocs))
		(error 'MPI:scatterv "wrong data size: nprocs = " nprocs
		       " data length = " data-len))
	    (let ((sendlengths (map vlen data)))
	      ;; Scatter the lengths of the buffers to all the processes
	      (let ((mylen (MPI_scatter_int (list->s32vector sendlengths) root comm)))
		;; Build single buffer with all data 
		(let* ((total   (apply + sendlengths))
		       (sendbuf (makev total)))
		  (fold (lambda (x offset)
			  (let ((len (vlen x)))
			    (dimemcpy sendbuf x len offset)
			    (+ offset len)))
			0 data)
		  ;; Allocate receive buffer & compute sendcounts and displs
		  (let ((myrecv (makev mylen)))
		    ;; Do the scatter & return received value
		    (scatterv sendbuf (list->s32vector sendlengths) myrecv root comm
			      (make-s32vector (length data))
			      (make-s32vector (length data)))
		    myrecv)))))
	  ;; If not root, get our length
	  (let ((mylen (MPI_scatter_int (void) root comm)))
	    ;; Allocate receive buffer
	    (let ((myrecv (makev mylen)))
	      ;; Do the scatter & return received value
	      (scatterv (void) (void) myrecv root comm (void) (void))
	      myrecv))))))
  
(define MPI:scatterv-bytevector (make-scatterv blob-size make-blob bytevector_dimemcpy MPI_scatterv_bytevector))
	  
(define-syntax define-srfi4-scatterv
  (lambda (x r c)
    (let* ((type (cadr x))
	   (%define (r 'define))
	   (vlen      (string->symbol (string-append (symbol->string type) "vector-length")))
	   (makev     (string->symbol (string-append "make-" (symbol->string type) "vector")))
	   (dimemcpy  (string->symbol (string-append (symbol->string type) "vector_dimemcpy")))
	   (scatterv  (string->symbol (string-append "MPI_scatterv_" (symbol->string type) "vector")))
	   (name      (string->symbol (string-append "MPI:scatterv-" (symbol->string type) "vector"))))
      `(,%define ,name (make-scatterv ,vlen ,makev ,dimemcpy ,scatterv)))))

(define-srfi4-scatterv s8)
(define-srfi4-scatterv u8)
(define-srfi4-scatterv s16)
(define-srfi4-scatterv u16)
(define-srfi4-scatterv s32)
(define-srfi4-scatterv u32)
(define-srfi4-scatterv f32)
(define-srfi4-scatterv f64)
					   

;; Gather & gatherv


(define MPI_gather_int 
    (foreign-primitive scheme-object ((integer send)
				      (scheme-object recv)
				      (integer root)
				      (scheme-object comm))
#<<END
  int *vrecv; int rlen;
  C_word result; C_word *ptr;

  MPI_check_comm(comm);

  if (recv == C_SCHEME_UNDEFINED)
  {
    MPI_Gather(&send, 1, MPI_INT, NULL, 1, MPI_INT, root, Comm_val(comm));
    result = C_SCHEME_UNDEFINED;
  }
  else
  {
    vrecv  = C_c_s32vector(recv);

    MPI_Gather(&send, 1, MPI_INT, vrecv, 1, MPI_INT, root, Comm_val(comm));
    result = recv;
  }

  C_return (result);
END
))


(define MPI_gather_flonum 
    (foreign-primitive scheme-object ((double send)
				      (scheme-object recv)
				      (integer root)
				      (scheme-object comm))
#<<END
  double *vrecv; int rlen;
  C_word result; C_word *ptr;

  MPI_check_comm(comm);

  if (recv == C_SCHEME_UNDEFINED)
  {
    MPI_Gather(&send, 1, MPI_DOUBLE, NULL, 1, MPI_DOUBLE, root, Comm_val(comm));
    result = C_SCHEME_UNDEFINED;
  }
  else
  {
    vrecv  = C_c_f64vector(recv);
    rlen   = C_64vector_length (recv);

    MPI_Gather(&send, 1, MPI_DOUBLE, vrecv, rlen, MPI_DOUBLE, root, Comm_val(comm));
    result = recv;
  }

  C_return (result);
END
))


#>

C_word MPI_gather_bytevector (C_word send, C_word sendcount, C_word recv, C_word root, C_word comm)
{
  unsigned char *vrecv, *vsend; int  vroot, rlen, slen;
  C_word result; C_word *ptr;

  MPI_check_comm(comm);
  C_i_check_bytevector (send);

  vroot  = (int)C_num_to_int (root);
  vsend  = C_c_bytevector (send);
  slen   = (int)C_num_to_int (sendcount);

  if (recv == C_SCHEME_UNDEFINED)
  {
    MPI_Gather(vsend, slen, MPI_BYTE, NULL, slen, MPI_BYTE, vroot, Comm_val(comm));
    result = C_SCHEME_UNDEFINED;
  }
  else
  {
    C_i_check_bytevector (recv);
    vrecv  = C_c_bytevector(recv);
    rlen   = C_bytevector_length (recv);
    
    MPI_Gather(vsend, slen, MPI_BYTE, vrecv, slen, MPI_BYTE, vroot, Comm_val(comm));

    result = recv;
  }

  C_return (result);
}



C_word MPI_gather_u8vector (C_word send, C_word sendcount, C_word recv, C_word root, C_word comm)
{
  unsigned char *vrecv, *vsend; int  vroot, rlen, slen;
  C_word result; C_word *ptr;

  MPI_check_comm(comm);

  vroot  = (int)C_num_to_int (root);
  vsend  = C_c_u8vector(send);
  slen   = (int)C_num_to_int (sendcount);

  if (recv == C_SCHEME_UNDEFINED)
  {
    MPI_Gather(vsend, slen, MPI_UNSIGNED_CHAR, NULL, slen, MPI_UNSIGNED_CHAR, vroot, Comm_val(comm));
    result = C_SCHEME_UNDEFINED;
  }
  else
  {
    vrecv  = C_c_u8vector(recv);
    rlen   = C_8vector_length(recv);
    MPI_Gather(vsend, slen, MPI_UNSIGNED_CHAR, vrecv, slen, MPI_UNSIGNED_CHAR, vroot, Comm_val(comm));
    result = recv;
  }

  C_return (result);
}



C_word MPI_gather_s8vector (C_word send, C_word sendcount, C_word recv, C_word root, C_word comm)
{
  char *vrecv, *vsend; int  vroot, rlen, slen;
  C_word result; C_word *ptr;

  MPI_check_comm(comm);

  vroot  = (int)C_num_to_int (root);
  vsend  = C_c_s8vector(send);
  slen   = (int)C_num_to_int (sendcount);

  if (recv == C_SCHEME_UNDEFINED)
  {
    MPI_Gather(vsend, slen, MPI_SIGNED_CHAR, NULL, slen, MPI_SIGNED_CHAR, vroot, Comm_val(comm));
    result = C_SCHEME_UNDEFINED;
  }
  else
  {
    vrecv  = C_c_s8vector(recv);
    rlen   = C_8vector_length(recv);
    MPI_Gather(vsend, slen, MPI_SIGNED_CHAR, vrecv, slen, MPI_SIGNED_CHAR, vroot, Comm_val(comm));
    result = recv;
  }

  C_return (result);
}



C_word MPI_gather_u16vector (C_word send, C_word sendcount, C_word recv, C_word root, C_word comm)
{
  unsigned short *vrecv, *vsend; int  vroot, rlen, slen;
  C_word result; C_word *ptr;

  MPI_check_comm(comm);

  vroot  = (int)C_num_to_int (root);
  vsend  = C_c_u16vector(send);
  slen   = (int)C_num_to_int (sendcount);

  if (recv == C_SCHEME_UNDEFINED)
  {
    MPI_Gather(vsend, slen, MPI_UNSIGNED_SHORT, NULL, slen, MPI_UNSIGNED_SHORT, vroot, Comm_val(comm));
    result = C_SCHEME_UNDEFINED;
  }
  else
  {
    vrecv  = C_c_u16vector(recv);
    rlen   = C_16vector_length(recv);
    MPI_Gather(vsend, slen, MPI_UNSIGNED_SHORT, vrecv, slen, MPI_UNSIGNED_SHORT, vroot, Comm_val(comm));
    result = recv;  
  }

  C_return (result);
}


C_word MPI_gather_s16vector (C_word send, C_word sendcount, C_word recv, C_word root, C_word comm)
{
  short *vrecv, *vsend; int  vroot, rlen, slen;
  C_word result; C_word *ptr;

  MPI_check_comm(comm);

  vroot  = (int)C_num_to_int (root);
  vsend  = C_c_s16vector(send);
  slen   = (int)C_num_to_int (sendcount);

  if (recv == C_SCHEME_UNDEFINED)
  {
    MPI_Gather(vsend, slen, MPI_SHORT, NULL, slen, MPI_SHORT, vroot, Comm_val(comm));
    result = C_SCHEME_UNDEFINED;
  }
  else
  {
    vrecv  = C_c_s16vector(recv);
    rlen   = C_16vector_length(recv);
    MPI_Gather(vsend, slen, MPI_SHORT, vrecv, slen, MPI_SHORT, vroot, Comm_val(comm));
    result = recv;  
  }

  C_return (result);
}



C_word MPI_gather_u32vector (C_word send, C_word sendcount, C_word recv, C_word root, C_word comm)
{
  int *vrecv, *vsend; int  vroot, rlen, slen;
  C_word result; C_word *ptr;

  MPI_check_comm(comm);

  vroot  = (int)C_num_to_int (root);
  vsend  = C_c_u32vector(send);
  slen   = (int)C_num_to_int (sendcount);

  if (recv == C_SCHEME_UNDEFINED)
  {
    MPI_Gather(vsend, slen, MPI_UNSIGNED, NULL, slen, MPI_UNSIGNED, vroot, Comm_val(comm));
    result = C_SCHEME_UNDEFINED;
  }
  else
  {
    vrecv  = C_c_u32vector(recv);
    rlen   = C_32vector_length(recv);
    MPI_Gather(vsend, slen, MPI_UNSIGNED, vrecv, slen, MPI_UNSIGNED, vroot, Comm_val(comm));
    result = recv;  
  }

  C_return (result);
}



C_word MPI_gather_s32vector (C_word send, C_word sendcount, C_word recv, C_word root, C_word comm)
{
  int *vrecv, *vsend; int  vroot, rlen, slen;
  C_word result; C_word *ptr;

  MPI_check_comm(comm);

  vroot  = (int)C_num_to_int (root);
  vsend  = C_c_s32vector(send);
  slen   = (int)C_num_to_int (sendcount);

  if (recv == C_SCHEME_UNDEFINED)
  {
    MPI_Gather(vsend, slen, MPI_INT, NULL, slen, MPI_INT, vroot, Comm_val(comm));
    result = C_SCHEME_UNDEFINED;
  }
  else
  {
    vrecv  = C_c_s32vector(recv);
    rlen   = C_32vector_length(recv);
    MPI_Gather(vsend, slen, MPI_INT, vrecv, slen, MPI_INT, vroot, Comm_val(comm));
    result = recv;  
  }

  C_return (result);
}



C_word MPI_gather_f32vector (C_word send, C_word sendcount, C_word recv, C_word root, C_word comm)
{
  float *vrecv, *vsend; int  vroot, rlen, slen;
  C_word result; C_word *ptr;

  MPI_check_comm(comm);

  vroot  = (int)C_num_to_int (root);
  vsend  = C_c_f32vector(send);
  slen   = (int)C_num_to_int (sendcount);

  if (recv == C_SCHEME_UNDEFINED)
  {
    MPI_Gather(vsend, slen, MPI_FLOAT, NULL, slen, MPI_FLOAT, vroot, Comm_val(comm));
    result = C_SCHEME_UNDEFINED;
  }
  else
  {
    vrecv  = C_c_f32vector(recv);
    rlen   = C_32vector_length(recv);
    MPI_Gather(vsend, slen, MPI_FLOAT, vrecv, slen, MPI_FLOAT, vroot, Comm_val(comm));
    result = recv;  
  }

  C_return (result);
}



C_word MPI_gather_f64vector (C_word send, C_word sendcount, C_word recv, C_word root, C_word comm)
{
  double *vrecv, *vsend; int  vroot, rlen, slen;
  C_word result; C_word *ptr;

  MPI_check_comm(comm);

  vroot  = (int)C_num_to_int (root);
  vsend  = C_c_f64vector(send);
  slen   = (int)C_num_to_int (sendcount);

  if (recv == C_SCHEME_UNDEFINED)
  {
    MPI_Gather(vsend, slen, MPI_DOUBLE, NULL, slen, MPI_DOUBLE, vroot, Comm_val(comm));
    result = C_SCHEME_UNDEFINED;
  }
  else
  {
    vrecv  = C_c_f64vector(recv);
    rlen   = C_64vector_length(recv);
    MPI_Gather(vsend, slen, MPI_DOUBLE, vrecv, slen, MPI_DOUBLE, vroot, Comm_val(comm));
    result = recv;  
  }

  C_return (result);
}



C_word MPI_gatherv_bytevector (C_word sendbuf, C_word recvbuf, C_word recvlengths, 
			       C_word root, C_word comm, C_word recvcounts, C_word displs)
{
  int len, vroot; int *vrecvlengths, *vrecvcounts, *vdispls;

  MPI_check_comm (comm);

  C_i_check_bytevector (sendbuf);

  vroot = (int)C_num_to_int (root);

  if (recvbuf == C_SCHEME_UNDEFINED)
  {
     MPI_Gatherv (C_c_bytevector(sendbuf), C_bytevector_length(sendbuf), MPI_BYTE,
                  NULL, NULL, NULL, MPI_BYTE,
                  vroot, Comm_val(comm));
  }
  else
  {
     C_i_check_bytevector (recvbuf);

     len           = C_32vector_length(recvlengths);
     vrecvlengths  = C_c_s32vector(recvlengths);
     vrecvcounts   = C_c_s32vector(recvcounts);
     vdispls       = C_c_s32vector(displs);

     MPI_counts_displs(len, vrecvlengths, vrecvcounts, vdispls);
  
     MPI_Gatherv (C_c_bytevector(sendbuf), C_bytevector_length(sendbuf), MPI_BYTE,
                  C_c_bytevector(recvbuf), vrecvcounts, vdispls, MPI_BYTE,
                  vroot, Comm_val(comm));
  }

  C_return (recvbuf);
}



C_word MPI_gatherv_u8vector (C_word sendbuf, C_word recvbuf, C_word recvlengths, 
		             C_word root, C_word comm, C_word recvcounts, C_word displs)
{
  int len, vroot; int *vrecvlengths, *vrecvcounts, *vdispls;

  MPI_check_comm (comm);

  vroot = (int)C_num_to_int (root);

  if (recvbuf == C_SCHEME_UNDEFINED)
  {
     MPI_Gatherv (C_c_u8vector(sendbuf), C_8vector_length(sendbuf), MPI_UNSIGNED_CHAR,
                  NULL, NULL, NULL, MPI_UNSIGNED_CHAR,
                  vroot, Comm_val(comm));
  }
  else
  {
     len           = C_32vector_length(recvlengths);
     vrecvlengths  = C_c_s32vector(recvlengths);
     vrecvcounts   = C_c_s32vector(recvcounts);
     vdispls       = C_c_s32vector(displs);

     MPI_counts_displs(len, vrecvlengths, vrecvcounts, vdispls);
  
     MPI_Gatherv (C_c_u8vector(sendbuf), C_8vector_length(sendbuf), MPI_UNSIGNED_CHAR,
                  C_c_u8vector(recvbuf), vrecvcounts, vdispls, MPI_UNSIGNED_CHAR,
                  vroot, Comm_val(comm));
  }

  C_return (recvbuf);
}


C_word MPI_gatherv_s8vector (C_word sendbuf, C_word recvbuf, C_word recvlengths, 
		             C_word root, C_word comm, C_word recvcounts, C_word displs)
{
  int len, vroot; int *vrecvlengths, *vrecvcounts, *vdispls;

  MPI_check_comm (comm);

  vroot = (int)C_num_to_int (root);

  if (recvbuf == C_SCHEME_UNDEFINED)
  {
     MPI_Gatherv (C_c_s8vector(sendbuf), C_8vector_length(sendbuf), MPI_SIGNED_CHAR,
                  NULL, NULL, NULL, MPI_SIGNED_CHAR,
                  vroot, Comm_val(comm));
  }
  else
  {
     len           = C_32vector_length(recvlengths);
     vrecvlengths  = C_c_s32vector(recvlengths);
     vrecvcounts   = C_c_s32vector(recvcounts);
     vdispls       = C_c_s32vector(displs);

     MPI_counts_displs(len, vrecvlengths, vrecvcounts, vdispls);
  
     MPI_Gatherv (C_c_s8vector(sendbuf), C_8vector_length(sendbuf), MPI_SIGNED_CHAR,
                  C_c_s8vector(recvbuf), vrecvcounts, vdispls, MPI_SIGNED_CHAR,
                  vroot, Comm_val(comm));
  }

  C_return (recvbuf);
}


C_word MPI_gatherv_u16vector (C_word sendbuf, C_word recvbuf, C_word recvlengths, 
		             C_word root, C_word comm, C_word recvcounts, C_word displs)
{
  int len, vroot; int *vrecvlengths, *vrecvcounts, *vdispls;

  MPI_check_comm (comm);

  vroot = (int)C_num_to_int (root);

  if (recvbuf == C_SCHEME_UNDEFINED)
  {
     MPI_Gatherv (C_c_u16vector(sendbuf), C_16vector_length(sendbuf), MPI_UNSIGNED_SHORT,
                  NULL, NULL, NULL, MPI_UNSIGNED_SHORT,
                  vroot, Comm_val(comm));
  }
  else
  {
     len           = C_32vector_length(recvlengths);
     vrecvlengths  = C_c_s32vector(recvlengths);
     vrecvcounts   = C_c_s32vector(recvcounts);
     vdispls       = C_c_s32vector(displs);

     MPI_counts_displs(len, vrecvlengths, vrecvcounts, vdispls);
  
     MPI_Gatherv (C_c_u16vector(sendbuf), C_16vector_length(sendbuf), MPI_UNSIGNED_SHORT,
                  C_c_u16vector(recvbuf), vrecvcounts, vdispls, MPI_UNSIGNED_SHORT,
                  vroot, Comm_val(comm));
  }

  C_return (recvbuf);
}


C_word MPI_gatherv_s16vector (C_word sendbuf, C_word recvbuf, C_word recvlengths, 
			      C_word root, C_word comm, C_word recvcounts, C_word displs)
{
  int len, vroot; int *vrecvlengths, *vrecvcounts, *vdispls;

  MPI_check_comm (comm);

  vroot = (int)C_num_to_int (root);

  if (recvbuf == C_SCHEME_UNDEFINED)
  {
     MPI_Gatherv (C_c_s16vector(sendbuf), C_16vector_length(sendbuf), MPI_SHORT,
                  NULL, NULL, NULL, MPI_SHORT,
                  vroot, Comm_val(comm));
  }
  else
  {
     len           = C_32vector_length(recvlengths);
     vrecvlengths  = C_c_s32vector(recvlengths);
     vrecvcounts   = C_c_s32vector(recvcounts);
     vdispls       = C_c_s32vector(displs);

     MPI_counts_displs(len, vrecvlengths, vrecvcounts, vdispls);
  
     MPI_Gatherv (C_c_s16vector(sendbuf), C_16vector_length(sendbuf), MPI_SHORT,
                  C_c_s16vector(recvbuf), vrecvcounts, vdispls, MPI_SHORT,
                  vroot, Comm_val(comm));
  }

  C_return (recvbuf);
}



C_word MPI_gatherv_u32vector (C_word sendbuf, C_word recvbuf, C_word recvlengths, 
		              C_word root, C_word comm, C_word recvcounts, C_word displs)
{
  int len, vroot; int *vrecvlengths, *vrecvcounts, *vdispls;

  MPI_check_comm (comm);

  vroot = (int)C_num_to_int (root);

  if (recvbuf == C_SCHEME_UNDEFINED)
  {
     MPI_Gatherv (C_c_u32vector(sendbuf), C_32vector_length(sendbuf), MPI_UNSIGNED,
                  NULL, NULL, NULL, MPI_UNSIGNED, vroot, Comm_val(comm));
  }
  else
  {
     len           = C_32vector_length(recvlengths);
     vrecvlengths  = C_c_s32vector(recvlengths);
     vrecvcounts   = C_c_s32vector(recvcounts);
     vdispls       = C_c_s32vector(displs);

     MPI_counts_displs(len, vrecvlengths, vrecvcounts, vdispls);
  
     MPI_Gatherv (C_c_u32vector(sendbuf), C_32vector_length(sendbuf), MPI_UNSIGNED,
                  C_c_u32vector(recvbuf), vrecvcounts, vdispls, MPI_UNSIGNED,
                  vroot, Comm_val(comm));
  }

  C_return (recvbuf);
}


C_word MPI_gatherv_s32vector (C_word sendbuf, C_word recvbuf, C_word recvlengths, 
			      C_word root, C_word comm, C_word recvcounts, C_word displs)
{
  int len, vroot; int *vrecvlengths, *vrecvcounts, *vdispls;

  MPI_check_comm (comm);

  vroot = (int)C_num_to_int (root);

  if (recvbuf == C_SCHEME_UNDEFINED)
  {
     MPI_Gatherv (C_c_s32vector(sendbuf), C_32vector_length(sendbuf), MPI_INT,
                  NULL, NULL, NULL, MPI_INT,
                  vroot, Comm_val(comm));
  }
  else
  {
     len           = C_32vector_length(recvlengths);
     vrecvlengths  = C_c_s32vector(recvlengths);
     vrecvcounts   = C_c_s32vector(recvcounts);
     vdispls       = C_c_s32vector(displs);

     MPI_counts_displs(len, vrecvlengths, vrecvcounts, vdispls);
  
     MPI_Gatherv (C_c_s32vector(sendbuf), C_32vector_length(sendbuf), MPI_INT,
                  C_c_s32vector(recvbuf), vrecvcounts, vdispls, MPI_INT,
                  vroot, Comm_val(comm));
  }

  C_return (recvbuf);
}



C_word MPI_gatherv_f32vector (C_word sendbuf, C_word recvbuf, C_word recvlengths, 
			      C_word root, C_word comm, C_word recvcounts, C_word displs)
{
  int len, vroot; int *vrecvlengths, *vrecvcounts, *vdispls;

  MPI_check_comm (comm);

  vroot = (int)C_num_to_int (root);

  if (recvbuf == C_SCHEME_UNDEFINED)
  {
     MPI_Gatherv (C_c_f32vector(sendbuf), C_32vector_length(sendbuf), MPI_FLOAT,
                  NULL, NULL, NULL, MPI_FLOAT, vroot, Comm_val(comm));
  }
  else
  {
     len           = C_32vector_length(recvlengths);
     vrecvlengths  = C_c_s32vector(recvlengths);
     vrecvcounts   = C_c_s32vector(recvcounts);
     vdispls       = C_c_s32vector(displs);

     MPI_counts_displs(len, vrecvlengths, vrecvcounts, vdispls);
  
     MPI_Gatherv (C_c_s32vector(sendbuf), C_32vector_length(sendbuf), MPI_FLOAT,
                  C_c_s32vector(recvbuf), vrecvcounts, vdispls, MPI_FLOAT,
                  vroot, Comm_val(comm));
  }

  C_return (recvbuf);
}



C_word MPI_gatherv_f64vector (C_word sendbuf, C_word recvbuf, C_word recvlengths, 
			      C_word root, C_word comm, C_word recvcounts, C_word displs)
{
  int len, vroot; int *vrecvlengths, *vrecvcounts, *vdispls;

  MPI_check_comm (comm);

  vroot = (int)C_num_to_int (root);

  if (recvbuf == C_SCHEME_UNDEFINED)
  {
     MPI_Gatherv (C_c_f64vector(sendbuf), C_64vector_length(sendbuf), MPI_DOUBLE,
                  NULL, NULL, NULL, MPI_DOUBLE, vroot, Comm_val(comm));
  }
  else
  {
     len           = C_32vector_length(recvlengths);
     vrecvlengths  = C_c_s32vector(recvlengths);
     vrecvcounts   = C_c_s32vector(recvcounts);
     vdispls       = C_c_s32vector(displs);

     MPI_counts_displs(len, vrecvlengths, vrecvcounts, vdispls);
  
     MPI_Gatherv (C_c_f64vector(sendbuf), C_64vector_length(sendbuf), MPI_DOUBLE,
                  C_c_f64vector(recvbuf), vrecvcounts, vdispls, MPI_DOUBLE,
                  vroot, Comm_val(comm));
  }

  C_return (recvbuf);
}

<#


(define MPI_gather_u8vector (foreign-lambda scheme-object "MPI_gather_u8vector"
					    scheme-object scheme-object scheme-object scheme-object scheme-object))
(define MPI_gather_s8vector (foreign-lambda scheme-object "MPI_gather_s8vector"
					    scheme-object scheme-object scheme-object scheme-object scheme-object))

(define MPI_gather_u16vector (foreign-lambda scheme-object "MPI_gather_u16vector"
					     scheme-object scheme-object scheme-object scheme-object scheme-object))
(define MPI_gather_s16vector (foreign-lambda scheme-object "MPI_gather_s16vector"
					     scheme-object scheme-object scheme-object scheme-object scheme-object))

(define MPI_gather_u32vector (foreign-lambda scheme-object "MPI_gather_u32vector"
					     scheme-object scheme-object scheme-object scheme-object scheme-object))
(define MPI_gather_s32vector (foreign-lambda scheme-object "MPI_gather_s32vector"
					     scheme-object scheme-object scheme-object scheme-object scheme-object))


(define MPI_gather_f32vector (foreign-lambda scheme-object "MPI_gather_f32vector"
					     scheme-object scheme-object scheme-object scheme-object scheme-object))
(define MPI_gather_f64vector (foreign-lambda scheme-object "MPI_gather_f64vector"
					     scheme-object scheme-object scheme-object scheme-object scheme-object))


(define MPI_gather_bytevector (foreign-lambda scheme-object "MPI_gather_bytevector" 
					      scheme-object scheme-object scheme-object scheme-object scheme-object ))


(define (make-gather make-obj obj-len gather)
  (lambda (v sendcount root comm)
    (let ((myself (MPI:comm-rank comm))
	  (nprocs (MPI:comm-size comm)))
      (if (not (= root myself))
	  ;; If this is not the root process, send the data to the root
	  (if (<= sendcount (obj-len v))
	      (gather v sendcount (void) root comm)
	      (error 'MPI:gather "data length is less than sendcount"))
	  ;; Otherwise, the root process allocates a buffer and
	  ;; receives the data
	  (let ((recv  (make-obj (* nprocs sendcount))))
	    (gather v sendcount recv root comm))))))


(define (MPI:gather-int send root comm)
  (let ((nprocs (MPI:comm-size comm))
	(myself (MPI:comm-rank comm)))
    (if (= myself root)
	(MPI_gather_int send (make-s32vector nprocs 0) root comm)
	(MPI_gather_int send (void) root comm))))


(define (MPI:gather-flonum send root comm)
  (let ((nprocs (MPI:comm-size comm))
	(myself (MPI:comm-rank comm)))
    (if (= myself root)
	(MPI_gather_flonum send (make-f64vector nprocs 0) root comm)
	(MPI_gather_flonum send (void) root comm))))


(define MPI:gather-bytevector (make-gather make-blob blob-size MPI_gather_bytevector))
	  
(define-syntax define-srfi4-gather
  (lambda (x r c)
    (let* ((type      (cadr x))
	   (%define   (r 'define))
	   (name      (string->symbol (string-append "MPI:gather-" (symbol->string type) "vector")))
	   (makev     (string->symbol (string-append "make-" (symbol->string type) "vector")))
	   (vlen      (string->symbol (string-append (symbol->string type) "vector-length")))
	   (gather    (string->symbol (string-append "MPI_gather_" (symbol->string type) "vector"))))
       `(,%define ,name (make-gather ,makev ,vlen ,gather)))))

(define-srfi4-gather s8)
(define-srfi4-gather u8)
(define-srfi4-gather s16)
(define-srfi4-gather u16)
(define-srfi4-gather s32)
(define-srfi4-gather u32)
(define-srfi4-gather f32)
(define-srfi4-gather f64)


(define MPI_gatherv_bytevector (foreign-lambda scheme-object "MPI_gatherv_bytevector" 
					       scheme-object scheme-object scheme-object 
					       scheme-object scheme-object scheme-object 
					       scheme-object ))

(define MPI_gatherv_u8vector (foreign-lambda scheme-object "MPI_gatherv_u8vector" 
					     scheme-object scheme-object scheme-object 
					     scheme-object scheme-object scheme-object 
					     scheme-object ))
(define MPI_gatherv_s8vector (foreign-lambda scheme-object "MPI_gatherv_s8vector" 
					     scheme-object scheme-object scheme-object 
					     scheme-object scheme-object scheme-object 
					     scheme-object ))


(define MPI_gatherv_u16vector (foreign-lambda scheme-object "MPI_gatherv_u16vector" 
					      scheme-object scheme-object scheme-object 
					      scheme-object scheme-object scheme-object 
					      scheme-object ))
(define MPI_gatherv_s16vector (foreign-lambda scheme-object "MPI_gatherv_s16vector" 
					      scheme-object scheme-object scheme-object 
					      scheme-object scheme-object scheme-object 
					      scheme-object ))


(define MPI_gatherv_u32vector (foreign-lambda scheme-object "MPI_gatherv_u32vector" 
					      scheme-object scheme-object scheme-object 
					      scheme-object scheme-object scheme-object 
					      scheme-object ))
(define MPI_gatherv_s32vector (foreign-lambda scheme-object "MPI_gatherv_s32vector" 
					      scheme-object scheme-object scheme-object 
					      scheme-object scheme-object scheme-object 
					      scheme-object ))


(define MPI_gatherv_f32vector (foreign-lambda scheme-object "MPI_gatherv_f32vector" 
					      scheme-object scheme-object scheme-object 
					      scheme-object scheme-object scheme-object 
					      scheme-object ))
(define MPI_gatherv_f64vector (foreign-lambda scheme-object "MPI_gatherv_f64vector" 
					      scheme-object scheme-object scheme-object 
					      scheme-object scheme-object scheme-object 
					      scheme-object ))



(define (make-gatherv vlen makev simemcpy gatherv)
  (lambda  (data root comm)
    (let ((myself (MPI:comm-rank comm))
	  (nprocs (MPI:comm-size comm))
	  (mylen (vlen data)))
      (if (= root myself)
	  ;; Gather the lengths of the data from all processes
	  (let ((recvlengths (MPI_gather_int mylen (make-s32vector nprocs) root comm)))
	    ;; Allocate receive buffer 
	    (let* ((total    (apply + (s32vector->list recvlengths)))
		   (recvbuf  (makev total)))
	      ;; Gather the data
	      (gatherv data recvbuf recvlengths root comm
		       (make-s32vector nprocs)
		       (make-s32vector nprocs))
	      ;; Build a list of results & return
	      (let loop ((i 0) (offset 0) (lst (list)))
		(if (< i nprocs)
		    (let* ((len   (s32vector-ref recvlengths i))
			   (vect  (makev len)))
		      (simemcpy vect recvbuf len offset)
		      (loop (+ 1 i) (+ offset len) (cons vect lst)))
		    (reverse lst)))))
	  
	  ;; If not root, send our length
	  (let ((ignore (MPI_gather_int mylen (void) root comm)))
	    ;; Send our data
	    (gatherv data (void) (void) root comm (void) (void))
	    (void))))))


(define MPI:gatherv-bytevector (make-gatherv blob-size make-blob bytevector_simemcpy MPI_gatherv_bytevector))
	  
(define-syntax define-srfi4-gatherv
  (lambda (x r c)
    (let* ((type      (cadr x))
	   (%define   (r 'define))
	   (vlen      (string->symbol (string-append (symbol->string type) "vector-length")))
	   (makev     (string->symbol (string-append "make-" (symbol->string type) "vector")))
	   (simemcpy  (string->symbol (string-append (symbol->string type) "vector_simemcpy")))
	   (gatherv   (string->symbol (string-append "MPI_gatherv_" (symbol->string type) "vector")))
	   (name      (string->symbol (string-append "MPI:gatherv-" (symbol->string type) "vector"))))
       `(,%define ,name (make-gatherv ,vlen ,makev ,simemcpy ,gatherv)))))

(define-srfi4-gatherv s8)
(define-srfi4-gatherv u8)
(define-srfi4-gatherv s16)
(define-srfi4-gatherv u16)
(define-srfi4-gatherv s32)
(define-srfi4-gatherv u32)
(define-srfi4-gatherv f32)
(define-srfi4-gatherv f64)
					   

;; Gather  to all

(define MPI_allgather_int 
    (foreign-primitive scheme-object ((integer send)
				      (scheme-object recv)
				      (scheme-object comm))
#<<END
  int *vrecv; 
  C_word result; 

  MPI_check_comm(comm);

  vrecv  = C_c_s32vector(recv);

  MPI_Allgather(&send, 1, MPI_INT, vrecv, 1, MPI_INT, Comm_val(comm));
  result = recv;

  C_return (result);
END
))

(define MPI_allgather_flonum 
    (foreign-primitive scheme-object ((double send)
				      (scheme-object recv)
				      (scheme-object comm))
#<<END
  double *vrecv; 
  C_word result; 

  MPI_check_comm(comm);

  vrecv  = C_c_f64vector(recv);

  MPI_Allgather(&send, 1, MPI_DOUBLE, vrecv, 1, MPI_DOUBLE, Comm_val(comm));
  result = recv;

  C_return (result);
END
))

#>

C_word MPI_allgather_bytevector (C_word sendbuf, C_word recvbuf, C_word recvlengths, 
				 C_word comm, C_word recvcounts, C_word displs)
{
  int len; int *vrecvlengths, *vrecvcounts, *vdispls;

  MPI_check_comm (comm);

  C_i_check_bytevector (sendbuf);
  C_i_check_bytevector (recvbuf);

  len           = C_32vector_length(recvlengths);
  vrecvlengths  = C_c_s32vector(recvlengths);
  vrecvcounts   = C_c_s32vector(recvcounts);
  vdispls       = C_c_s32vector(displs);

  MPI_counts_displs(len, vrecvlengths, vrecvcounts, vdispls);
  
  MPI_Allgatherv (C_c_bytevector(sendbuf), C_bytevector_length(sendbuf), MPI_BYTE,
	          C_c_bytevector(recvbuf), vrecvcounts, vdispls, MPI_BYTE,
	          Comm_val(comm));

  C_return (recvbuf);
}


C_word MPI_allgather_s8vector (C_word sendbuf, C_word recvbuf, C_word recvlengths, 
			       C_word comm, C_word recvcounts, C_word displs)
{
  int len; int *vrecvlengths, *vrecvcounts, *vdispls;
  char *vsend, *vrecv;
  MPI_check_comm (comm);

  vsend  = C_c_s8vector(sendbuf);
  vrecv  = C_c_s8vector(recvbuf);

  len           = C_32vector_length(recvlengths);
  vrecvlengths  = C_c_s32vector(recvlengths);
  vrecvcounts   = C_c_s32vector(recvcounts);
  vdispls       = C_c_s32vector(displs);
  
  MPI_counts_displs(len, vrecvlengths, vrecvcounts, vdispls);
  
  MPI_Allgatherv (vsend, C_8vector_length(sendbuf), MPI_SIGNED_CHAR,
	          vrecv, vrecvcounts, vdispls, MPI_SIGNED_CHAR,
	          Comm_val(comm));

  C_return (recvbuf);
}

C_word MPI_allgather_u8vector (C_word sendbuf, C_word recvbuf, C_word recvlengths, 
			       C_word comm, C_word recvcounts, C_word displs)
{
  int len; int *vrecvlengths, *vrecvcounts, *vdispls;
  char *vsend, *vrecv;
  MPI_check_comm (comm);

  vsend  = C_c_u8vector(sendbuf);
  vrecv  = C_c_u8vector(recvbuf);

  len           = C_32vector_length(recvlengths);
  vrecvlengths  = C_c_s32vector(recvlengths);
  vrecvcounts   = C_c_s32vector(recvcounts);
  vdispls       = C_c_s32vector(displs);
  
  MPI_counts_displs(len, vrecvlengths, vrecvcounts, vdispls);
  
  MPI_Allgatherv (vsend, C_8vector_length(sendbuf), MPI_UNSIGNED_CHAR,
	          vrecv, vrecvcounts, vdispls, MPI_UNSIGNED_CHAR,
	          Comm_val(comm));

  C_return (recvbuf);
}


C_word MPI_allgather_s16vector (C_word sendbuf, C_word recvbuf, C_word recvlengths, 
			        C_word comm, C_word recvcounts, C_word displs)
{
  int len; int *vrecvlengths, *vrecvcounts, *vdispls;
  short *vsend, *vrecv;
  MPI_check_comm (comm);

  vsend  = C_c_s16vector(sendbuf);
  vrecv  = C_c_s16vector(recvbuf);

  len           = C_32vector_length(recvlengths);
  vrecvlengths  = C_c_s32vector(recvlengths);
  vrecvcounts   = C_c_s32vector(recvcounts);
  vdispls       = C_c_s32vector(displs);
  
  MPI_counts_displs(len, vrecvlengths, vrecvcounts, vdispls);
  
  MPI_Allgatherv (vsend, C_16vector_length(sendbuf), MPI_SHORT,
	          vrecv, vrecvcounts, vdispls, MPI_SHORT,
	          Comm_val(comm));

  C_return (recvbuf);
}


C_word MPI_allgather_u16vector (C_word sendbuf, C_word recvbuf, C_word recvlengths, 
			        C_word comm, C_word recvcounts, C_word displs)
{
  int len; int *vrecvlengths, *vrecvcounts, *vdispls;
  unsigned short *vsend, *vrecv;
  MPI_check_comm (comm);

  vsend  = C_c_u16vector(sendbuf);
  vrecv  = C_c_u16vector(recvbuf);

  len           = C_32vector_length(recvlengths);
  vrecvlengths  = C_c_s32vector(recvlengths);
  vrecvcounts   = C_c_s32vector(recvcounts);
  vdispls       = C_c_s32vector(displs);
  
  MPI_counts_displs(len, vrecvlengths, vrecvcounts, vdispls);
  
  MPI_Allgatherv (vsend, C_16vector_length(sendbuf), MPI_UNSIGNED_SHORT,
	          vrecv, vrecvcounts, vdispls, MPI_UNSIGNED_SHORT,
	          Comm_val(comm));

  C_return (recvbuf);
}



C_word MPI_allgather_s32vector (C_word sendbuf, C_word recvbuf, C_word recvlengths, 
			        C_word comm, C_word recvcounts, C_word displs)
{
  int len; int *vrecvlengths, *vrecvcounts, *vdispls;
  int *vsend, *vrecv;
  MPI_check_comm (comm);

  vsend  = C_c_s32vector(sendbuf);
  vrecv  = C_c_s32vector(recvbuf);

  len           = C_32vector_length(recvlengths);
  vrecvlengths  = C_c_s32vector(recvlengths);
  vrecvcounts   = C_c_s32vector(recvcounts);
  vdispls       = C_c_s32vector(displs);
  
  MPI_counts_displs(len, vrecvlengths, vrecvcounts, vdispls);
  
  MPI_Allgatherv (vsend, C_32vector_length(sendbuf), MPI_INT,
	          vrecv, vrecvcounts, vdispls, MPI_INT,
	          Comm_val(comm));

  C_return (recvbuf);
}


C_word MPI_allgather_u32vector (C_word sendbuf, C_word recvbuf, C_word recvlengths, 
			        C_word comm, C_word recvcounts, C_word displs)
{
  int len; int *vrecvlengths, *vrecvcounts, *vdispls;
  unsigned int *vsend, *vrecv;
  MPI_check_comm (comm);

  vsend  = C_c_u32vector(sendbuf);
  vrecv  = C_c_u32vector(recvbuf);

  len           = C_32vector_length(recvlengths);
  vrecvlengths  = C_c_s32vector(recvlengths);
  vrecvcounts   = C_c_s32vector(recvcounts);
  vdispls       = C_c_s32vector(displs);
  
  MPI_counts_displs(len, vrecvlengths, vrecvcounts, vdispls);
  
  MPI_Allgatherv (vsend, C_32vector_length(sendbuf), MPI_UNSIGNED,
	          vrecv, vrecvcounts, vdispls, MPI_UNSIGNED,
	          Comm_val(comm));

  C_return (recvbuf);
}


C_word MPI_allgather_f32vector (C_word sendbuf, C_word recvbuf, C_word recvlengths, 
			        C_word comm, C_word recvcounts, C_word displs)
{
  int len; int *vrecvlengths, *vrecvcounts, *vdispls;
  float *vsend, *vrecv;
  MPI_check_comm (comm);

  vsend  = C_c_f32vector(sendbuf);
  vrecv  = C_c_f32vector(recvbuf);

  len           = C_32vector_length(recvlengths);
  vrecvlengths  = C_c_s32vector(recvlengths);
  vrecvcounts   = C_c_s32vector(recvcounts);
  vdispls       = C_c_s32vector(displs);
  
  MPI_counts_displs(len, vrecvlengths, vrecvcounts, vdispls);
  
  MPI_Allgatherv (vsend, C_32vector_length(sendbuf), MPI_FLOAT,
	          vrecv, vrecvcounts, vdispls, MPI_FLOAT,
	          Comm_val(comm));

  C_return (recvbuf);
}



C_word MPI_allgather_f64vector (C_word sendbuf, C_word recvbuf, C_word recvlengths, 
			        C_word comm, C_word recvcounts, C_word displs)
{
  int len; int *vrecvlengths, *vrecvcounts, *vdispls;
  double *vsend, *vrecv;
  MPI_check_comm (comm);

  vsend  = C_c_f64vector(sendbuf);
  vrecv  = C_c_f64vector(recvbuf);

  len           = C_32vector_length(recvlengths);
  vrecvlengths  = C_c_s32vector(recvlengths);
  vrecvcounts   = C_c_s32vector(recvcounts);
  vdispls       = C_c_s32vector(displs);
  
  MPI_counts_displs(len, vrecvlengths, vrecvcounts, vdispls);
  
  MPI_Allgatherv (vsend, C_64vector_length(sendbuf), MPI_DOUBLE,
	          vrecv, vrecvcounts, vdispls, MPI_DOUBLE,
	          Comm_val(comm));

  C_return (recvbuf);
}

<#



(define MPI_allgather_s8vector (foreign-lambda scheme-object "MPI_allgather_s8vector" 
					       scheme-object scheme-object scheme-object scheme-object 
					       scheme-object scheme-object ))

(define MPI_allgather_u8vector (foreign-lambda scheme-object "MPI_allgather_u8vector" 
					       scheme-object scheme-object scheme-object scheme-object 
					       scheme-object scheme-object ))

(define MPI_allgather_s16vector (foreign-lambda scheme-object "MPI_allgather_s16vector" 
						scheme-object scheme-object scheme-object scheme-object
						scheme-object scheme-object ))

(define MPI_allgather_u16vector (foreign-lambda scheme-object "MPI_allgather_u16vector" 
						scheme-object scheme-object scheme-object scheme-object
						scheme-object scheme-object ))

(define MPI_allgather_s32vector (foreign-lambda scheme-object "MPI_allgather_s32vector" 
						scheme-object scheme-object scheme-object scheme-object
						scheme-object scheme-object ))

(define MPI_allgather_u32vector (foreign-lambda scheme-object "MPI_allgather_u32vector" 
						scheme-object scheme-object scheme-object scheme-object
						scheme-object scheme-object ))

(define MPI_allgather_f32vector (foreign-lambda scheme-object "MPI_allgather_f32vector" 
						scheme-object scheme-object scheme-object scheme-object 
						scheme-object scheme-object ))

(define MPI_allgather_f64vector (foreign-lambda scheme-object "MPI_allgather_f64vector" 
						scheme-object scheme-object scheme-object scheme-object 
						scheme-object scheme-object ))

(define MPI_allgather_bytevector (foreign-lambda scheme-object "MPI_allgather_bytevector" 
						 scheme-object scheme-object scheme-object scheme-object
						 scheme-object scheme-object ))


(define (make-allgather vlen makev simemcpy allgather)
  (lambda (v root comm)
    (let ((myself (MPI:comm-rank comm))
	  (nprocs (MPI:comm-size comm)))
      ;; gather lengths for all data
      (let ((lengths (MPI_allgather_int (vlen v) (make-s32vector nprocs 0) comm)))
	;; allocate a buffer and gather the data
	(let ((recv  (makev (apply + (s32vector->list lengths)))))
	  (allgather v recv lengths comm (make-s32vector nprocs 0) (make-s32vector nprocs 0))
	  ;; Build a list of results & return
	  (let loop ((i 0) (offset 0) (lst (list)))
	    (if (< i nprocs)
		(let* ((len   (s32vector-ref lengths i))
		       (vect  (makev len)))
		  (simemcpy vect recv len offset)
		  (loop (+ 1 i) (+ offset len) (cons vect lst)))
		(reverse lst))))))))


(define (MPI:allgather-int send root comm)
  (let ((nprocs (MPI:comm-size comm)))
    (MPI_allgather_int send (make-s32vector nprocs 0) comm)))

(define (MPI:allgather-flonum send root comm)
  (let ((nprocs (MPI:comm-size comm)))
    (MPI_allgather_flonum send (make-f64vector nprocs 0) comm)))

(define MPI:allgather-bytevector (make-allgather blob-size make-blob bytevector_simemcpy MPI_allgather_bytevector))
	  
(define-syntax define-srfi4-allgather
  (lambda (x r c)
    (let* ((type      (cadr x))
	   (%define   (r 'define))
	   (vlen      (string->symbol (string-append (symbol->string type) "vector-length")))
	   (makev     (string->symbol (string-append "make-" (symbol->string type) "vector")))
	   (simemcpy  (string->symbol (string-append (symbol->string type) "vector_simemcpy")))
	   (allgather (string->symbol (string-append "MPI_allgather_" (symbol->string type) "vector")))
	   (name      (string->symbol (string-append "MPI:allgather-" (symbol->string type) "vector"))))
       `(,%define ,name (make-allgather ,vlen ,makev ,simemcpy ,allgather)))))

(define-srfi4-allgather s8)
(define-srfi4-allgather u8)
(define-srfi4-allgather s16)
(define-srfi4-allgather u16)
(define-srfi4-allgather s32)
(define-srfi4-allgather u32)
(define-srfi4-allgather f32)
(define-srfi4-allgather f64)
					   

;; Reduce

(define MPI:i_max  0)
(define MPI:i_min  1)
(define MPI:i_sum  2)
(define MPI:i_prod 3)
(define MPI:i_land 4)
(define MPI:i_lor  5)
(define MPI:i_xor  6)

(define MPI:f_max  0)
(define MPI:f_min  1)
(define MPI:f_sum  2)
(define MPI:f_prod 3)

#>

static MPI_Op reduce_intop[] =
  { MPI_MAX, MPI_MIN, MPI_SUM, MPI_PROD, MPI_BAND, MPI_BOR, MPI_BXOR };

static MPI_Op reduce_floatop[] =
  { MPI_MAX, MPI_MIN, MPI_SUM, MPI_PROD };

<#


(define MPI_reduce_int 
    (foreign-primitive scheme-object ((integer data)
				      (integer op)
				      (integer root)
				      (integer myself)
				      (scheme-object comm))
#<<END
  int n;
  C_word result; C_word *ptr;

  MPI_check_comm(comm);

  if (myself == root)
  {
    n = 0;
    MPI_Reduce(&data, &n, 1, MPI_INT, reduce_intop[op], root, Comm_val(comm));

    ptr = C_alloc (C_SIZEOF_FLONUM);
    result = C_int_to_num (&ptr, n);
  }
  else
  {
    MPI_Reduce(&data, NULL, 1, MPI_INT, reduce_intop[op], root, Comm_val(comm));
    result = C_SCHEME_UNDEFINED;
  }

  C_return (result);
END
))



(define MPI_reduce_flonum 
    (foreign-primitive scheme-object ((double data)
				      (integer op)
				      (integer root)
				      (integer myself)
				      (scheme-object comm))
#<<END
  double n; C_word *ptr;
  C_word result; 

  MPI_check_comm(comm);

  if (myself == root)
  {
    n = 0;
    MPI_Reduce(&data, &n, 1, MPI_DOUBLE, reduce_floatop[op], root, Comm_val(comm));

    ptr = C_alloc (C_SIZEOF_FLONUM);
    result = C_flonum (&ptr, n);
  }
  else
  {
    MPI_Reduce(&data, NULL, 1, MPI_DOUBLE, reduce_floatop[op], root, Comm_val(comm));
    
    result = C_SCHEME_UNDEFINED;
  }

  C_return (result);
END
))


#>


C_word MPI_reduce_s8vector (C_word data, C_word recv, C_word op, C_word root, C_word comm)
{
  int vroot, vop; 
  char *vdata, *vrecv;
  C_word result;

  MPI_check_comm (comm);

  vdata  = C_c_s8vector(data);
  vroot  = (int)C_num_to_int (root);
  vop    = (int)C_num_to_int (op);

  if (recv == C_SCHEME_UNDEFINED)
  {
     MPI_Reduce (vdata, NULL, C_8vector_length(data), MPI_SIGNED_CHAR,
	         reduce_intop[vop], vroot, Comm_val(comm));
     result = C_SCHEME_UNDEFINED;
  }
  else
  {
     vrecv  = C_c_s8vector(recv);
     MPI_Reduce (vdata, vrecv, C_8vector_length(data), MPI_SIGNED_CHAR,
	         reduce_intop[vop], vroot, Comm_val(comm));
     result = recv;
  }

  C_return (result);
}


C_word MPI_reduce_u8vector (C_word data, C_word recv, C_word op, C_word root, C_word comm)
{
  int vroot, vop; 
  unsigned char *vdata, *vrecv;
  C_word result;

  MPI_check_comm (comm);

  vdata  = C_c_u8vector(data);
  vroot  = (int)C_num_to_int (root);
  vop    = (int)C_num_to_int (op);

  if (recv == C_SCHEME_UNDEFINED)
  {
     MPI_Reduce (vdata, NULL, C_8vector_length(data), MPI_UNSIGNED_CHAR,
	         reduce_intop[vop], vroot, Comm_val(comm));
     result = C_SCHEME_UNDEFINED;
  }
  else
  {
     vrecv  = C_c_u8vector(recv);
     MPI_Reduce (vdata, vrecv, C_8vector_length(data), MPI_UNSIGNED_CHAR,
	         reduce_intop[vop], vroot, Comm_val(comm));
     result = recv;
  }

  C_return (result);
}


C_word MPI_reduce_s16vector (C_word data, C_word recv, C_word op, C_word root, C_word comm)
{
  int vroot, vop; 
  short *vdata, *vrecv;
  C_word result;

  MPI_check_comm (comm);

  vdata  = C_c_s16vector(data);
  vroot  = (int)C_num_to_int (root);
  vop    = (int)C_num_to_int (op);

  if (recv == C_SCHEME_UNDEFINED)
  {
     MPI_Reduce (vdata, NULL, C_16vector_length(data), MPI_SHORT,
	         reduce_intop[vop], vroot, Comm_val(comm));
     result = C_SCHEME_UNDEFINED;
  }
  else
  {
     vrecv  = C_c_s16vector(recv);
     MPI_Reduce (vdata, vrecv, C_16vector_length(data), MPI_SHORT,
	         reduce_intop[vop], vroot, Comm_val(comm));
     result = recv;
  }

  C_return (result);
}


C_word MPI_reduce_u16vector (C_word data, C_word recv, C_word op, C_word root, C_word comm)
{
  int vroot, vop; 
  unsigned short *vdata, *vrecv;
  C_word result;

  MPI_check_comm (comm);

  vdata  = C_c_u16vector(data);
  vroot  = (int)C_num_to_int (root);
  vop    = (int)C_num_to_int (op);

  if (recv == C_SCHEME_UNDEFINED)
  {
     MPI_Reduce (vdata, NULL, C_16vector_length(data), MPI_UNSIGNED_SHORT,
	         reduce_intop[vop], vroot, Comm_val(comm));
     result = C_SCHEME_UNDEFINED;
  }
  else
  {
     vrecv  = C_c_u16vector(recv);
     MPI_Reduce (vdata, vrecv, C_16vector_length(data), MPI_UNSIGNED_SHORT,
	         reduce_intop[vop], vroot, Comm_val(comm));
     result = recv;
  }

  C_return (result);
}


C_word MPI_reduce_s32vector (C_word data, C_word recv, C_word op, C_word root, C_word comm)
{
  int vroot, vop; 
  int *vdata, *vrecv;
  C_word result;

  MPI_check_comm (comm);

  vdata  = C_c_s32vector(data);
  vroot  = (int)C_num_to_int (root);
  vop    = (int)C_num_to_int (op);

  if (recv == C_SCHEME_UNDEFINED)
  {
     MPI_Reduce (vdata, NULL, C_32vector_length(data), MPI_INT,
	         reduce_intop[vop], vroot, Comm_val(comm));
     result = C_SCHEME_UNDEFINED;
  }
  else
  {
     vrecv  = C_c_s32vector(recv);
     MPI_Reduce (vdata, vrecv, C_32vector_length(data), MPI_INT,
	         reduce_intop[vop], vroot, Comm_val(comm));
     result = recv;
  }

  C_return (result);
}


C_word MPI_reduce_u32vector (C_word data, C_word recv, C_word op, C_word root, C_word comm)
{
  int vroot, vop; 
  unsigned int *vdata, *vrecv;
  C_word result;

  MPI_check_comm (comm);

  vdata  = C_c_u32vector(data);
  vroot  = (int)C_num_to_int (root);
  vop    = (int)C_num_to_int (op);

  if (recv == C_SCHEME_UNDEFINED)
  {
     MPI_Reduce (vdata, NULL, C_32vector_length(data), MPI_UNSIGNED,
	         reduce_intop[vop], vroot, Comm_val(comm));
     result = C_SCHEME_UNDEFINED;
  }
  else
  {
     vrecv  = C_c_u32vector(recv);
     MPI_Reduce (vdata, vrecv, C_32vector_length(data), MPI_UNSIGNED,
	         reduce_intop[vop], vroot, Comm_val(comm));
     result = recv;
  }

  C_return (result);
}


C_word MPI_reduce_f32vector (C_word data, C_word recv, C_word op, C_word root, C_word comm)
{
  int vroot, vop; 
  float *vdata, *vrecv;
  C_word result;

  MPI_check_comm (comm);

  vdata  = C_c_f32vector(data);
  vroot  = (int)C_num_to_int (root);
  vop    = (int)C_num_to_int (op);

  if (recv == C_SCHEME_UNDEFINED)
  {
     MPI_Reduce (vdata, NULL, C_32vector_length(data), MPI_FLOAT,
	         reduce_floatop[vop], vroot, Comm_val(comm));
     result = C_SCHEME_UNDEFINED;
  }
  else
  {
     vrecv  = C_c_f32vector(recv);
     MPI_Reduce (vdata, vrecv, C_32vector_length(data), MPI_FLOAT,
	         reduce_floatop[vop], vroot, Comm_val(comm));
     result = recv;
  }

  C_return (result);
}


C_word MPI_reduce_f64vector (C_word data, C_word recv, C_word op, C_word root, C_word comm)
{
  int vroot, vop; 
  double *vdata, *vrecv;
  C_word result;

  MPI_check_comm (comm);

  vdata  = C_c_f64vector(data);
  vroot  = (int)C_num_to_int (root);
  vop    = (int)C_num_to_int (op);

  if (recv == C_SCHEME_UNDEFINED)
  {
     MPI_Reduce (vdata, NULL, C_64vector_length(data), MPI_DOUBLE,
	         reduce_floatop[vop], vroot, Comm_val(comm));
     result = C_SCHEME_UNDEFINED;
  }
  else
  {
     vrecv  = C_c_f64vector(recv);
     MPI_Reduce (vdata, vrecv, C_64vector_length(data), MPI_DOUBLE,
	         reduce_floatop[vop], vroot, Comm_val(comm));
     result = recv;
  }

  C_return (result);
}

<#

(define MPI_reduce_s8vector (foreign-lambda scheme-object "MPI_reduce_s8vector" 
					    scheme-object scheme-object scheme-object scheme-object scheme-object ))

(define MPI_reduce_u8vector (foreign-lambda scheme-object "MPI_reduce_u8vector" 
					    scheme-object scheme-object scheme-object scheme-object scheme-object ))


(define MPI_reduce_s16vector (foreign-lambda scheme-object "MPI_reduce_s16vector" 
					     scheme-object scheme-object scheme-object scheme-object scheme-object ))

(define MPI_reduce_u16vector (foreign-lambda scheme-object "MPI_reduce_u16vector" 
					     scheme-object scheme-object scheme-object scheme-object scheme-object ))


(define MPI_reduce_s32vector (foreign-lambda scheme-object "MPI_reduce_s32vector" 
					     scheme-object scheme-object scheme-object scheme-object scheme-object ))

(define MPI_reduce_u32vector (foreign-lambda scheme-object "MPI_reduce_u32vector" 
					     scheme-object scheme-object scheme-object scheme-object scheme-object ))


(define MPI_reduce_f32vector (foreign-lambda scheme-object "MPI_reduce_f32vector" 
					     scheme-object scheme-object scheme-object scheme-object scheme-object ))

(define MPI_reduce_f64vector (foreign-lambda scheme-object "MPI_reduce_f32vector" 
					     scheme-object scheme-object scheme-object scheme-object scheme-object ))

(define (make-reduce vlen makev reduce)
  (lambda (send op root comm)
    (let ((len    (vlen send))
	  (myself (MPI:comm-rank comm)))
      (if (= root myself)
	  (reduce send (makev len) op root comm)
	  (reduce send (void) op root comm)))))

(define (MPI:reduce-int send op root comm)
  (let ((myself (MPI:comm-rank comm)))
     (MPI_reduce_int send op root myself comm)))

(define (MPI:reduce-flonum send op root comm)
  (let ((myself (MPI:comm-rank comm)))
    (MPI_reduce_flonum send op root myself comm)))

	  
(define-syntax define-srfi4-reduce
  (lambda (x r c)
    (let* ((type      (cadr x))
	   (%define   (r 'define))
	   (vlen      (string->symbol (string-append (symbol->string type) "vector-length")))
	   (makev     (string->symbol (string-append "make-" (symbol->string type) "vector")))
	   (reduce    (string->symbol (string-append "MPI_reduce_" (symbol->string type) "vector")))
	   (name      (string->symbol (string-append "MPI:reduce-" (symbol->string type) "vector"))))
       `(,%define ,name (make-reduce ,vlen ,makev ,reduce)))))

(define-srfi4-reduce s8)
(define-srfi4-reduce u8)
(define-srfi4-reduce s16)
(define-srfi4-reduce u16)
(define-srfi4-reduce s32)
(define-srfi4-reduce u32)
(define-srfi4-reduce f32)
(define-srfi4-reduce f64)
					   

;; Reduce at all nodes 


(define MPI_allreduce_int 
    (foreign-primitive scheme-object ((integer data)
				      (integer op)
				      (scheme-object comm))
#<<END
  int n; 
  C_word result; C_word *ptr;

  MPI_check_comm(comm);

  n = 0;
  MPI_Allreduce(&data, &n, 1, MPI_INT, reduce_intop[op], Comm_val(comm));

  ptr = C_alloc (C_SIZEOF_FLONUM);
  result = C_int_to_num (&ptr, n);

  C_return (result);
END
))


(define MPI_allreduce_flonum 
    (foreign-primitive scheme-object ((double data)
				      (integer op)
				      (scheme-object comm))
#<<END
  double n; C_word *ptr;
  C_word result; 

  MPI_check_comm(comm);

  n = 0;
  MPI_Allreduce(&data, &n, 1, MPI_DOUBLE, reduce_floatop[op], Comm_val(comm));

  ptr = C_alloc (C_SIZEOF_FLONUM);
  result = C_flonum (&ptr, n);

  C_return (result);
END
))


#>


C_word MPI_allreduce_s8vector (C_word data, C_word recv, C_word op, C_word comm)
{
  int vop; 
  char *vdata, *vrecv;
  C_word result;

  MPI_check_comm (comm);

  vdata  = C_c_s8vector(data);
  vrecv  = C_c_s8vector(recv);
  vop    = (int)C_num_to_int (op);

  MPI_Allreduce (vdata, vrecv, C_8vector_length(data), MPI_SIGNED_CHAR,
	         reduce_intop[vop], Comm_val(comm));
  result = recv;

  C_return (result);
}


C_word MPI_allreduce_u8vector (C_word data, C_word recv, C_word op, C_word comm)
{
  int vop; 
  unsigned char *vdata, *vrecv;
  C_word result;

  MPI_check_comm (comm);

  vdata  = C_c_u8vector(data);
  vrecv  = C_c_u8vector(recv);
  vop    = (int)C_num_to_int (op);

  MPI_Allreduce (vdata, vrecv, C_8vector_length(data), MPI_UNSIGNED_CHAR,
	         reduce_intop[vop], Comm_val(comm));
  result = recv;

  C_return (result);
}


C_word MPI_allreduce_s16vector (C_word data, C_word recv, C_word op, C_word comm)
{
  int vop; 
  short *vdata, *vrecv;
  C_word result;

  MPI_check_comm (comm);

  vdata  = C_c_s16vector(data);
  vrecv  = C_c_s16vector(recv);
  vop    = (int)C_num_to_int (op);

  MPI_Allreduce (vdata, vrecv, C_16vector_length(data), MPI_SHORT,
	         reduce_intop[vop], Comm_val(comm));
  result = recv;

  C_return (result);
}


C_word MPI_allreduce_u16vector (C_word data, C_word recv, C_word op, C_word comm)
{
  int vop; 
  unsigned short *vdata, *vrecv;
  C_word result;

  MPI_check_comm (comm);

  vdata  = C_c_u16vector(data);
  vrecv  = C_c_u16vector(recv);
  vop    = (int)C_num_to_int (op);

  MPI_Allreduce (vdata, vrecv, C_16vector_length(data), MPI_UNSIGNED_SHORT,
	         reduce_intop[vop], Comm_val(comm));
  result = recv;

  C_return (result);
}


C_word MPI_allreduce_s32vector (C_word data, C_word recv, C_word op, C_word comm)
{
  int vop; 
  int *vdata, *vrecv;
  C_word result;

  MPI_check_comm (comm);

  vdata  = C_c_s32vector(data);
  vrecv  = C_c_s32vector(recv);
  vop    = (int)C_num_to_int (op);

  MPI_Allreduce (vdata, vrecv, C_32vector_length(data), MPI_INT,
	         reduce_intop[vop], Comm_val(comm));
  result = recv;

  C_return (result);
}


C_word MPI_allreduce_u32vector (C_word data, C_word recv, C_word op, C_word comm)
{
  int vop; 
  unsigned int *vdata, *vrecv;
  C_word result;

  MPI_check_comm (comm);

  vdata  = C_c_u32vector(data);
  vrecv  = C_c_u32vector(recv);
  vop    = (int)C_num_to_int (op);

  MPI_Allreduce (vdata, vrecv, C_32vector_length(data), MPI_UNSIGNED,
	         reduce_intop[vop], Comm_val(comm));
  result = recv;

  C_return (result);
}


C_word MPI_allreduce_f32vector (C_word data, C_word recv, C_word op, C_word comm)
{
  int vop; 
  float *vdata, *vrecv;
  C_word result;

  MPI_check_comm (comm);

  vdata  = C_c_f32vector(data);
  vrecv  = C_c_f32vector(recv);
  vop    = (int)C_num_to_int (op);

  MPI_Allreduce (vdata, vrecv, C_32vector_length(data), MPI_FLOAT,
	         reduce_floatop[vop], Comm_val(comm));
  result = recv;

  C_return (result);
}


C_word MPI_allreduce_f64vector (C_word data, C_word recv, C_word op, C_word comm)
{
  int vop; 
  double *vdata, *vrecv;
  C_word result;

  MPI_check_comm (comm);

  vdata  = C_c_f64vector(data);
  vrecv  = C_c_f64vector(recv);
  vop    = (int)C_num_to_int (op);

  MPI_Allreduce (vdata, vrecv, C_64vector_length(data), MPI_DOUBLE,
	         reduce_floatop[vop], Comm_val(comm));
  result = recv;

  C_return (result);
}

<#

(define MPI_allreduce_s8vector (foreign-lambda scheme-object "MPI_allreduce_s8vector" 
					       scheme-object scheme-object scheme-object scheme-object ))

(define MPI_allreduce_u8vector (foreign-lambda scheme-object "MPI_allreduce_u8vector" 
					       scheme-object scheme-object scheme-object scheme-object ))

(define MPI_allreduce_s16vector (foreign-lambda scheme-object "MPI_allreduce_s16vector" 
						scheme-object scheme-object scheme-object scheme-object ))

(define MPI_allreduce_u16vector (foreign-lambda scheme-object "MPI_allreduce_u16vector" 
						scheme-object scheme-object scheme-object scheme-object ))

(define MPI_allreduce_s32vector (foreign-lambda scheme-object "MPI_allreduce_s32vector" 
						scheme-object scheme-object scheme-object scheme-object ))

(define MPI_allreduce_u32vector (foreign-lambda scheme-object "MPI_allreduce_u32vector" 
						scheme-object scheme-object scheme-object scheme-object ))


(define MPI_allreduce_f32vector (foreign-lambda scheme-object "MPI_allreduce_f32vector" 
						scheme-object scheme-object scheme-object scheme-object ))

(define MPI_allreduce_f64vector (foreign-lambda scheme-object "MPI_allreduce_f64vector" 
						scheme-object scheme-object scheme-object scheme-object ))


(define (make-allreduce vlen makev allreduce)
  (lambda (send op comm)
    (let ((len    (vlen send)))
      (allreduce send (makev len) op comm))))

(define (MPI:allreduce-int send op comm)
  (MPI_allreduce_int send op comm))

(define (MPI:allreduce-flonum send op comm)
  (MPI_allreduce_flonum send op comm))
	  
(define-syntax define-srfi4-allreduce
  (lambda (x r c)
    (let* ((type       (cadr x))
	   (%define    (r 'define))
	   (vlen       (string->symbol (string-append (symbol->string type) "vector-length")))
	   (makev      (string->symbol (string-append "make-" (symbol->string type) "vector")))
	   (allreduce  (string->symbol (string-append "MPI_allreduce_" (symbol->string type) "vector")))
	   (name       (string->symbol (string-append "MPI:allreduce-" (symbol->string type) "vector"))))
       `(,%define ,name (make-allreduce ,vlen ,makev ,allreduce)))))

(define-srfi4-allreduce s8)
(define-srfi4-allreduce u8)
(define-srfi4-allreduce s16)
(define-srfi4-allreduce u16)
(define-srfi4-allreduce s32)
(define-srfi4-allreduce u32)
(define-srfi4-allreduce f32)
(define-srfi4-allreduce f64)
					   

;; Scan

(define MPI_scan_int 
    (foreign-primitive scheme-object ((integer data)
				      (integer op)
				      (scheme-object comm))
#<<END
  int n; 
  C_word result; C_word *ptr;

  MPI_check_comm(comm);

  n = 0;
  MPI_Scan(&data, &n, 1, MPI_INT, reduce_intop[op], Comm_val(comm));

  ptr = C_alloc (C_SIZEOF_FLONUM);
  result = C_int_to_num (&ptr, n);

  C_return (result);
END
))


(define MPI_scan_flonum 
    (foreign-primitive scheme-object ((double data)
				      (integer op)
				      (scheme-object comm))
#<<END
  double n; C_word *ptr;
  C_word result; 

  MPI_check_comm(comm);

  n = 0;
  MPI_Scan(&data, &n, 1, MPI_DOUBLE, reduce_floatop[op], Comm_val(comm));

  ptr = C_alloc (C_SIZEOF_FLONUM);
  result = C_flonum (&ptr, n);

  C_return (result);
END
))


#>


C_word MPI_scan_s8vector (C_word data, C_word recv, C_word op, C_word comm)
{
  int vop; 
  char *vdata, *vrecv;
  C_word result;

  MPI_check_comm (comm);

  vdata  = C_c_s8vector(data);
  vrecv  = C_c_s8vector(recv);
  vop    = (int)C_num_to_int (op);

  MPI_Scan (vdata, vrecv, C_8vector_length(data), MPI_SIGNED_CHAR,
	         reduce_intop[vop], Comm_val(comm));
  result = recv;

  C_return (result);
}


C_word MPI_scan_u8vector (C_word data, C_word recv, C_word op, C_word comm)
{
  int vop; 
  unsigned char *vdata, *vrecv;
  C_word result;

  MPI_check_comm (comm);

  vdata  = C_c_u8vector(data);
  vrecv  = C_c_u8vector(recv);
  vop    = (int)C_num_to_int (op);

  MPI_Scan (vdata, vrecv, C_8vector_length(data), MPI_UNSIGNED_CHAR,
	         reduce_intop[vop], Comm_val(comm));
  result = recv;

  C_return (result);
}


C_word MPI_scan_s16vector (C_word data, C_word recv, C_word op, C_word comm)
{
  int vop; 
  short *vdata, *vrecv;
  C_word result;

  MPI_check_comm (comm);

  vdata  = C_c_s16vector(data);
  vrecv  = C_c_s16vector(recv);
  vop    = (int)C_num_to_int (op);

  MPI_Scan (vdata, vrecv, C_16vector_length(data), MPI_SHORT,
	         reduce_intop[vop], Comm_val(comm));
  result = recv;

  C_return (result);
}


C_word MPI_scan_u16vector (C_word data, C_word recv, C_word op, C_word comm)
{
  int vop; 
  unsigned short *vdata, *vrecv;
  C_word result;

  MPI_check_comm (comm);

  vdata  = C_c_u16vector(data);
  vrecv  = C_c_u16vector(recv);
  vop    = (int)C_num_to_int (op);

  MPI_Scan (vdata, vrecv, C_16vector_length(data), MPI_UNSIGNED_SHORT,
	         reduce_intop[vop], Comm_val(comm));
  result = recv;

  C_return (result);
}


C_word MPI_scan_s32vector (C_word data, C_word recv, C_word op, C_word comm)
{
  int vop; 
  int *vdata, *vrecv;
  C_word result;

  MPI_check_comm (comm);

  vdata  = C_c_s32vector(data);
  vrecv  = C_c_s32vector(recv);
  vop    = (int)C_num_to_int (op);

  MPI_Scan (vdata, vrecv, C_32vector_length(data), MPI_INT,
	         reduce_intop[vop], Comm_val(comm));
  result = recv;

  C_return (result);
}


C_word MPI_scan_u32vector (C_word data, C_word recv, C_word op, C_word comm)
{
  int vop; 
  unsigned int *vdata, *vrecv;
  C_word result;

  MPI_check_comm (comm);

  vdata  = C_c_u32vector(data);
  vrecv  = C_c_u32vector(recv);
  vop    = (int)C_num_to_int (op);

  MPI_Scan (vdata, vrecv, C_32vector_length(data), MPI_UNSIGNED,
	         reduce_intop[vop], Comm_val(comm));
  result = recv;

  C_return (result);
}


C_word MPI_scan_f32vector (C_word data, C_word recv, C_word op, C_word comm)
{
  int vop; 
  float *vdata, *vrecv;
  C_word result;

  MPI_check_comm (comm);

  vdata  = C_c_f32vector(data);
  vrecv  = C_c_f32vector(recv);
  vop    = (int)C_num_to_int (op);

  MPI_Scan (vdata, vrecv, C_32vector_length(data), MPI_FLOAT,
	         reduce_floatop[vop], Comm_val(comm));
  result = recv;

  C_return (result);
}


C_word MPI_scan_f64vector (C_word data, C_word recv, C_word op, C_word comm)
{
  int vop; 
  double *vdata, *vrecv;
  C_word result;

  MPI_check_comm (comm);

  vdata  = C_c_f64vector(data);
  vrecv  = C_c_f64vector(recv);
  vop    = (int)C_num_to_int (op);

  MPI_Scan (vdata, vrecv, C_64vector_length(data), MPI_DOUBLE,
	         reduce_floatop[vop], Comm_val(comm));
  result = recv;

  C_return (result);
}

<#

(define MPI_scan_s8vector (foreign-lambda scheme-object "MPI_scan_s8vector" 
					  scheme-object scheme-object scheme-object scheme-object ))

(define MPI_scan_u8vector (foreign-lambda scheme-object "MPI_scan_u8vector" 
					  scheme-object scheme-object scheme-object scheme-object ))

(define MPI_scan_s16vector (foreign-lambda scheme-object "MPI_scan_s16vector" 
					   scheme-object scheme-object scheme-object scheme-object ))

(define MPI_scan_u16vector (foreign-lambda scheme-object "MPI_scan_u16vector" 
					   scheme-object scheme-object scheme-object scheme-object ))

(define MPI_scan_s32vector (foreign-lambda scheme-object "MPI_scan_s32vector" 
					   scheme-object scheme-object scheme-object scheme-object ))

(define MPI_scan_u32vector (foreign-lambda scheme-object "MPI_scan_u32vector" 
					   scheme-object scheme-object scheme-object scheme-object ))


(define MPI_scan_f32vector (foreign-lambda scheme-object "MPI_scan_f32vector" 
					   scheme-object scheme-object scheme-object scheme-object ))

(define MPI_scan_f64vector (foreign-lambda scheme-object "MPI_scan_f64vector" 
					   scheme-object scheme-object scheme-object scheme-object ))


(define (make-scan vlen makev scan)
  (lambda (send op comm)
    (let ((len    (vlen send)))
      (scan send (makev len) op comm))))

(define (MPI:scan-int send op comm)
  (MPI_scan_int send op comm))

(define (MPI:scan-flonum send op comm)
  (MPI_scan_flonum send op comm))


	  
(define-syntax define-srfi4-scan
  (lambda (x r c)
    (let* ((type       (cadr x))
	   (%define    (r 'define))
	   (vlen       (string->symbol (string-append (symbol->string type) "vector-length")))
	   (makev      (string->symbol (string-append "make-" (symbol->string type) "vector")))
	   (scan       (string->symbol (string-append "MPI_scan_" (symbol->string type) "vector")))
	   (name       (string->symbol (string-append "MPI:scan-" (symbol->string type) "vector"))))
       `(,%define ,name (make-scan ,vlen ,makev ,scan)))))

(define-srfi4-scan s8)
(define-srfi4-scan u8)
(define-srfi4-scan s16)
(define-srfi4-scan u16)
(define-srfi4-scan s32)
(define-srfi4-scan u32)
(define-srfi4-scan f32)
(define-srfi4-scan f64)
