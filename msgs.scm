
;;
;; Chicken MPI interface. Based on the Caml/MPI interface by Xavier
;; Leroy.
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


;; Point-to-point communication 


; Include into generated code, but don't parse:
#>

C_word MPI_send_fixnum (C_word data, C_word dest, C_word tag, C_word comm)
{
  int n, vdest, vtag;

  MPI_check_comm(comm);

  n = C_unfix(data);
  vdest = (int)C_num_to_int (dest);
  vtag  = (int)C_num_to_int (tag);

  MPI_Send(&n, 1, MPI_INT, vdest, vtag, Comm_val(comm));

  C_return(C_SCHEME_UNDEFINED);
}


C_word MPI_send_int (C_word data, C_word dest, C_word tag, C_word comm)
{
  long n; int vdest, vtag;

  MPI_check_comm(comm);

  n = C_num_to_long(data);
  vdest = (int)C_num_to_int (dest);
  vtag  = (int)C_num_to_int (tag);

  MPI_Send(&n, 1, MPI_LONG, vdest, vtag, Comm_val(comm));

  C_return(C_SCHEME_UNDEFINED);
}

C_word MPI_send_flonum (C_word data, C_word dest, C_word tag, C_word comm)
{
  double n; int vdest, vtag;

  MPI_check_comm(comm);

  n = C_c_double(data);
  vdest = (int)C_num_to_int (dest);
  vtag  = (int)C_num_to_int (tag);

  MPI_Send(&n, 1, MPI_DOUBLE, vdest, vtag, Comm_val(comm));

  C_return(C_SCHEME_UNDEFINED);
}

C_word MPI_send_u8vector (C_word data, C_word dest, C_word tag, C_word comm)
{
  unsigned char *vect; int len, vdest, vtag;

  MPI_check_comm(comm);

  vect  = C_c_u8vector(data);
  len   = C_8vector_length(data);
  vdest = (int)C_num_to_int (dest);
  vtag  = (int)C_num_to_int (tag);

  MPI_Send(vect, len, MPI_UNSIGNED_CHAR, vdest, vtag, Comm_val(comm));

  C_return(C_SCHEME_UNDEFINED);
}


C_word MPI_send_s8vector (C_word data, C_word dest, C_word tag, C_word comm)
{
  char *vect; int len, vdest, vtag;

  MPI_check_comm(comm);

  vect  = C_c_s8vector(data);
  len   = C_8vector_length(data);
  vdest = (int)C_num_to_int (dest);
  vtag  = (int)C_num_to_int (tag);

  MPI_Send(vect, len, MPI_SIGNED_CHAR, vdest, vtag, Comm_val(comm));

  C_return(C_SCHEME_UNDEFINED);
}

C_word MPI_send_u16vector (C_word data, C_word dest, C_word tag, C_word comm)
{
  unsigned short *vect; int len, vdest, vtag;

  MPI_check_comm(comm);

  vect  = C_c_u16vector(data);
  len   = C_16vector_length(data);
  vdest = (int)C_num_to_int (dest);
  vtag  = (int)C_num_to_int (tag);

  MPI_Send(vect, len, MPI_UNSIGNED_SHORT, vdest, vtag, Comm_val(comm));

  C_return(C_SCHEME_UNDEFINED);
}


C_word MPI_send_s16vector (C_word data, C_word dest, C_word tag, C_word comm)
{
  short *vect; int len, vdest, vtag;

  MPI_check_comm(comm);

  vect  = C_c_s16vector(data);
  len   = C_16vector_length(data);
  vdest = (int)C_num_to_int (dest);
  vtag  = (int)C_num_to_int (tag);

  MPI_Send(vect, len, MPI_SHORT, vdest, vtag, Comm_val(comm));

  C_return(C_SCHEME_UNDEFINED);
}


C_word MPI_send_u32vector (C_word data, C_word dest, C_word tag, C_word comm)
{
  unsigned int *vect; int len, vdest, vtag;

  MPI_check_comm(comm);

  vect  = C_c_u32vector(data);
  len   = C_32vector_length(data);
  vdest = (int)C_num_to_int (dest);
  vtag  = (int)C_num_to_int (tag);

  MPI_Send(vect, len, MPI_UNSIGNED, vdest, vtag, Comm_val(comm));

  C_return(C_SCHEME_UNDEFINED);
}


C_word MPI_send_s32vector (C_word data, C_word dest, C_word tag, C_word comm)
{
  int *vect; int len, vdest, vtag;

  MPI_check_comm(comm);

  vect  = C_c_s32vector(data);
  len   = C_32vector_length(data);
  vdest = (int)C_num_to_int (dest);
  vtag  = (int)C_num_to_int (tag);

  MPI_Send(vect, len, MPI_INT, vdest, vtag, Comm_val(comm));

  C_return(C_SCHEME_UNDEFINED);
}


C_word MPI_send_f32vector (C_word data, C_word dest, C_word tag, C_word comm)
{
  float *vect; int len, vdest, vtag;

  MPI_check_comm(comm);

  vect  = C_c_f32vector(data);
  len   = C_32vector_length(data);
  vdest = (int)C_num_to_int (dest);
  vtag  = (int)C_num_to_int (tag);

  MPI_Send(vect, len, MPI_FLOAT, vdest, vtag, Comm_val(comm));

  C_return(C_SCHEME_UNDEFINED);
}


C_word MPI_send_f64vector (C_word data, C_word dest, C_word tag, C_word comm)
{
  double *vect; int len, vdest, vtag;

  MPI_check_comm(comm);

  vect  = C_c_f64vector(data);
  len   = C_64vector_length(data);
  vdest = (int)C_num_to_int (dest);
  vtag  = (int)C_num_to_int (tag);

  MPI_Send(vect, len, MPI_DOUBLE, vdest, vtag, Comm_val(comm));

  C_return(C_SCHEME_UNDEFINED);
}


C_word MPI_send_bytevector (C_word data, C_word dest, C_word tag, C_word comm)
{
  char * buffer;
  int len; int vdest, vtag;

  MPI_check_comm(comm);
  C_i_check_bytevector (data);

  vdest = (int)C_num_to_int (dest);
  vtag  = (int)C_num_to_int (tag);

  len = C_bytevector_length (data);
  buffer = C_c_bytevector (data);

  MPI_Send(buffer, len, MPI_BYTE, vdest, vtag, Comm_val(comm));

  C_return(C_SCHEME_UNDEFINED);
}

<#

;; Sending data

(define MPI:send-fixnum (foreign-lambda scheme-object "MPI_send_fixnum" 
					scheme-object scheme-object scheme-object scheme-object ))
(define MPI:send-int (foreign-lambda scheme-object "MPI_send_int" 
				     scheme-object scheme-object scheme-object scheme-object ))
(define MPI:send-flonum (foreign-lambda scheme-object "MPI_send_flonum" 
					scheme-object scheme-object scheme-object scheme-object ))

(define MPI:send-u8vector (foreign-lambda scheme-object "MPI_send_u8vector" 
					  scheme-object scheme-object scheme-object scheme-object ))
(define MPI:send-s8vector (foreign-lambda scheme-object "MPI_send_s8vector" 
					  scheme-object scheme-object scheme-object scheme-object ))
(define MPI:send-u16vector (foreign-lambda scheme-object "MPI_send_u16vector" 
					   scheme-object scheme-object scheme-object scheme-object ))
(define MPI:send-s16vector (foreign-lambda scheme-object "MPI_send_s16vector" 
					   scheme-object scheme-object scheme-object scheme-object ))
(define MPI:send-u32vector (foreign-lambda scheme-object "MPI_send_u32vector" 
					   scheme-object scheme-object scheme-object scheme-object ))
(define MPI:send-s32vector (foreign-lambda scheme-object "MPI_send_s32vector" 
					   scheme-object scheme-object scheme-object scheme-object ))
(define MPI:send-f32vector (foreign-lambda scheme-object "MPI_send_f32vector" 
					   scheme-object scheme-object scheme-object scheme-object ))
(define MPI:send-f64vector (foreign-lambda scheme-object "MPI_send_f64vector" 
					   scheme-object scheme-object scheme-object scheme-object ))

(define MPI_send_bytevector (foreign-lambda scheme-object "MPI_send_bytevector" 
					    scheme-object scheme-object scheme-object scheme-object ))

(define (MPI:send-bytevector blob dest tag comm)
  (MPI_send_bytevector blob dest tag comm))
  
(define (MPI:send x dest tag comm)
  (cond ((fixnum? x)    (MPI:send-fixnum x dest tag comm))
	((blob? x)      (MPI:send-bytevector x dest tag comm))
	((integer? x)   (MPI:send-int x dest tag comm))
	((number? x)    (MPI:send-flonum x dest tag comm))
	((s8vector? x)  (MPI:send-s8vector x dest tag comm))
	((u8vector? x)  (MPI:send-u8vector x dest tag comm))
	((s16vector? x) (MPI:send-s16vector x dest tag comm))
	((u16vector? x) (MPI:send-u16vector x dest tag comm))
	((s32vector? x) (MPI:send-s32vector x dest tag comm))
	((u32vector? x) (MPI:send-u32vector x dest tag comm))
	((f32vector? x) (MPI:send-f32vector x dest tag comm))
	((f64vector? x) (MPI:send-f64vector x dest tag comm))
	(else (error 'MPI:send "unknown object type: " x))))
	
	

;; Probe for pending messages and determine length 
(define MPI:probe 
    (foreign-primitive ((integer source)
			(integer tag)
			(scheme-object comm))
#<<EOF
  MPI_Status status;
  int count;
  C_word status_count, status_source, status_tag;
  C_word *ptr;

  MPI_check_comm(comm);

  MPI_Probe(source, tag, Comm_val(comm), &status);
  MPI_Get_count(&status, MPI_BYTE, &count);

  status_count = C_fix(count);

  ptr = C_alloc (C_SIZEOF_FLONUM);
  status_source = C_int_to_num (&ptr, status.MPI_SOURCE);

  ptr = C_alloc (C_SIZEOF_FLONUM);
  status_tag = C_int_to_num (&ptr, status.MPI_TAG);

#if defined(C_BINARY_VERSION) && (C_BINARY_VERSION >= 8)
  C_word rval[5] = { C_SCHEME_UNDEFINED, C_k, status_count, status_source, status_tag };
  C_values(5, rval);
#else
  C_values(5, C_SCHEME_UNDEFINED, C_k, status_count, status_source, status_tag );
#endif
EOF
))

(define MPI:receive-int 
    (foreign-primitive scheme-object ((integer source)
				      (integer tag)
				      (scheme-object comm))
#<<EOF
  long n; 
  C_word result; C_word *ptr;

  MPI_check_comm(comm);

  MPI_Recv(&n, 1, MPI_LONG, source, tag, Comm_val(comm), MPI_STATUS_IGNORE);

  ptr = C_alloc (C_SIZEOF_FLONUM);
  result = C_long_to_num (&ptr, n);
  
  C_return(result);
EOF
))

(define MPI:receive-flonum 
    (foreign-primitive scheme-object ((integer source)
				      (integer tag)
				      (scheme-object comm))
#<<EOF
  double n; C_word *ptr;
  C_word result; 

  MPI_check_comm(comm);

  MPI_Recv(&n, 1, MPI_DOUBLE, source, tag, Comm_val(comm), MPI_STATUS_IGNORE);

  ptr = C_alloc (C_SIZEOF_FLONUM);
  result = C_flonum (&ptr, n);

  C_return(result);
EOF
))


(define MPI:receive-fixnum
    (foreign-primitive scheme-object ((integer source)
				      (integer tag)
				      (scheme-object comm))
#<<EOF
  int n; 
  C_word result; 

  MPI_check_comm(comm);

  MPI_Recv(&n, 1, MPI_INT, source, tag, Comm_val(comm), MPI_STATUS_IGNORE);

  C_return(C_fix(n));
EOF
))

#>

C_word MPI_receive_u8vector (C_word data, C_word source, C_word tag, C_word comm)
{
  unsigned char *vect; int len, vsource, vtag;

  MPI_check_comm(comm);

  vsource = (int)C_num_to_int (source);
  vtag    = (int)C_num_to_int (tag);

  vect    = C_c_u8vector(data);
  len     = C_8vector_length(data);

  MPI_Recv(vect, len, MPI_UNSIGNED_CHAR, vsource, vtag, Comm_val(comm), MPI_STATUS_IGNORE);

  C_return(data);
}


C_word MPI_receive_s8vector (C_word data, C_word source, C_word tag, C_word comm)
{
  char *vect; int len, vsource, vtag;

  MPI_check_comm(comm);

  vect    = C_c_s8vector(data);
  len     = C_8vector_length(data);
  vsource = (int)C_num_to_int (source);
  vtag    = (int)C_num_to_int (tag);

  MPI_Recv(vect, len, MPI_SIGNED_CHAR, vsource, vtag, Comm_val(comm), MPI_STATUS_IGNORE);

  C_return(data);
}


C_word MPI_receive_u16vector (C_word data, C_word source, C_word tag, C_word comm)
{
  unsigned short *vect; int len, vsource, vtag;

  MPI_check_comm(comm);

  vect    = C_c_u16vector(data);
  len     = C_16vector_length(data);
  vsource = (int)C_num_to_int (source);
  vtag    = (int)C_num_to_int (tag);

  MPI_Recv(vect, len, MPI_UNSIGNED_SHORT, vsource, vtag, Comm_val(comm), MPI_STATUS_IGNORE);

  C_return(data);
}


C_word MPI_receive_s16vector (C_word data, C_word source, C_word tag, C_word comm)
{
  short *vect; int len, vsource, vtag;

  MPI_check_comm(comm);

  vect    = C_c_s16vector(data);
  len     = C_16vector_length(data);
  vsource = (int)C_num_to_int (source);
  vtag    = (int)C_num_to_int (tag);

  MPI_Recv(vect, len, MPI_SHORT, vsource, vtag, Comm_val(comm), MPI_STATUS_IGNORE);

  C_return(data);
}


C_word MPI_receive_u32vector (C_word data, C_word source, C_word tag, C_word comm)
{
  unsigned int *vect; int len, vsource, vtag;

  MPI_check_comm(comm);

  vect    = C_c_u32vector(data);
  len     = C_32vector_length(data);
  vsource = (int)C_num_to_int (source);
  vtag    = (int)C_num_to_int (tag);

  MPI_Recv(vect, len, MPI_UNSIGNED, vsource, vtag, Comm_val(comm), MPI_STATUS_IGNORE);

  C_return(data);
}


C_word MPI_receive_s32vector (C_word data, C_word source, C_word tag, C_word comm)
{
  int *vect; int len, vsource, vtag;

  MPI_check_comm(comm);

  vect    = C_c_s32vector(data);
  len     = C_32vector_length(data);
  vsource = (int)C_num_to_int (source);
  vtag    = (int)C_num_to_int (tag);

  MPI_Recv(vect, len, MPI_INT, vsource, vtag, Comm_val(comm), MPI_STATUS_IGNORE);

  C_return(data);
}


C_word MPI_receive_f32vector (C_word data, C_word source, C_word tag, C_word comm)
{
  float *vect; int len, vsource, vtag;

  MPI_check_comm(comm);

  vect    = C_c_f32vector(data);
  len     = C_32vector_length(data);
  vsource = (int)C_num_to_int (source);
  vtag    = (int)C_num_to_int (tag);

  MPI_Recv(vect, len, MPI_FLOAT, vsource, vtag, Comm_val(comm), MPI_STATUS_IGNORE);

  C_return(data);
}


C_word MPI_receive_f64vector (C_word data, C_word source, C_word tag, C_word comm)
{
  double *vect; int len, vsource, vtag;

  MPI_check_comm(comm);

  vect    = C_c_f64vector(data);
  len     = C_64vector_length(data);
  vsource = (int)C_num_to_int (source);
  vtag    = (int)C_num_to_int (tag);

  MPI_Recv(vect, len, MPI_DOUBLE, vsource, vtag, Comm_val(comm), MPI_STATUS_IGNORE);

  C_return(data);
}


C_word MPI_receive_bytevector (C_word data, C_word source, C_word tag, C_word comm)
{
  char * buffer;
  long len; int vsource, vtag;

  MPI_check_comm(comm);
  C_i_check_bytevector (data);
  
  vsource = (int)C_num_to_int (source);
  vtag  = (int)C_num_to_int (tag);

  len = C_bytevector_length (data);
  buffer = C_c_bytevector (data);

  MPI_Recv(buffer, len, MPI_BYTE, vsource, vtag, Comm_val(comm), MPI_STATUS_IGNORE);

  C_return (data);
}


<#


;; Receiving data


(define MPI_receive_u8vector (foreign-lambda scheme-object "MPI_receive_u8vector" 
					     scheme-object scheme-object scheme-object scheme-object ))
(define MPI_receive_s8vector (foreign-lambda scheme-object "MPI_receive_s8vector" 
					     scheme-object scheme-object scheme-object scheme-object ))
(define MPI_receive_u16vector (foreign-lambda scheme-object "MPI_receive_u16vector" 
					      scheme-object scheme-object scheme-object scheme-object ))
(define MPI_receive_s16vector (foreign-lambda scheme-object "MPI_receive_s16vector" 
					      scheme-object scheme-object scheme-object scheme-object ))
(define MPI_receive_u32vector (foreign-lambda scheme-object "MPI_receive_u32vector" 
					      scheme-object scheme-object scheme-object scheme-object ))
(define MPI_receive_s32vector (foreign-lambda scheme-object "MPI_receive_s32vector" 
					      scheme-object scheme-object scheme-object scheme-object ))
(define MPI_receive_f32vector (foreign-lambda scheme-object "MPI_receive_f32vector" 
					      scheme-object scheme-object scheme-object scheme-object ))
(define MPI_receive_f64vector (foreign-lambda scheme-object "MPI_receive_f64vector" 
					      scheme-object scheme-object scheme-object scheme-object ))

(define MPI_receive_bytevector (foreign-lambda scheme-object "MPI_receive_bytevector" 
					       scheme-object scheme-object scheme-object scheme-object ))

(define (make-receive makev recv)
  (lambda (len source tag comm)
    (let ((buffer (makev len)))
      (recv buffer source tag comm))))


(define MPI:receive-bytevector (make-receive make-blob MPI_receive_bytevector))

(define-syntax define-srfi4-receive
  (lambda (x r c)
    (let* ((type    (cadr x))
	   (%define (r 'define))
	   (makev   (string->symbol (string-append "make-" (symbol->string type) "vector")))
	   (recv    (string->symbol (string-append "MPI_receive_" (symbol->string type) "vector")))
	   (name    (string->symbol (string-append "MPI:receive-" (symbol->string type) "vector"))))
       `(,%define ,name (make-receive ,makev ,recv)))))

(define-srfi4-receive s8)
(define-srfi4-receive u8)
(define-srfi4-receive s16)
(define-srfi4-receive u16)
(define-srfi4-receive s32)
(define-srfi4-receive u32)
(define-srfi4-receive f32)
(define-srfi4-receive f64)


(define (MPI:receive source tag comm)
  (let-values (((len actual-source actual-tag) (MPI:probe source tag comm)))
    (MPI:receive-bytevector len actual-source actual-tag comm)))

(define (MPI:receive-with-status source tag comm)
  (let-values (((len actual-source actual-tag) (MPI:probe source tag comm)))
    (let ((v (MPI:receive-bytevector len source tag comm)))
      (values v actual-source actual-tag))))


;; Auxiliaries 
#>
int MPI_get_any_tag(void)
{
  return MPI_ANY_TAG;
}

int MPI_get_any_source (void)
{
  return (MPI_ANY_SOURCE);
}
<#

(define MPI_get_any_tag     (foreign-lambda integer "MPI_get_any_tag"))
(define MPI_get_any_source  (foreign-lambda integer "MPI_get_any_source"))

(define MPI:any-tag (MPI_get_any_tag))
(define MPI:any-source (MPI_get_any_source))

