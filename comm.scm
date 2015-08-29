
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


;; Handling of communicators 

; Include into generated code, but don't parse:
#>

static C_word MPI_comm_p(C_word obj) 
{
  if (C_immediatep(obj)) {
    return C_SCHEME_FALSE;
  } else if (C_block_header(obj) == MPI_COMM_TAG) 
  {
    return C_SCHEME_TRUE;
  } else {
    return C_SCHEME_FALSE;
  }
}


static C_word MPI_check_comm (C_word obj) 
{
  if (C_immediatep(obj)) 
  {
    chicken_MPI_exception (MPI_ERR_COMM, 32, "invalid MPI communicator object");
  } else if (C_block_header(obj) == MPI_COMM_TAG) 
  {
    return C_SCHEME_UNDEFINED;
  } else {
    chicken_MPI_exception (MPI_ERR_COMM, 32, "invalid MPI communicator object");
  }
}

<#


(define MPI:comm? (foreign-lambda scheme-object "MPI_comm_p" scheme-object))

(define MPI_comm_finalizer 
    (foreign-safe-lambda* void ((scheme-object comm))
#<<END
   MPI_Comm *x;
   MPI_check_comm (comm);

   x = Comm_val (comm);

   MPI_Comm_free (x);
END
))

(define MPI_alloc_comm 
    (foreign-primitive scheme-object ((nonnull-c-pointer comm)
				      (scheme-object finalizer))
#<<END

   C_word result;
   chicken_MPI_comm_t newcomm;

   newcomm.tag = MPI_COMM_TAG;
   newcomm.comm_data = comm;
   result = (C_word)&newcomm;

   //C_do_register_finalizer(result, finalizer);
   
   C_return (result);
END
))

(define MPI_comm_world
    (foreign-primitive nonnull-c-pointer ()
#<<END
   C_word result;

   result = (C_word)MPI_COMM_WORLD;

   C_return (result);
END
))

(define (MPI:get-comm-world)
  (let ((w (MPI_comm_world)))
    (MPI_alloc_comm w MPI_comm_finalizer)))


(define MPI:comm-size
    (foreign-primitive scheme-object ((scheme-object x))
#<<END
   C_word *ptr;
   C_word result;
   int size;

   if (MPI_comm_p (x))
     {
       MPI_Comm_size (Comm_val(x), &size);
       ptr = C_alloc (C_SIZEOF_FLONUM);
       result = C_int_to_num (&ptr, size);
     }
   else
     {
       result = C_SCHEME_FALSE;
     }

   C_return (result);
END
))


(define MPI:comm-rank
    (foreign-primitive scheme-object ((scheme-object x))
#<<END
   C_word result;
   C_word *ptr;
   int rank;

   if (MPI_comm_p (x))
     {
       MPI_Comm_rank (Comm_val(x), &rank);
       ptr = C_alloc (C_SIZEOF_FLONUM);
       result = C_int_to_num (&ptr, rank);
     }
   else
     {
       result = C_SCHEME_FALSE;
     }

   C_return (result);
END
))


#>
C_word MPI_comm_compare(C_word comm1, C_word comm2)
{
  int res;
  res = 0;

  if ((MPI_comm_p (comm1)) && (MPI_comm_p (comm2)))
  {

    MPI_Comm_compare(Comm_val(comm1), Comm_val(comm2), &res);
  }

  if (res == 0) 
     return C_SCHEME_TRUE;
  else
     return C_SCHEME_FALSE;
}
<#

(define MPI:comm-equal? (foreign-lambda scheme-object "MPI_comm_compare" scheme-object scheme-object))

(define MPI_comm_split
    (foreign-primitive nonnull-c-pointer ((scheme-object comm) (integer color) (integer key))
#<<END
  C_word result;

  MPI_Comm newcomm;

  MPI_check_comm (comm);
  if (MPI_comm_p (comm))
     {
       MPI_Comm_split(Comm_val(comm), color, key, &newcomm);
       result = (C_word)newcomm;
     }
   else
     {
       result = (C_word)NULL;
     };

  C_return(result);
END
))

(define (MPI:comm-split comm color split)
  (MPI_alloc_comm (MPI_comm_split comm color split) MPI_comm_finalizer))

#>
C_word MPI_get_undefined(void)
{
  return C_fix(MPI_UNDEFINED);
}
<#


(define MPI_comm_create
   (foreign-primitive nonnull-c-pointer ((scheme-object comm) (scheme-object group))
#<<END
  C_word result;
  MPI_Comm newcomm;
  
  MPI_check_comm (comm);
  if ((MPI_comm_p (comm)) && (MPI_group_p (group)))
  {
     MPI_Comm_create(Comm_val(comm), Group_val(group), &newcomm);
     result = (C_word)newcomm;
  }
  else
  {
     result = (C_word)NULL;
  }

  C_return (result);
END
))

(define (MPI:comm-create comm group)
  (MPI_alloc_comm (MPI_comm_create comm group) MPI_comm_finalizer))

(define MPI:undefined (foreign-lambda scheme-object "MPI_get_undefined"))

(define MPI_cart_create
    (foreign-primitive nonnull-c-pointer ((scheme-object comm)
					  (integer ndims)
					  (integer nperiods)
					  (nonnull-u32vector dims)
					  (nonnull-u32vector periods)
					  (bool reorder))
#<<END
  MPI_Comm newcomm;
  C_word result;

  MPI_check_comm (comm);
  if (MPI_comm_p (comm))
     {
      MPI_Cart_create(Comm_val(comm), ndims, dims, periods, 
		      reorder, &newcomm);
      result = (C_word)newcomm;
     }
   else
     {
         result = (C_word)NULL;
     }

   C_return(result);
END
))

(define (MPI:make-cart comm dims periods reorder)
  (MPI_alloc_comm
   (MPI_cart_create 
    comm (u32vector-length dims) (u32vector-length periods)
    dims periods reorder)
   MPI_comm_finalizer))

(define MPI_dims_create
    (foreign-primitive scheme-object ((integer nnodes)
				      (integer ndims)
				      (scheme-object dims))
#<<END
  C_word result;

  unsigned int *vdims;

  if (C_vectorp(dims))
  {
     vdims = C_c_u32vector(dims);
     MPI_Dims_create(nnodes, ndims, vdims);
     result = dims;
  }
  else
  {
     result = C_SCHEME_FALSE;
  };

  C_return(result);
END
))


(define (MPI:make-dims nnodes ndims)
  (if (integer? ndims)
      (MPI_dims_create nnodes ndims (make-u32vector ndims))
      (MPI_dims_create nnodes (u32vector-length ndims) ndims)))

(define MPI:cart-rank
  (foreign-primitive scheme-object ((scheme-object comm)
				    (scheme-object coords))
#<<END
  C_word *ptr;
  C_word result;
  int ndims, rank;
  int * vcoords;

  if ((C_vectorp(coords)) && (MPI_comm_p (comm)))
  {
    ndims = C_u_i_32vector_length(coords);
    vcoords = C_c_s32vector(coords);
    MPI_Cart_rank(Comm_val(comm), vcoords, &rank);
    ptr = C_alloc (C_SIZEOF_FLONUM);
    result = C_int_to_num (&ptr, rank);
  }
  else
  {
    result = C_SCHEME_FALSE;
  }

  C_return (result);
END
))

(define MPI_cart_dim
  (foreign-primitive scheme-object ((scheme-object comm))
#<<END
     C_word result;
     int ndims;

     if (MPI_comm_p (comm))
     {
        MPI_Cartdim_get(Comm_val(comm), &ndims);
        result = C_fix(ndims);
     }
     else
     {
        result = C_SCHEME_FALSE;
     }

    C_return (result);
END
))

(define MPI_cart_coords
  (foreign-primitive scheme-object ((scheme-object comm)
				    (integer rank)
				    (integer ndims)
				    (scheme-object coords))
#<<END
  C_word result;
  int * vcoords;

  if ((C_vectorp(coords)) && MPI_comm_p (comm))
  {
     vcoords = C_c_s32vector(coords);
     MPI_Cart_coords(Comm_val(comm), rank, ndims, vcoords);
     result = coords;
  }
  else
  {
     result = C_SCHEME_FALSE;
  }

  C_return (result);
END
))

(define (MPI:cart-coords comm rank)
  (let ((ndims (MPI_cart_dim comm)))
    (MPI_cart_coords comm rank ndims (make-s32vector ndims))))

