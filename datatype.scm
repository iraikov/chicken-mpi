
;;
;; Chicken MPI interface.
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


;; Derived datatypes


(define-record-type mpi-data
  (make-mpi-data ty count buffer)
  mpi-data?
  (ty      mpi-data-ty)
  (count   mpi-data-count)
  (buffer  mpi-data-buffer))


;; Handling of datatypes 

; Include into generated code, but don't parse:
#>

static C_word MPI_datatype_p(C_word obj) 
{
  if (C_immediatep(obj)) {
    return C_SCHEME_FALSE;
  } else if (C_block_header(obj) == MPI_DATATYPE_TAG) 
  {
    return C_SCHEME_TRUE;
  } else {
    return C_SCHEME_FALSE;
  }
}


static C_word MPI_check_datatype (C_word obj) 
{
  if (C_immediatep(obj)) 
  {
    chicken_MPI_exception (MPI_ERR_COMM, 32, "invalid MPI datatype object");
  } else if (C_block_header(obj) == MPI_DATATYPE_TAG) 
  {
    return C_SCHEME_UNDEFINED;
  } else {
    chicken_MPI_exception (MPI_ERR_COMM, 32, "invalid MPI datatype object");
  }
}

<#


(define MPI_type_null
    (foreign-primitive nonnull-c-pointer ()
#<<END
   C_word result;

   result = (C_word)MPI_DATATYPE_NULL;

   C_return (result);
END
))

(define MPI_type_char
    (foreign-primitive nonnull-c-pointer ()
#<<END
   C_word result;

   result = (C_word)MPI_CHAR;

   C_return (result);
END
))

(define MPI_type_int
    (foreign-primitive nonnull-c-pointer ()
#<<END
   C_word result;

   printf ("type-int = %p\n", MPI_LONG);
   result = (C_word)MPI_LONG;

   C_return (result);
END
))

(define MPI_type_fixnum
    (foreign-primitive nonnull-c-pointer ()
#<<END
   C_word result;

   result = (C_word)MPI_INT;

   C_return (result);
END
))

(define MPI_type_flonum
    (foreign-primitive nonnull-c-pointer ()
#<<END
   C_word result;

   result = (C_word)MPI_DOUBLE;

   C_return (result);
END
))


(define MPI_type_u8
    (foreign-primitive nonnull-c-pointer ()
#<<END
   C_word result;

   result = (C_word)MPI_UNSIGNED_CHAR;

   C_return (result);
END
))


(define MPI_type_s8
    (foreign-primitive nonnull-c-pointer ()
#<<END
   C_word result;

   result = (C_word)MPI_CHAR;

   C_return (result);
END
))


(define MPI_type_u16
    (foreign-primitive nonnull-c-pointer ()
#<<END
   C_word result;

   result = (C_word)MPI_UNSIGNED_SHORT;

   C_return (result);
END
))


(define MPI_type_s16
    (foreign-primitive nonnull-c-pointer ()
#<<END
   C_word result;

   result = (C_word)MPI_SHORT;

   C_return (result);
END
))


(define MPI_type_u32
    (foreign-primitive nonnull-c-pointer ()
#<<END
   C_word result;

   result = (C_word)MPI_UNSIGNED;

   C_return (result);
END
))


(define MPI_type_s32
    (foreign-primitive nonnull-c-pointer ()
#<<END
   C_word result;

   result = (C_word)MPI_INT;

   C_return (result);
END
))


(define MPI_type_f32
    (foreign-primitive nonnull-c-pointer ()
#<<END
   C_word result;

   result = (C_word)MPI_FLOAT;

   C_return (result);
END
))


(define MPI_type_f64
    (foreign-primitive nonnull-c-pointer ()
#<<END
   C_word result;

   result = (C_word)MPI_DOUBLE;

   C_return (result);
END
))


(define MPI_type_byte
    (foreign-primitive nonnull-c-pointer ()
#<<END
   C_word result;

   result = (C_word)MPI_BYTE;

   C_return (result);
END
))

(define MPI:datatype? (foreign-lambda scheme-object "MPI_datatype_p" scheme-object))


(define MPI_datatype_finalizer 
    (foreign-safe-lambda* void ((scheme-object ty))
#<<END
   MPI_Datatype *x;
   MPI_check_datatype (ty);

   x = Datatype_val (ty);

   MPI_Type_free (x);
END
))

(define MPI_alloc_datatype 
    (foreign-primitive scheme-object ((scheme-object finalizer))
#<<END

   C_word result;
   chicken_MPI_datatype_t newdatatype;
   MPI_Datatype *ty = malloc(sizeof(MPI_Datatype));

   printf ("alloc: ty = %p\n", ty);

   newdatatype.tag = MPI_DATATYPE_TAG;
   newdatatype.datatype_data = ty;
   result = (C_word)&newdatatype;

   //C_do_register_finalizer(result, finalizer);
   
   C_return (result);
END
))


(define MPI_copy_datatype 
    (foreign-primitive scheme-object ((nonnull-c-pointer cty) (scheme-object finalizer))
#<<END

   C_word result;
   chicken_MPI_datatype_t newdatatype;

   newdatatype.tag = MPI_DATATYPE_TAG;
   newdatatype.datatype_data = cty;
   result = (C_word)&newdatatype;

   //C_do_register_finalizer(result, finalizer);
   
   C_return (result);
END
))


(define MPI:datatype? (foreign-lambda scheme-object "MPI_datatype_p" scheme-object))


;; Commits a datatype
(define MPI:commit 
    (foreign-lambda* void ((scheme-object ty))
#<<EOF
  int status;

  MPI_check_datatype(ty);

  status = MPI_Commit(Datatype_val(ty));

  if (status != MPI_SUCCESS) 
  {
    chicken_MPI_exception (MPI_ERR_TYPE, 27, "invalid MPI datatype commit");
  }
  
EOF
))


(define MPI_type_contiguous 
    (foreign-lambda* void ((int count)
                           (scheme-object ty)
                           (scheme-object newty))
#<<EOF
  int status;

  MPI_check_datatype(ty);
  MPI_check_datatype(newty);

  printf ("ty = %p newty = %p\n", Datatype_val(ty), Datatype_val(newty));
  status = MPI_Type_Contiguous(count, *((MPI_Datatype *)(Datatype_val(ty))), (MPI_Datatype *)(Datatype_val(newty)));
  printf ("after contig\n");

  if (status != MPI_SUCCESS) 
  {
    chicken_MPI_exception (MPI_ERR_TYPE, 31, "invalid MPI datatype contiguous");
  }
  
EOF
))


(define MPI:type-extent 
    (foreign-lambda* int ((scheme-object ty))
#<<EOF
  int status;
  int count;

  MPI_check_datatype(ty);

  status = MPI_Type_extent(Datatype_val(ty), &count);

  if (status != MPI_SUCCESS) 
  {
    chicken_MPI_exception (MPI_ERR_TYPE, 20, "invalid MPI datatype");
  }

  return count;
  
EOF
))


(define (MPI:type-contiguous count ty)
  (let ((newty (MPI_alloc_datatype MPI_datatype_finalizer)))
    (MPI_type_contiguous count ty newty)
    newty))

  
(define (MPI:type-fixnum)
  (let ((newty (MPI_copy_datatype (MPI_type_fixnum) MPI_datatype_finalizer)))
    newty))

  
(define (MPI:type-int)
  (let ((newty (MPI_copy_datatype (MPI_type_int) MPI_datatype_finalizer)))
    newty))

  
(define (MPI:type-char)
  (let ((newty (MPI_copy_datatype (MPI_type_char) MPI_datatype_finalizer)))
    newty))

  
(define (MPI:type-flonum)
  (let ((newty (MPI_copy_datatype (MPI_type_flonum) MPI_datatype_finalizer)))
    newty))

  
(define (MPI:type-s8)
  (let ((newty (MPI_copy_datatype (MPI_type_s8) MPI_datatype_finalizer)))
    newty))

  
(define (MPI:type-u8)
  (let ((newty (MPI_copy_datatype (MPI_type_u8) MPI_datatype_finalizer)))
    newty))

  
(define (MPI:type-s16)
  (let ((newty (MPI_copy_datatype (MPI_type_s16) MPI_datatype_finalizer)))
    newty))

  
(define (MPI:type-u16)
  (let ((newty (MPI_copy_datatype (MPI_type_u16) MPI_datatype_finalizer)))
    newty))

  
(define (MPI:type-s32)
  (let ((newty (MPI_copy_datatype (MPI_type_s32) MPI_datatype_finalizer)))
    newty))

  
(define (MPI:type-u32)
  (let ((newty (MPI_copy_datatype (MPI_type_u32) MPI_datatype_finalizer)))
    newty))

  
(define (MPI:type-f32)
  (let ((newty (MPI_copy_datatype (MPI_type_f32) MPI_datatype_finalizer)))
    newty))

  
(define (MPI:type-f64)
  (let ((newty (MPI_copy_datatype (MPI_type_f64) MPI_datatype_finalizer)))
    newty))
  

(define (MPI:type-byte)
  (let ((newty (MPI_copy_datatype (MPI_type_byte) MPI_datatype_finalizer)))
    newty))

  
