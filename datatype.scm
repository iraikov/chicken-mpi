
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


(define MPI:datatype? (foreign-lambda scheme-object "MPI_datatype_p" scheme-object))

(define MPI_datatype_finalizer 
    (foreign-safe-lambda* void ((scheme-object ty))
#<<END
   MPI_Datatype *x;
   MPI_check_datatype (ty);

   x = Datatype_val (ty);

   MPI_Type_free (*x);
END
))

(define MPI_alloc_datatype 
    (foreign-primitive scheme-object ((scheme-object finalizer))
#<<END

   C_word result;
   chicken_MPI_datatype_t newdatatype;

   newdatatype.tag = MPI_DATATYPE_TAG;
   newdatatype.datatype_data = 0;
   result = (C_word)&newdatatype;

   //C_do_register_finalizer(result, finalizer);
   
   C_return (result);
END
))

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

  status = MPI_Type_Contiguous(count, *(Datatype_val(ty)), Datatype_val(newty));

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
  

