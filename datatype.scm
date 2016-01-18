
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
   MPI_Datatype x;
   MPI_check_datatype (ty);

   x = Datatype_val (ty);

   MPI_Type_free (&x);
END
))

(define MPI_alloc_datatype 
    (foreign-primitive scheme-object ((nonnull-c-pointer cty)
                                      (scheme-object finalizer))
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


(define MPI:type-char 
  (MPI_alloc_datatype (foreign-value "MPI_CHAR" nonnull-c-pointer) 
                      MPI_datatype_finalizer))
  

(define MPI:type-int 
  (MPI_alloc_datatype (foreign-value "MPI_LONG" nonnull-c-pointer) 
                      MPI_datatype_finalizer))
  


(define MPI:make-type-struct 
    (foreign-primitive scheme-object ((int fieldcount)
                                      (scheme-object blocklens)
                                      (scheme-object displs)
                                      (scheme-object fieldtys))
#<<EOF
  int i, status;
  int *array_of_blocklens;
  MPI_Aint *array_of_displs;
  MPI_Datatype *array_of_types, newtype;
  chicken_MPI_datatype_t newdatatype;
  C_word result, x, tail;

  C_i_check_list (blocklens);
  C_i_check_list (displs);
  C_i_check_list (fieldtys);

  if (!(fieldcount > 0))
  {
    chicken_MPI_exception (MPI_ERR_TYPE, 32, "invalid MPI struct datatype size");
  }

  array_of_blocklens = malloc(fieldcount*sizeof(int));
  array_of_displs = malloc(fieldcount*sizeof(MPI_Aint));
  array_of_types = malloc(fieldcount*sizeof(MPI_Datatype));

  tail = blocklens;
  for (i=0; i<fieldcount; i++)
  {
     x = C_u_i_car (tail);
     tail = C_u_i_cdr (tail);
     array_of_blocklens[i] = C_num_to_int(x);
  }
  tail = displs;
  for (i=0; i<fieldcount; i++)
  {
     x = C_u_i_car (tail);
     tail = C_u_i_cdr (tail);
     array_of_displs[i] = C_num_to_int(x);
  }
  tail = fieldtys;
  for (i=0; i<fieldcount; i++)
  {
     x = C_u_i_car (tail);
     tail = C_u_i_cdr (tail);
     array_of_types[i] = Datatype_val(x);
  }

  status = MPI_Type_create_struct(fieldcount, 
                                  array_of_blocklens,
                                  array_of_displs,
                                  array_of_types,
                                  &newtype);

  free(array_of_blocklens);
  free(array_of_displs);
  free(array_of_types);

  if (status != MPI_SUCCESS) 
  {
    chicken_MPI_exception (MPI_ERR_TYPE, 26, "invalid MPI struct datatype");
  }

  status = MPI_Type_commit(&newtype);

  if (status != MPI_SUCCESS) 
  {
    chicken_MPI_exception (MPI_ERR_TYPE, 27, "invalid MPI datatype commit");
  }

  newdatatype.tag = MPI_DATATYPE_TAG;
  newdatatype.datatype_data = (void *)newtype;
  result = (C_word)&newdatatype;

  C_return(result);
EOF
))


(define MPI:type-extent 
    (foreign-safe-lambda* int ((scheme-object ty))
#<<EOF
  int status, result;
  MPI_Aint lb, extent;

  MPI_check_datatype(ty);

  status = MPI_Type_get_extent(Datatype_val(ty), &lb, &extent);

  if (status != MPI_SUCCESS) 
  {
    chicken_MPI_exception (MPI_ERR_TYPE, 20, "invalid MPI datatype");
  }

  result = (int)extent;
  C_return(result);
  
EOF
))



(define MPI:type-size 
    (foreign-safe-lambda* int ((scheme-object ty))
#<<EOF
  int status, result;
  int size;

  MPI_check_datatype(ty);

  status = MPI_Type_size(Datatype_val(ty), &size);

  if (status != MPI_SUCCESS) 
  {
    chicken_MPI_exception (MPI_ERR_TYPE, 20, "invalid MPI datatype");
  }

  result = (int)size;
  C_return(result);
  
EOF
))


