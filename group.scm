
;;
;; Chicken MPI interface. Based on the Caml/MPI interface by Xavier
;; Leroy.
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

;; Handling of communication groups

; Include into generated code, but don't parse:
#>

static C_word MPI_group_p(C_word obj) 
{
  if (C_immediatep(obj)) {
    return C_SCHEME_FALSE;
  } else if (C_block_header(obj) == MPI_GROUP_TAG) 
  {
    return C_SCHEME_TRUE;
  } else {
    return C_SCHEME_FALSE;
  }
}

static C_word MPI_check_group (C_word obj) 
{
  if (C_immediatep(obj)) 
  {
   chicken_MPI_exception (MPI_ERR_COMM, "MPI_check_group",
                          32, "invalid MPI group object");
  } else if (C_block_header(obj) == MPI_GROUP_TAG) 
  {
    return C_SCHEME_UNDEFINED;
  } else {
          chicken_MPI_exception (MPI_ERR_COMM, "MPI_check_group",
                                 32, "invalid MPI group object");
  }
}

<#

(define MPI:group? (foreign-lambda scheme-object "MPI_group_p" scheme-object))

(define MPI_alloc_group 
    (foreign-primitive scheme-object ((nonnull-c-pointer group))
#<<END

   C_word result;
   chicken_MPI_group_t newg;

   newg.tag = MPI_GROUP_TAG;
   newg.group_data = group;
   result = (C_word)&newg;
   
   C_return (result);
END
))

(define MPI:group-size
    (foreign-primitive scheme-object ((scheme-object x))
#<<END
   C_word *ptr;
   C_word result;
   int size;

   if (MPI_group_p (x))
     {
       MPI_Group_size (Group_val(x), &size);
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


(define MPI:group-rank
    (foreign-primitive scheme-object ((scheme-object x))
#<<END
   C_word result;
   C_word *ptr;
   int rank;

   if (MPI_group_p (x))
     {
       MPI_Group_rank (Group_val(x), &rank);
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

(define MPI_group_translate_ranks
    (foreign-primitive scheme-object ((scheme-object group1)
				      (scheme-object group2)
				      (integer nranks)
				      (scheme-object ranks)
				      (scheme-object ranks1)
				      (scheme-object ranks2))
#<<END
  int i; int *vranks, *vranks1, *vranks2;
  C_word result;

   C_i_check_vector (ranks);
   C_i_check_vector (ranks1);
   C_i_check_vector (ranks2);

   if ((MPI_group_p (group1)) && (MPI_group_p (group2)))
     {
        vranks  = C_c_s32vector (ranks);
        vranks1 = C_c_s32vector (ranks1);
        vranks2 = C_c_s32vector (ranks2);
        for (i = 0; i < nranks; i++) 
            vranks1[i] = vranks[i];
        MPI_Group_translate_ranks(Group_val(group1), nranks, vranks1,
                                  Group_val(group2), vranks2);
        result = ranks2;
     }
     else 
     {
       result = C_SCHEME_FALSE;
     };

  C_return(result);
END
))


(define (MPI:group-translate-ranks group1 ranks group2)
  (let ((nranks (s32vector-length ranks)))
    (MPI_group_translate_ranks group1 group2 nranks 
			       ranks (make-s32vector nranks)
			       (make-s32vector nranks))))

(define MPI_comm_group
    (foreign-primitive nonnull-c-pointer ((scheme-object comm))
#<<END
  C_word result;
  MPI_Group group;

  MPI_check_comm (comm);
  if ((MPI_comm_p (comm)))
  {
     MPI_Comm_group(Comm_val(comm), &group);
     result = (C_word)group;
  }
  else
  { 
     result = (C_word)NULL;
  }

  C_return (result);
END
))

(define (MPI:comm-group comm)
  (MPI_alloc_group (MPI_comm_group comm)))


(define MPI_group_union
    (foreign-primitive nonnull-c-pointer ((scheme-object group1)
					  (scheme-object group2))
#<<END
  C_word result;
  MPI_Group group;

  MPI_check_group(group1);
  MPI_check_group(group2);
  if ((MPI_group_p (group1)) && (MPI_group_p (group2)))
  {
     MPI_Group_union(Group_val(group1), Group_val(group2), &group);
     result = (C_word)group;
  }
  else
  { 
     result = (C_word)NULL;
  }

  C_return (result);
END
))

(define (MPI:group-union group1 group2)
  (MPI_alloc_group (MPI_group_union group1 group2)))


(define MPI_group_difference
    (foreign-primitive nonnull-c-pointer ((scheme-object group1)
					  (scheme-object group2))
#<<END
  C_word result;
  MPI_Group group;

  MPI_check_group(group1);
  MPI_check_group(group2);
  if ((MPI_group_p (group1)) && (MPI_group_p (group2)))
  {
     MPI_Group_difference(Group_val(group1), Group_val(group2), &group);
     result = (C_word)group;
  }
  else
  { 
     result = (C_word)NULL;
  }

  C_return (result);
END
))

(define (MPI:group-difference group1 group2)
  (MPI_alloc_group (MPI_group_difference group1 group2)))


(define MPI_group_intersection
    (foreign-primitive nonnull-c-pointer ((scheme-object group1)
					  (scheme-object group2))
#<<END
  C_word result;
  MPI_Group group;

  MPI_check_group(group1);
  MPI_check_group(group2);
  if ((MPI_group_p (group1)) && (MPI_group_p (group2)))
  {
     MPI_Group_intersection(Group_val(group1), Group_val(group2), &group);
     result = (C_word)group;
  }
  else
  { 
     result = (C_word)NULL;
  }

  C_return (result);
END
))

(define (MPI:group-intersection group1 group2)
  (MPI_alloc_group (MPI_group_intersection group1 group2)))


(define MPI_group_incl
    (foreign-primitive nonnull-c-pointer ((scheme-object group)
					  (integer nranks)
					  (scheme-object ranks))
#<<END
  C_word result;
  int * vranks;
  MPI_Group newg;

  C_i_check_vector (ranks);
  MPI_check_group(group);

  if ((MPI_group_p (group)))
  {
     vranks  = C_c_s32vector (ranks); 
     MPI_Group_incl(Group_val(group), nranks, vranks, &newg);
     result = (C_word)newg;
  }
  else
  { 
     result = (C_word)NULL;
  }

  C_return (result);
END
))

(define (MPI:group-incl group ranks)
  (MPI_alloc_group (MPI_group_incl group (s32vector-length ranks) ranks)))


(define MPI_group_excl
    (foreign-primitive nonnull-c-pointer ((scheme-object group)
					  (integer nranks)
					  (scheme-object ranks))
#<<END
  C_word result;
  int * vranks;
  MPI_Group newg;

  C_i_check_vector (ranks);
  MPI_check_group(group);

  if ((MPI_group_p (group)))
  {
     vranks  = C_c_s32vector (ranks); 
     MPI_Group_incl(Group_val(group), nranks, vranks, &newg);
     result = (C_word)newg;
  }
  else
  { 
     result = (C_word)NULL;
  }

  C_return (result);
END
))

(define (MPI:group-excl group ranks)
  (MPI_alloc_group (MPI_group_excl group (s32vector-length ranks) ranks)))


; Include into generated code, but don't parse:
#>
static void MPI_extract_ranges (C_word ranges,
                                  /*out*/ int * num,
                                  /*out*/ int * exranges)
{
   int i, nranges; C_word range;
   C_i_check_vector (ranges);   

   nranges = C_unfix(C_i_vector_length (ranges));

   for (i = 0; i < nranges; i++) 
   {
     range = C_i_vector_ref (ranges, C_fix(i));
     exranges[(3*i)+0] = C_num_to_int(C_u_i_s32vector_ref(range, C_fix(0)));
     exranges[(3*i)+1] = C_num_to_int(C_u_i_s32vector_ref(range, C_fix(1)));
     exranges[(3*i)+2] = C_num_to_int(C_u_i_s32vector_ref(range, C_fix(2)));
   }
   
}
<#


(define MPI_group_range_incl
    (foreign-primitive nonnull-c-pointer ((scheme-object group)
					  (scheme-object ranges)
					  (s32vector exranges))
#<<END
  C_word result;
  MPI_Group newg;
  int num;

  C_i_check_vector (ranges);
  MPI_check_group (group);

  if ((MPI_group_p (group)))
  {
     MPI_extract_ranges (ranges, &num, exranges);
     MPI_Group_range_incl(Group_val(group), num, exranges, &newg);
     result = (C_word)newg;
  }
  else
  { 
     result = (C_word)NULL;
  }

  C_return (result);
END
))


(define (MPI:group-range-incl group ranges)
  (let ((len (vector-length ranges)))
    (MPI_alloc_group (MPI_group_range_incl group ranges (make-s32vector (* 3 len))))))




(define MPI_group_range_excl
    (foreign-primitive nonnull-c-pointer ((scheme-object group)
					  (scheme-object ranges)
					  (s32vector exranges))
#<<END
  C_word result;
  MPI_Group newg;
  int num;

  C_i_check_vector (ranges);
  MPI_check_group (group);

  if ((MPI_group_p (group)))
  {
     MPI_extract_ranges (ranges, &num, exranges);
     MPI_Group_range_excl(Group_val(group), num, exranges, &newg);
     result = (C_word)newg;
  }
  else
  { 
     result = (C_word)NULL;
  }

  C_return (result);
END
))


(define (MPI:group-range-excl group ranges)
  (let ((len (vector-length ranges)))
    (MPI_alloc_group (MPI_group_range_excl group ranges (make-s32vector (* 3 len))))))

