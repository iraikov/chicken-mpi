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

;; Error handling, initialization and finalization

;; The following three functions are borrowed from the
;; Chicken-specific parts of SWIG
#>
static void chicken_Panic (C_char *) C_noret;
static void chicken_Panic (C_char *msg)
{
  C_word *a = C_alloc (C_SIZEOF_STRING (strlen (msg)));
  C_word scmmsg = C_string2 (&a, msg);
  C_halt (scmmsg);
  exit (5); /* should never get here */
}

static void chicken_ThrowException(C_word value) C_noret;
static void chicken_ThrowException(C_word value)
{
  char *aborthook = C_text("\003sysabort");

  C_word *a = C_alloc(C_SIZEOF_STRING(strlen(aborthook)));
  C_word abort = C_intern2(&a, aborthook);

  abort = C_block_item(abort, 0);
  if (C_immediatep(abort))
    chicken_Panic(C_text("`##sys#abort' is not defined"));

#if defined(C_BINARY_VERSION) && (C_BINARY_VERSION >= 8)
  C_word rval[3] = { abort, C_SCHEME_UNDEFINED, value };
  C_do_apply(3, rval);
#else
  C_save(value);
  C_do_apply(1, abort, C_SCHEME_UNDEFINED);
#endif
}

void chicken_MPI_exception (int code, int msglen, const char *msg) 
{
  C_word *a;
  C_word scmmsg;
  C_word list;

  a = C_alloc (C_SIZEOF_STRING (msglen) + C_SIZEOF_LIST(2));
  scmmsg = C_string2 (&a, (char *) msg);
  list = C_list(&a, 2, C_fix(code), scmmsg);
  chicken_ThrowException(list);
}

static void MPI_error_handler(MPI_Comm * comm, int * errcode, ...)
{
  char errmsg[MPI_MAX_ERROR_STRING + 1];
  int resultlen;

  MPI_Error_string(*errcode, errmsg, &resultlen);

  chicken_MPI_exception (*errcode, resultlen, errmsg);
}
<#

(define MPI_spawn 
  (foreign-primitive nonnull-c-pointer ((c-string command) (scheme-object arguments) (integer maxprocs)
					(scheme-object locations) (integer root) (scheme-object comm)
					(s32vector errcodes))
#<<EOF
  int locc, argc, i, argvsz, slen, locvsz;
  char ** argv; char *s, *skey, *sval, **locv;
  MPI_Errhandler hdlr; 
  MPI_Info info; 
  MPI_Comm intercomm;
  C_word x, tail, key, val;
  C_word result;

  C_i_check_list (arguments);
  if (C_i_listp (arguments))
  {
     argc = C_num_to_int(C_i_length(arguments));
     argvsz = (argc + 1) * sizeof(char *);
     if ((argv = malloc(argvsz)) != NULL)
     {
       if (argc > 0)
       {
         tail = arguments;
         for (i = 0; i < argc; i++) 
         {
	   x = C_u_i_car (tail);
	   tail = C_u_i_cdr (tail);
           C_i_check_string (x);
	   slen = C_num_to_int(C_i_string_length (x));
	   if (( s = malloc (slen+1)) != NULL)
	   {
	      memcpy (s, C_c_string (x), slen);
	      s[slen] = 0;
	      argv[i] = s;
           } else
           { 
             argv[i] = NULL;
           }
           
        }
       } else
       {
         i = 0;
       }

       argv[i] = NULL;

       MPI_Info_create(&info);

       C_i_check_list (locations);
       if (C_i_listp (locations))
       {
	  locc = C_num_to_int(C_i_length(locations));
	  locvsz = ((2*locc) + 1) * sizeof(char *);
          locv = malloc(locvsz);

          if ((locc > 0) && (locv != NULL))
          {
            tail = locations;
            for (i = 0; i < locc; i+=2) 
            {
 	       x = C_u_i_car (tail);
	       tail = C_u_i_cdr (tail);
               C_i_check_pair (x);
	       key = C_u_i_car (x);
	       val = C_u_i_cadr (x);
	       skey = NULL;
	       sval = NULL;
	       C_i_check_string (key);
	       slen = C_num_to_int(C_i_string_length (key));
	       if (( skey = malloc (slen+1)) != NULL)
	       {
		  memcpy (skey, C_c_string (key), slen);
		  skey[slen] = 0;
	       }
	       C_i_check_string (val);
	       slen = C_num_to_int(C_i_string_length (val));
	       if (( sval = malloc (slen+1)) != NULL)
	       {
		  memcpy (sval, C_c_string (val), slen);
		  sval[slen] = 0;
	       }
               if ((skey != NULL) && (sval != NULL))
	       {
		  MPI_Info_set(info, skey, sval);
                  locv[i] = skey;
                  locv[i+1] = sval;
	       }

            }
            locv[i] = NULL;
          }
       }

       MPI_Comm_spawn(command, argv, maxprocs, info, root, Comm_val(comm),
		      &intercomm, errcodes);

       MPI_Info_free (&info);

       for (i = 0; i < locc; i+=2)
       {  
          skey = locv[i];
          sval = locv[i+1];
	  if (skey != NULL)
	  {
	     free (skey);
          }
	  if (sval != NULL)
	  {
	     free (sval);
          }
          locv[i] = NULL;
          locv[i+1] = NULL;
       }
       free (locv);

       for (i = 0; i < argc; i++)
       {  
          s = argv[i];
	  if (s != NULL)
	  {
	     free (s);
          }
          argv[i] = NULL;
       }
       free (argv);
     }
  }

  result = (C_word)intercomm;
  C_return (result);
EOF
))

(define (MPI:spawn command arguments maxprocs locations root comm)
  (and (integer? maxprocs) (positive? maxprocs) 
       (let  ((errcodes (make-s32vector maxprocs 0))
	      (locations (map (lambda (p) (list (->string (car p)) (->string (cadr p)))) locations)))
	 (let  ((intercomm (MPI_spawn command arguments maxprocs locations root comm errcodes)))
	   (list intercomm errcodes)))))
    


(define MPI_init 
    (foreign-primitive scheme-object ((scheme-object arguments))
#<<EOF
  int argc, i, argvsz, slen;
  char ** argv; char *s;
  MPI_Errhandler hdlr;
  C_word x, tail;
  MPI_Datatype newty;
  int status;

  C_i_check_list (arguments);
  if (C_i_listp (arguments))
  {
     argc = C_num_to_int(C_i_length(arguments));
     argvsz = (argc + 1) * sizeof(char *);
     if ((argv = malloc(argvsz)) != NULL)
     {
       if (argc > 0)
       {
         tail = arguments;
         for (i = 0; i < argc; i++) 
         {
	   x = C_u_i_car (tail);
	   tail = C_u_i_cdr (tail);
           C_i_check_string (x);
	   slen = C_num_to_int(C_i_string_length (x));
	   if (( s = malloc (slen+1)) != NULL)
	   {
	      memcpy (s, C_c_string (x), slen);
	      s[slen] = 0;
	      argv[i] = s;
           } else
           { 
             argv[i] = NULL;
           }
           
        }
       } else
       {
         i = 0;
       }

       argv[i] = NULL;
        
       MPI_Init(&argc, &argv);

       for (i = 0; i < argc; i++)
       {  
          s = argv[i];
	  if (s != NULL)
	  {
	     free (s);
          }
          argv[i] = NULL;
       }
       free (argv);
     }
     MPI_Errhandler_create((MPI_Handler_function *)MPI_error_handler, &hdlr);
     MPI_Errhandler_set(MPI_COMM_WORLD, hdlr);
  }

  C_return (C_SCHEME_UNDEFINED);
EOF
))


(define (MPI:init . args)
  (MPI_init args))
    

(define MPI:finalize 
  (foreign-primitive scheme-object ()
#<<EOF
  MPI_Finalize();
  C_return (C_SCHEME_UNDEFINED);
EOF
))

(define MPI:wtime 
  (foreign-primitive scheme-object ()
#<<EOF
  C_word result;
  C_word *ptr;

  ptr = C_alloc (C_SIZEOF_FLONUM);

  result = C_number(&ptr, MPI_Wtime());

  C_return (result);
EOF
))
