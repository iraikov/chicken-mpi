;;
;; Chicken MPI interface. Based on the Caml/MPI interface by Xavier
;; Leroy.
;;
;; Copyright 2007-2026 Ivan Raikov.
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

;; Initialization and finalization

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

(define-mpi-checked (MPI:spawn command arguments maxprocs locations root comm)
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

     #if MPI_VERSION >= 3
       MPI_Comm_create_errhandler((MPI_Comm_errhandler_function *)chicken_MPI_error_handler, &hdlr);
       MPI_Comm_set_errhandler(MPI_COMM_WORLD, hdlr);
     #else
       MPI_Errhandler_create((MPI_Handler_function *)chicken_MPI_error_handler, &hdlr);
       MPI_Errhandler_set(MPI_COMM_WORLD, hdlr);
     #endif

     MPI_Add_error_class(&chicken_MPI_errclass);
     MPI_Add_error_string(chicken_MPI_errclass, "invalid MPI struct datatype size");
  }

  C_return (C_SCHEME_UNDEFINED);
EOF
))


(define-mpi-checked MPI:finalize
  (foreign-primitive scheme-object ()
#<<EOF
  MPI_Finalize();
  C_return (C_SCHEME_UNDEFINED);
EOF
))

;; MPI_Initialized/MPI_Finalized are specifically documented as callable at
;; any time, including before MPI_Init.

(define MPI:initialized?
  (foreign-lambda* bool () "int flag; MPI_Initialized(&flag); C_return(flag);"))

(define MPI:finalized?
  (foreign-lambda* bool () "int flag; MPI_Finalized(&flag); C_return(flag);"))

;; MPI_Abort is a last resort, non-collective and is itself part of the
;; exit-handler's error path; it shouldn't be able to raise a new exception
;; out of that path.

(define MPI_abort_raw
  (foreign-lambda* void ((mpi-comm comm) (int code))
    "MPI_Abort(Comm_val(comm), code);"))

(define (MPI:abort code #!optional (comm (MPI:get-comm-world)))
  (MPI_abort_raw comm code))

;; Automatic finalize-or-abort on process exit, installed once by MPI:init
;; (see below), for consistency with mpi4py's atexit-registered finalize.
;; A forgotten MPI:finalize or an unhandled Scheme exception would otherwise
;; either leave the job improperly torn down or, hang peer ranks that
;; are still waiting on the rank that just died. MPI_Finalize is collective
;; and would cause a deadlock in that situation, so an error exit calls
;; MPI_Abort instead, which is safe for a single rank to call.
;;
;; Guarded by MPI:initialized?/MPI:finalized? so this is not invoked for any
;; program that never calls MPI:init, and safe if the program already
;; called MPI:finalize itself. Wrapped in handle-exceptions so a problem
;; here can't prevent the rest of CHICKEN's normal exit sequence from
;; running.

(define mpi-exit-code 0)
(define mpi-exit-handler-installed? #f)

(define (mpi-install-exit-handler!)
  (unless mpi-exit-handler-installed?
    (set! mpi-exit-handler-installed? #t)
    (exit-handler
     (let ((orig (exit-handler)))
       (lambda (#!optional (code 0))
         (set! mpi-exit-code code)
         (orig code))))
    (on-exit
     (lambda ()
       (handle-exceptions exn
           #f
         (when (and (MPI:initialized?) (not (MPI:finalized?)))
           (if (zero? mpi-exit-code)
               (MPI:finalize)
               (MPI:abort mpi-exit-code))))))))

(define-mpi-checked (MPI:init . args)
  (MPI_init args)
  (mpi-install-exit-handler!))

(define-mpi-checked MPI:wtime
  (foreign-primitive scheme-object ()
#<<EOF
  C_word result;
  C_word *ptr;

  ptr = C_alloc (C_SIZEOF_FLONUM);

  result = C_number(&ptr, MPI_Wtime());

  C_return (result);
EOF
))
