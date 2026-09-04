
;; Error handling
;;
;; Two kinds of C-side errors get handled by the same exception path:
;;
;;  - genuine MPI runtime failures, caught automatically by the error
;;    handler installed on MPI_COMM_WORLD in MPI_init (init.scm);
;;  - conditions this binding detects itself (e.g. an invalid struct field
;;    count in MPI:make-type-struct), reported by explicitly invoking that
;;    same handler via MPI_Comm_call_errhandler.
;;
;; Argument validation happens separately, entirely in Scheme, via the
;; mpi-comm/mpi-group/mpi-datatype foreign types below. CHICKEN runs
;; their converters before the foreign call, so a bad argument raises
;; immediately and safely, without ever reaching C.

#>

static int chicken_MPI_error_pending = 0;
static int chicken_MPI_error_code = MPI_SUCCESS;
static char chicken_MPI_error_msg[MPI_MAX_ERROR_STRING + 1];

/* Error class for conditions this binding detects itself, with no
   matching native MPI error code. Replaced with a real registered class
   by MPI_Add_error_class in MPI_init; the initializer here is just a
   placeholder in case MPI_init is never called. */
static int chicken_MPI_errclass = MPI_ERR_TYPE;

static void chicken_MPI_error_handler(MPI_Comm *comm, int *errcode, ...)
{
  int resultlen;

  chicken_MPI_error_pending = 1;
  chicken_MPI_error_code = *errcode;

  MPI_Error_string(*errcode, chicken_MPI_error_msg, &resultlen);
  if (resultlen < 0) resultlen = 0;
  if (resultlen > MPI_MAX_ERROR_STRING) resultlen = MPI_MAX_ERROR_STRING;
  chicken_MPI_error_msg[resultlen] = 0;
}

<#

(define mpi-error-pending?
  (foreign-lambda* bool () "C_return(chicken_MPI_error_pending);"))

(define mpi-error-code
  (foreign-lambda* int () "C_return(chicken_MPI_error_code);"))

(define mpi-error-message
  (foreign-lambda* c-string () "C_return(chicken_MPI_error_msg);"))

(define mpi-clear-error!
  (foreign-lambda* void () "chicken_MPI_error_pending = 0;"))

(define (mpi-check-error! loc)
  (when (mpi-error-pending?)
    (let ((code (mpi-error-code))
          (msg  (mpi-error-message)))
      (mpi-clear-error!)
      (error loc msg code))))

;; Argument validation for comm/group/datatype parameters: the converter
;; runs in Scheme before the foreign call is made, so an invalid argument
;; raises immediately via the ordinary (still fully working) `error`.

(define-foreign-type mpi-comm scheme-object
  (lambda (x) (if (MPI:comm? x) x (error 'mpi-comm "invalid MPI communicator object" x))))

(define-foreign-type mpi-group scheme-object
  (lambda (x) (if (MPI:group? x) x (error 'mpi-group "invalid MPI group object" x))))

(define-foreign-type mpi-datatype scheme-object
  (lambda (x) (if (MPI:datatype? x) x (error 'mpi-datatype "invalid MPI datatype object" x))))

;; Stands in for `define` at each exported MPI: binding, so every exported
;; procedure checks for (and raises) a pending MPI-level error right after
;; it returns. Handles both definition shapes used throughout this egg.

(define-syntax define-mpi-checked
  (syntax-rules ()
    ((_ (name . args) body ...)
     (define (name . args)
       (call-with-values
        (lambda () (begin body ...))
        (lambda results
          (mpi-check-error! 'name)
          (apply values results)))))
    ((_ name value)
     (define name
       (let ((proc value))
         (lambda args
           (call-with-values
            (lambda () (apply proc args))
            (lambda results
              (mpi-check-error! 'name)
              (apply values results)))))))))
