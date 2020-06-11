
(import (chicken base) (chicken format) (chicken process) (chicken process-context) srfi-13 compile-file)
(define args (command-line-arguments))

(define (mpi-try-compile header ldflags cppflags)
  (print "header: " header " cppflags: " cppflags " ldflags: " ldflags)
  (and (try-compile 
	(string-append header "\n" 
			"int main(int argc, char **argv) { MPI_Init(&argc, &argv); return 0; }\n")
	ldflags: ldflags
	cflags: cppflags
        verbose: #t
	)
       (cons ldflags cppflags)))

(define-syntax mpi-test 
  (syntax-rules ()
    ((_ (flags ...))
     (condition-case (mpi-try-compile flags ...)
		     (t ()    #f)))))

(define mpi-dir (get-environment-variable "MPI_DIR"))

(define ld+cpp-options
  (or 
   (and mpi-dir (mpi-test ("#include <mpi.h>" 
			   (sprintf "-lmpi -L~S" (make-pathname mpi-dir "lib") )
			   (sprintf "-I~S -L~S" 
				    (make-pathname mpi-dir "include") 
				    (make-pathname mpi-dir "lib") ))
			  ))
   (mpi-test ("#include <mpi.h>" "-lmpi" ""))
   (mpi-test ("#include <mpi.h>" "-lmpich" "-I/usr/include/mpich"))
   (mpi-test ("#include <mpi.h>" "-lmpi" "-I/usr/include/mpi"))
   (mpi-test ("#include <mpi.h>" "-lmpi" "-I/usr/lib/openmpi/include"))
   (error "unable to figure out location of MPI library; try setting environment variable MPI_DIR to the proper location")))

(define cmd (intersperse (append args (list (sprintf "-L \"~A\"" (car ld+cpp-options)) 
                                            (sprintf "\"~A\"" (cdr ld+cpp-options)))) " "))
(print (string-concatenate cmd))
(system (string-concatenate cmd))
