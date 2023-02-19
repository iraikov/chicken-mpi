
(import (chicken base) (chicken format) (chicken process) (chicken process-context) (chicken pathname) srfi-1 srfi-13 compile-file)
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
    ((_ (header flags ...))
     (condition-case (mpi-try-compile header flags ...)
		     (t ()    #f)))))

(define mpi-dir (get-environment-variable "MPI_DIR"))

(define ld+cpp-options
  (or 
   (and mpi-dir (let ((ldflags (sprintf "-lmpi -L~S" (make-pathname mpi-dir "lib") ))
                      (cppflags  (sprintf "-I~S -L~S" 
                                          (make-pathname mpi-dir "include") 
                                          (make-pathname mpi-dir "lib") )))
                  (mpi-test ("#include <mpi.h>" ldflags cppflags))))
   (mpi-test ("#include <mpi.h>" "-lmpi" ""))
   (mpi-test ("#include <mpi.h>" "-lmpich" "-I/usr/lib/x86_64-linux-gnu/mpich/include -L/usr/lib/x86_64-linux-gnu/mpich/lib"))
   (mpi-test ("#include <mpi.h>" "-lmpi" "-I/usr/lib/x86_64-linux-gnu/openmpi/include -L/usr/lib/x86_64-linux-gnu/openmpi/lib"))
   (mpi-test ("#include <mpi.h>" "-lmpich" "-I/usr/include/mpich"))
   (mpi-test ("#include <mpi.h>" "-lmpi" "-I/usr/include/mpi"))
   (mpi-test ("#include <mpi.h>" "-lmpi" "-I/usr/lib/openmpi/include"))
   (error (if mpi-dir
              (sprintf "unable to detect MPI library in ~S" mpi-dir)
              "unable to figure out location of MPI library; try setting environment variable MPI_DIR to the proper location"))
   )
  )

(define ld-options (car ld+cpp-options))
(define c-options  (cdr ld+cpp-options))

(define cmd (intersperse
             (append args (list (sprintf "-L \"~A\"" ld-options)
                                (sprintf "-C \"~A\"" c-options)))
             " "))

(print (string-concatenate cmd))
(system (string-concatenate cmd))
