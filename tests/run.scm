;;
;;
;; Chicken MPI test 
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

(use posix files setup-api)

(define prefix (pathname-directory (program-name)))

(if (not (zero? (system (conc "mpirun -np 32 "  (find-program 'csi) " -s " 
			      (make-pathname prefix  "mpitest.scm")))))
    (exit 1))
