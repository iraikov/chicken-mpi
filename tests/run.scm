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

(import scheme (chicken base) (chicken pathname) (chicken string)
        (chicken process) (chicken process-context))

(define prefix (pathname-directory (program-name)))

(define mpirun (or (get-environment-variable "MPIRUN") "mpirun"))
(define csi (or (get-environment-variable "CHICKEN_CSI") "csi"))

(if (not (and (zero? (system (conc mpirun " -np 8 "  csi " -s " 
                                   (make-pathname prefix  "datatest.scm"))))
              (zero? (system (conc mpirun " -np 8 "  csi " -s " 
                                   (make-pathname prefix  "mpitest.scm"))))
              ))
              
    (exit 1))
