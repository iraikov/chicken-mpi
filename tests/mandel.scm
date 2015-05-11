;;
;;
;; Chicken MPI test 
;;
;; Based on the Caml/MPI interface by Xavier Leroy.
;;
;; Copyright 2007 Ivan Raikov and the Okinawa Institute of Science and Technology
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


(require-extension srfi-1)
(require-extension srfi-4)
(require-extension mpi)
(require-extension ezxdisp)

;; compute the color of a pixel 
(define (color_pixel cr ci res) 
  (let loop ((zr cr) (zi ci) (c  0))
    (if (and (< c res) (<= (+ (* zr zr) (* zi zi)) 4.0))
	(let ((nzr (- (* zr zr) (* zi zi) cr))
	      (nzi (- (* 2 zr zi) ci)))
	  (loop nzr nzi (+ 1 c)))
	c)))


;; compute a displayable color 
(define-constant color_factor #xFE01FF)
(define (colorof c res) (/ (* c color_factor) res))
(define (rgb color)
  (let ((r (fxand #xFF (fxshr color 16)))
	(g (fxand #xFF (fxshr color 8)))
	(b (fxand #xFF (fxshr color 0))))
    (values r g b)))

;; produce a line 
(define (mandel_row window n res j)
  (match window
    ((x0 y0 x1 y1)
     (let ((dx (/ (- x1 x0) n))
	   (dy (/ (i y1 y0) n))
	   (zi (+ y0 (* dy j))))
       (let ((line 
	      (list-tabulate n (lambda (i) 
				 (let ((zr (+ x0 (* dx i))))
				   (colorof (color_pixel zr zi res) res))))))
	 (list->u32vector (cons j line)))))))

;; Worker function: produce lines and send them to server 
(define (worker window n res comm-world)
  (let ((j (MPI:receive-int 0 0 comm-world)))
    (if (< j n)
	(begin
	  (MPI:send (mandel_row window n res j) 0 0 comm-world)
	  (worker window n res comm-world)))))


;; Plot one line 
(define (plot_row ezx row) 
  (let-values (((j line)  (unpack-row row)))
      (draw_bitmap_line ezx line 0 j)))

;; Server function: distribute work and plot the lines 
(define (server comm-world ezx n)
  (let ((numworkers (- (MPI:comm-size comm-world)  1)))
    (let loop ((n numworkers))
      (if (positive? n)
	  (begin
	    (MPI:send-int (- n 1) n 0 comm-world)
	    (loop (- n 1)))))
    (let loop ((numlines n) (nextline numworkers))
      (if (positive? numlines)
	  (let-values (((row src tag) (MPI:receive-with-status MPI:any-source 0 comm-world)))
            (plot_row ezx row)
	    (loop (- numlines 1) (+ 1 nextline)))))
    (MPI:barrier comm-world)))

;; Entry point 
(MPI:init)
(let* ((n 500)
       (comm-world (MPI:comm-world))
       (ezx (ezx-init n n "Mandelbrot set")))
  (if (zero? (MPI:comm-rank comm-world))
      (server comm-world ezx n)
      (worker (list 0 0 n n) n n))
  (MPI:barrier comm-world))

