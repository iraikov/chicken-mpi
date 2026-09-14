(define blob? bytevector?)
(define make-blob make-bytevector)
(define blob-size bytevector-length)

(define (blob->string b)
  (let* ((n (bytevector-length b))
         (s (make-string n)))
    (let loop ((i 0))
      (if (= i n)
          s
          (begin
            (string-set! s i (integer->char (bytevector-u8-ref b i)))
            (loop (+ i 1)))))))

(define (string->blob s)
  (let* ((n (string-length s))
         (b (make-bytevector n)))
    (let loop ((i 0))
      (if (= i n)
          b
          (begin
            (bytevector-u8-set! b i (char->integer (string-ref s i)))
            (loop (+ i 1)))))))
