(use-modules
  (srfi srfi-1)
  (ice-9 match)
  (ice-9 textual-ports))


(define raw-input (call-with-input-file "input" get-line))
;;(define raw-input "D2FE28")
;;(define raw-input "38006F45291200")
;;(define raw-input "8A004A801A8002F478")
;;(define raw-input "620080001611562C8802118E34")

(define (char->binary c)
  (match c
         (#\0 '(0 0 0 0))
         (#\1 '(0 0 0 1))
         (#\2 '(0 0 1 0))
         (#\3 '(0 0 1 1))
         (#\4 '(0 1 0 0))
         (#\5 '(0 1 0 1))
         (#\6 '(0 1 1 0))
         (#\7 '(0 1 1 1))
         (#\8 '(1 0 0 0))
         (#\9 '(1 0 0 1))
         (#\A '(1 0 1 0))
         (#\B '(1 0 1 1))
         (#\C '(1 1 0 0))
         (#\D '(1 1 0 1))
         (#\E '(1 1 1 0))
         (#\F '(1 1 1 1))))


(define (hexnums->binary input)
  (fold (lambda (digit prev)
          (append (char->binary digit) prev))
        '()
        (reverse input)))


(define (make-binary-reader hex)
  (cons 'bin (hexnums->binary (string->list hex))))


(define (get-bit! reader)
  (if (nil? (cdr reader))
    #f
    (let ((val (cadr reader)))
      (set-cdr! reader (cddr reader))
      val)))


(define (get-bits-rec! reader n so-far)
  (if (= n 0)
    (reverse so-far)
    (get-bits-rec! reader (1- n)
                   (cons (get-bit! reader) so-far))))


(define (get-bits! reader n)
  (get-bits-rec! reader n '()))


(define (more-packets? r)
  (any (lambda (a) (= a 1)) (cdr r)))


(define (make-binary-reader-from-bits bits)
  (cons 'bin bits))


(define (powers-of-2-rec n pwr so-far)
  (if (> n 0)
    (powers-of-2-rec
      (- n 1)
      (+ pwr 1)
      (cons (expt 2 pwr)
            so-far))
    so-far))


(define (powers-of-2 n)
  (powers-of-2-rec n 0 '()))


(define (bin->num r)
  (reduce + #f
          (map * r
               (powers-of-2 (length r)))))


(define (read-packets r)
  (let loop ((version-sum 0))
    (if (more-packets? r)
      (loop (+ version-sum (read-packet r)))
      version-sum)))


(define (read-packet r)
  (let* ((version (bin->num (get-bits! r 3)))
         (type (bin->num (get-bits! r 3))))

    (cond

      ;; Literal number
      ((eq? type 4)
       (let loop ((the-number '()))
         (let ((bits (get-bits! r 5)))
           (let ((the-new-number (append the-number (cdr bits))))
             (if (= (first bits) 1)
               (loop the-new-number)
               version)))))

      (else
        (let ((length-type (get-bit! r)))
          (cond

            ;; Composite packet, given length of sub-packets
            ((= length-type 0)
             (let ((total-length (bin->num (get-bits! r 15))))
               (let ((the-bits (get-bits! r total-length)))
                 (+ version
                    (read-packets
                      (make-binary-reader-from-bits the-bits))))))

            ;; Composite packet, given number of sub-packets
            ((= length-type 1)
             (let ((num-packets (bin->num (get-bits! r 11))))
               (let loop ((n-to-read num-packets)
                          (version-sum 0))
                 (if (> n-to-read 0)
                   (loop (1- n-to-read)
                         (+ (read-packet r) version-sum))
                   (+ version version-sum)))))))))))


(let ((input (make-binary-reader raw-input)))
  (read-packets input))
