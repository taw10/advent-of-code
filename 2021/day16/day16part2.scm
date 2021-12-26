(use-modules
  (srfi srfi-1)
  (ice-9 match)
  (ice-9 textual-ports))


(define raw-input (call-with-input-file "input" get-line))
;;(define raw-input "C200B40A82")
;;(define raw-input "04005AC33890")
;;(define raw-input "9C0141080250320F1802104A08")


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


(define (bin->num-rec r pwr so-far)
  (if (nil? r)
    so-far
    (bin->num-rec (cdr r)
                  (* pwr 2)
                  (+ so-far (* pwr (car r))))))


(define (bin->num r)
  (bin->num-rec (reverse r) 1 0))


(define (read-packets r)
  (let loop ((packets '()))
    (if (more-packets? r)
      (loop (append packets (list (read-packet r))))
      packets)))


(define (read-subpackets r)
  (let ((length-type (get-bit! r)))
    (cond

      ;; Given length of sub-packets
      ((= length-type 0)
       (let ((total-length (bin->num (get-bits! r 15))))
         (read-packets
           (make-binary-reader-from-bits (get-bits! r total-length)))))

      ;; Given number of sub-packets
      ((= length-type 1)
       (let ((num-packets (bin->num (get-bits! r 11))))
         (let loop ((n-to-read num-packets)
                    (packets '()))
           (if (> n-to-read 0)
             (loop (1- n-to-read)
                   (append packets (list (read-packet r))))
             packets)))))))


(define (apply-to-subpackets r proc)
  (apply proc (read-subpackets r)))


(define (apply-comp-to-subpackets r proc)
  (let ((sub-packets (read-subpackets r)))
    (if (proc (first sub-packets)
              (second sub-packets))
      1 0)))


(define (read-packet r)
  (let* ((version (bin->num (get-bits! r 3)))
         (type (bin->num (get-bits! r 3))))

    (cond

      ((eq? type 0) (apply-to-subpackets r +))
      ((eq? type 1) (apply-to-subpackets r *))
      ((eq? type 2) (apply-to-subpackets r min))
      ((eq? type 3) (apply-to-subpackets r max))

      ((eq? type 4)
       (let loop ((the-number '()))
         (let* ((bits (get-bits! r 5))
                (the-new-number (append the-number (cdr bits))))
           (if (= (first bits) 1)
             (loop the-new-number)
             (bin->num the-new-number)))))

      ((eq? type 5) (apply-comp-to-subpackets r >))
      ((eq? type 6) (apply-comp-to-subpackets r <))
      ((eq? type 7) (apply-comp-to-subpackets r =))

      (else
        (format #t "Unrecognised packet type ~a / ~a\n" type sub-packets)))))


(let ((input (make-binary-reader raw-input)))
  (read-packets input))
