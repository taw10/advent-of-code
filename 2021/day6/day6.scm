(use-modules (srfi srfi-1)
             (ice-9 textual-ports))


(define (numbers->bins l)
  (let ((bins (list 0 0 0 0 0 0 0 0 0)))
    (for-each
      (lambda (n)
        (list-set! bins n
                   (+ 1 (list-ref bins n))))
      l)
    bins))


(define (read-input filename)
  (numbers->bins
    (call-with-input-file
      filename
      (lambda (port)
        (map string->number
             (string-split (get-line port) #\,))))))


(define (list-sum c)
  (reduce + #f c))


(define (run-test n-days)
  (let ((inp (read-input "input")))
    (let loop ((counts inp) (n 1))
      (let ((baby-fish (car counts)))
        (list-set! counts 7
                   (+ baby-fish
                      (list-ref counts 7)))
        (format #t "After ~a days: ~a (~a total)\n"
                n counts (list-sum counts))
        (if (< n n-days)
          (loop (append (cdr counts) (list baby-fish))
                (+ n 1))
          (list-sum counts))))))

