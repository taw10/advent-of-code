(use-modules (srfi srfi-1)
             (ice-9 textual-ports))


(define (read-file filename)
  (call-with-input-file
    filename
    (lambda (port)
      (get-string-all port))))


(define (read-numbers-from-file filename)
  (filter number?
          (map string->number
               (string-split (read-file filename) #\newline))))


(define input (read-numbers-from-file "input"))


(define (number-of-increased-measurements input)
  (cdr (fold
         (lambda (depth prev)
           (let ((last-depth (car prev))
                 (n-deeper (cdr prev)))
             (cons
               depth
               (if (> depth last-depth)
                 (+ n-deeper 1)
                 n-deeper))))
         (cons (car input) 0)
         (cdr input))))

(define (make-windows input)
  (zip input (cdr input) (cddr input)))


(define (list-sum l)
  (fold + 0 l))

(define (number-of-increased-windows input)
  (number-of-increased-measurements
    (map list-sum (make-windows input))))


(format #t "Part 1: ~a\nPart 2: ~a\n"
        (number-of-increased-measurements input)
        (number-of-increased-windows input))
