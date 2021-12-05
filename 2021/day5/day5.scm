(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-26)
             (ice-9 textual-ports)
             (ice-9 receive))


(define-record-type <line>
  (make-line x1 y1 x2 y2)
  line?
  (x1 x1)
  (y1 y1)
  (x2 x2)
  (y2 y2))


(define (trim-last-element l)
  (reverse (cdr (reverse l))))


(define (read-file-as-list filename)
  (trim-last-element
    (string-split
      (call-with-input-file
        filename
        (lambda (port)
          (get-string-all port)))
      #\newline)))


(define (read-input filename)
  (map
    (lambda (str)
      (let ((m (string-match
                 "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)"
                 str)))
        (make-line (string->number (match:substring m 1))
                   (string->number (match:substring m 2))
                   (string->number (match:substring m 3))
                   (string->number (match:substring m 4)))))
    (read-file-as-list filename)))


(define (horiz-or-vert? l)
  (or
    (= (x1 l) (x2 l))
    (= (y1 l) (y2 l))))


(define (max-line-val lines f1 f2)
  (fold
    (lambda (line prev)
      (let ((this-max (max (f1 line) (f2 line))))
        (if (> this-max prev)
          this-max
          prev)))
    1
    lines))


(define (array-plusone! arr x y)
  (array-set! arr
              (+ 1 (array-ref arr x y))
              x y))


(define (sign n)
  (cond
    ((< n 0) -1)
    ((> n 0) +1)
    (else 0)))


(define (step-direction line)
  (values (sign (- (x2 line) (x1 line)))
          (sign (- (y2 line) (y1 line)))))


(define (mark-line grid line)
  (receive
    (x-dir y-dir)
    (step-direction line)
    (let loop ((x-point (x1 line))
               (y-point (y1 line)))
      (array-plusone! grid x-point y-point)
      (unless (and (= x-point (x2 line))
                   (= y-point (y2 line)))
        (loop (+ x-point x-dir)
              (+ y-point y-dir))))))


(define (num-points-over-1 grid)
  (fold (lambda (n prev)
          (if (> n 1)
            (+ prev 1)
            prev))
        0
        (apply append
               (array->list grid))))


(define (part1)
  (let ((lines (filter horiz-or-vert?
                       (read-input "input"))))
    (format #t "There are ~a lines\n" (length lines))
    (let ((x-max (+ 1 (max-line-val lines x1 x2)))
          (y-max (+ 1 (max-line-val lines y1 y2))))
      (let ((grid (make-array 0 x-max y-max)))
        (for-each
          (cut mark-line grid <>)
          lines)
        (num-points-over-1 grid)))))


(define (part2)
  (let ((lines (read-input "input")))
    (format #t "There are ~a lines\n" (length lines))
    (let ((x-max (+ 1 (max-line-val lines x1 x2)))
          (y-max (+ 1 (max-line-val lines y1 y2))))
      (let ((grid (make-array 0 x-max y-max)))
        (for-each
          (cut mark-line grid <>)
          lines)
        (num-points-over-1 grid)))))
