(use-modules
  (ice-9 textual-ports))


(define grid-w 10)
(define grid-h 5)


(define (set-grid-row! grid y line)
  (let ((len (string-length line)))
    (let loop ((x 0))
      (array-set! grid (string->number
                         (substring line x (+ x 1)))
                  x y)
      (unless (= x (- len 1))
        (loop (+ x 1))))))


(define (read-input port)
  (let ((grid (make-array 0 grid-w grid-h)))
    (let loop ((y 0))
      (let ((line (get-line port)))
        (if (eof-object? line)
          grid
          (begin
            (set-grid-row! grid y line)
            (loop (+ y 1))))))))


(define (list-sum c)
  (reduce + #f c))


(define (low-point? grid x y)
  (let ((p (array-ref grid x y)))
    (and
      (or (= x 0)
          (< p (array-ref grid (- x 1) y)))
      (or (= x (- grid-w 1))
          (< p (array-ref grid (+ x 1) y)))
      (or (= y 0)
          (< p (array-ref grid x (- y 1))))
      (or (= y (- grid-h 1))
          (< p (array-ref grid x (+ y 1)))))))


(define (part1)
  (let ((grid (call-with-input-file "example-input" read-input))
        (sum-risk-levels 0))
    (let loop-x ((x (- grid-w 1)))
      (let loop-y ((y (- grid-h 1)))
        (when (low-point? grid x y)
          (format #t "Low point ~a,~a = ~a\n" x y (array-ref grid x y))
          (set! sum-risk-levels
            (+ sum-risk-levels
               1
               (array-ref grid x y))))
        (when (> y 0)
          (loop-y (- y 1))))
      (when (> x 0)
        (loop-x (- x 1))))
    sum-risk-levels))
