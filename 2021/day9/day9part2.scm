(use-modules
  (srfi srfi-1)
  (ice-9 textual-ports))


(define grid-w 100)
(define grid-h 100)


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


(define (basin-size grid x y)
  (if (or
        (< x 0)
        (>= x grid-w)
        (< y 0)
        (>= y grid-h)
        (= (array-ref grid x y) 9))
    0
    (begin
      (array-set! grid 9 x y)
      (+ 1
         (basin-size grid (+ x 1) y)
         (basin-size grid (- x 1) y)
         (basin-size grid x (+ y 1))
         (basin-size grid x (- y 1))))))


(define (largest3 l)
  (take (sort l >) 3))


(define (list-mul l)
  (reduce * #f l))


(define (part2)
  (let ((grid (call-with-input-file "input" read-input))
        (basin-sizes '()))
    (let loop-x ((x (- grid-w 1)))
      (let loop-y ((y (- grid-h 1)))
        (unless (= 9 (array-ref grid x y))
          (set! basin-sizes
            (cons (basin-size grid x y)
                  basin-sizes)))
        (when (> y 0)
          (loop-y (- y 1))))
      (when (> x 0)
        (loop-x (- x 1))))
    (list-mul (largest3 basin-sizes))))
