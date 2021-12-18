(use-modules
  (srfi srfi-1)
  (srfi srfi-26)
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


(define (find-low-points grid)
  (let ((low-points '()))
    (let loop-x ((x (- grid-w 1)))
      (let loop-y ((y (- grid-h 1)))
        (when (low-point? grid x y)
          (set! low-points
            (cons (cons x y) low-points)))
        (when (> y 0)
          (loop-y (- y 1))))
      (when (> x 0)
        (loop-x (- x 1))))
    low-points))



(define (risk-level grid coord)
  (+ 1 (array-ref grid
                  (car coord)
                  (cdr coord))))


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


(define (run-test)
  (let ((grid (call-with-input-file "input" read-input))
        (sum-risk-levels 0))
    (let ((low-points (find-low-points grid)))
      (values

        ;; Part 1
        (list-sum
          (map
            (cut risk-level grid <>)
            low-points))

        ;; Part 2
        (list-mul
          (largest3
            (map
              (lambda (low-point)
                (basin-size grid
                            (car low-point)
                            (cdr low-point)))
              low-points)))))))
