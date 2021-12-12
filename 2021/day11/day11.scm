(use-modules
  (ice-9 textual-ports))


(define grid-w 10)
(define grid-h 10)


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


(define (print-grid grid)
  (let ((w (car (array-dimensions grid)))
        (h (cadr (array-dimensions grid))))
    (let loop-x ((x (- w 1)))
      (let loop-y ((y (- h 1)))
        (format #t "~a " (array-ref grid (- h y 1) (- w x 1)))
        (when (> y 0)
          (loop-y (- y 1))))
      (newline)
      (when (> x 0)
        (loop-x (- x 1))))))


(define (sum2d w h grid proc)
  (let ((sum 0))
    (let loop-x ((x (- w 1)))
      (let loop-y ((y (- h 1)))
        (set! sum (+ sum (proc grid x y)))
        (when (> y 0)
          (loop-y (- y 1))))
      (when (> x 0)
        (loop-x (- x 1))))
    sum))


(define (array+1! grid x y)
  (array-set! grid
              (1+ (array-ref grid x y))
              x y)
  0)


(define (if-within proc grid x y)
  (when
    (and
      (< x grid-w)
      (>= x 0)
      (< y grid-h)
      (>= y 0))
    (proc grid x y)))


(define (flash-octopus! grid x y)
  (if (> (array-ref grid x y) 9)
    (begin
      (array-set! grid -1000 x y)
      (if-within array+1! grid (1- x) (1- y))
      (if-within array+1! grid (1- x) y)
      (if-within array+1! grid (1- x) (1+ y))
      (if-within array+1! grid x (1- y))
      (if-within array+1! grid x (1+ y))
      (if-within array+1! grid (1+ x) (1- y))
      (if-within array+1! grid (1+ x) y)
      (if-within array+1! grid (1+ x) (1+ y))
      1)
    0))


(define (reset-octopus! grid x y)
  (if (< (array-ref grid x y) 0)
    (begin
      (array-set! grid 0 x y)
      1)
    0))


(define (flash-all w h grid)
  (let all-flash ((n-flashes 0))
    (let ((new-flashes (sum2d w h grid flash-octopus!)))
      (if (> new-flashes 0)
        (all-flash (+ new-flashes n-flashes))
        n-flashes))))


(define (part1)
  (let ((grid (call-with-input-file "input" read-input)))
    (let loop ((i 1) (total-flashes 0))
      (sum2d grid-w grid-h grid array+1!)
      (let ((new-flashes (flash-all grid-w grid-h grid)))
        (sum2d grid-w grid-h grid reset-octopus!)
        (if (< i 100)
          (loop (+ i 1) (+ total-flashes new-flashes))
          (+ total-flashes new-flashes))))))


(define (part2)
  (let ((grid (call-with-input-file "input" read-input)))
    (let loop ((i 1) (total-flashes 0))
      (sum2d grid-w grid-h grid array+1!)
      (let ((new-flashes (flash-all grid-w grid-h grid)))
        (if (= 100 (sum2d grid-w grid-h grid reset-octopus!))
          i
          (loop (+ i 1) (+ total-flashes new-flashes)))))))
