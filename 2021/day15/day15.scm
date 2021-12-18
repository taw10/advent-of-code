(use-modules
  (ice-9 format)
  (srfi srfi-1)
  (ice-9 match)
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


(define (print-array-row arr y)
  (format #t "~3@a:: " y)
  (let loop-x ((x 0))
    (format #t "~a "
            (array-ref arr x y))
    (if (= x (cadar (array-shape arr)))
      (newline)
      (loop-x (1+ x)))))


(define (print-array arr)
  (let ((shape (array-shape arr)))
    (let loop-y ((y 0))
      (print-array-row arr y)
      (unless (= y (second (second shape)))
        (loop-y (1+ y))))))


(define (array-refl array indices)
  (apply array-ref array indices))

(define (array-setl! array val indices)
  (apply array-set! array val indices))

(define (make-arrayl init shape)
  (apply make-array init shape))

(define (array-in-boundsl? array indices)
  (apply array-in-bounds? array indices))


(define (min-in-queue distance-array queue)
  (car
    (fold
      (lambda (coords prev)
        (let ((this-val (array-refl distance-array coords)))
          (if (< this-val (cdr prev))
            (cons coords this-val)
            prev)))
      (cons #f (inf))
      queue)))


(define (unvisited-neighbours point visited-arr)
  (filter (lambda (coords)
            (and
              (array-in-boundsl? visited-arr coords)
              (not (array-refl visited-arr coords))))
          (match point
                 ((x y) (list
                          (list (1+ x) y)
                          (list (1- x) y)
                          (list x (1- y))
                          (list x (1+ y)))))))


(define (propagate prev-array start so-far)
  (if (equal? (first so-far) start)
    so-far
    (propagate prev-array
               start
               (cons (array-refl prev-array (first so-far))
                     so-far))))


(define (dijkstra grid start finish)
  (let ((visited (make-arrayl #f (array-shape grid)))
        (dist (make-arrayl (inf) (array-shape grid)))
        (prev (make-arrayl #f (array-shape grid))))
    (array-setl! dist 0 start)
    (let loop ((queue (list start)))
      (let ((cur-point (min-in-queue dist queue)))
        (if cur-point
          (begin
            (array-setl! visited #t cur-point)
            (let ((cur-dist (array-refl dist cur-point))
                  (neigh (unvisited-neighbours cur-point visited)))
              (for-each
                (lambda (p)
                  (let ((new-dist (+ cur-dist (array-refl grid p))))
                    (when (< new-dist (array-refl dist p))
                      (array-setl! dist new-dist p)
                      (array-setl! prev cur-point p))))
                neigh)
              (loop (append (delete cur-point queue equal?) neigh))))
          (propagate prev start (list finish)))))))


(define (list-sum c)
  (reduce + #f c))


(define (lowest-total-risk grid start finish)
  (list-sum
    (map
      (lambda (coords)
        (array-refl grid coords))
      (cdr (dijkstra grid start finish)))))


(define (wrap-10-to-1 n)
  (if (> n 9)
    (- n 9)
    n))

(define (wrap-around n small-grid)
  (let ((grid (make-arrayl 0 (map (lambda (r)
                                    (* n (1+ (second r))))
                                  (array-shape small-grid)))))
    (array-index-map!
      grid
      (lambda (x y)
        (wrap-10-to-1
          (+ (array-ref small-grid
                        (remainder x grid-w)
                        (remainder y grid-h))
             (quotient x grid-w)
             (quotient y grid-h)))))
    grid))


(define (part1)
  (let ((grid (call-with-input-file "input" read-input)))
    (lowest-total-risk grid '(0 0) '(99 99))))


(define (part2)
  (let ((grid (call-with-input-file "input" read-input)))
    (lowest-total-risk (wrap-around 5 grid)
                       '(0 0)
                       '(499 499))))
