(use-modules
  (ice-9 format)
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


(define (make-first-index shape)
  (make-list (length shape) 0))


(define (inc-first l)
  (cons (1+ (first l))
        (cdr l)))


(define (inc-indices old-indices shape)
  (let ((new-indices
          (fold
            (lambda (n lim prev-c)
              (let ((carry (car prev-c))
                    (prev (cdr prev-c)))
                (let ((nval (+ carry n)))
                  (if (<= nval (second lim))
                    (cons 0 (cons nval prev))
                    (cons 1 (cons 0 prev))))))
            (cons 1 '())
            old-indices
            shape)))
    (if (> (car new-indices) 0)
      #f
      (reverse (cdr new-indices)))))


(define (array-refl array indices)
  (apply array-ref array indices))

(define (array-setl! array val indices)
  (apply array-set! array val indices))

(define (make-arrayl init shape)
  (apply make-array init shape))

(define (array-in-boundsl? array indices)
  (apply array-in-bounds? array indices))


(define (array-index-fold proc init . arrays)
  (let ((shape (array-shape (first arrays))))
    (let loop ((indices (make-first-index shape))
               (fold-val init))
      (let ((next-fold-val (apply proc indices fold-val
                                 (map (cut array-ref-list <> indices)
                                      arrays))))
        (let ((next-indices (inc-indices indices shape)))
          (if next-indices
            (loop next-indices next-fold-val)
            next-fold-val))))))


(define (min-unvisited distance-array visited-array)
  (cdr
    (array-index-fold
      (lambda (coords prev distance visited)
        (if visited
          prev
          (if (< distance (car prev))
            (cons distance coords)
            prev)))
      (cons (inf) #f)
      distance-array
      visited-array)))


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
    (let loop ((cur-point (min-unvisited dist visited)))
      (if cur-point
        (begin
          (array-setl! visited #t cur-point)
          (let ((cur-dist (array-refl dist cur-point)))
            (for-each
              (lambda (p)
                (let ((new-dist (+ cur-dist (array-refl grid p))))
                  (when (< new-dist (array-refl dist p))
                    (array-setl! dist new-dist p)
                    (array-setl! prev cur-point p))))
              (unvisited-neighbours cur-point visited)))
          (loop (min-unvisited dist visited)))
        (propagate prev start (list finish))))))


(define (list-sum c)
  (reduce + #f c))


(define (run-test)
  (let ((grid (call-with-input-file "input" read-input)))
    (list-sum
      (map
        (lambda (coords)
          (array-refl grid coords))
        (cdr (dijkstra grid '(0 0) '(99 99)))))))
