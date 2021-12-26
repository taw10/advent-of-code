(use-modules
  (srfi srfi-1)
  (srfi srfi-26)
  (srfi srfi-171)
  (ice-9 match)
  (ice-9 regex)
  (ice-9 textual-ports))


(define (string->cuboid str)
  (let ((m (string-match
             "([onf]+) x=([0-9,\\-]+)\\.\\.([0-9,\\-]+)\\,y=([0-9,\\-]+)\\.\\.([0-9,\\-]+)\\,z=([0-9,\\-]+)\\.\\.([0-9,\\-]+)"
             str)))
    (if m
      (list
        (if (string=? (match:substring m 1) "off")
          0 1)
        (string->number (match:substring m 2))
        (string->number (match:substring m 3))
        (string->number (match:substring m 4))
        (string->number (match:substring m 5))
        (string->number (match:substring m 6))
        (string->number (match:substring m 7)))
      (format #t "Invalid line: '~a'\n" str))))


(define (read-input-rec port so-far)
  (let ((line (get-line port)))
    (if (eof-object? line)
      so-far
      (read-input-rec port
                  (cons
                    (string->cuboid line)
                    so-far)))))


(define (read-input port)
  (reverse (read-input-rec port '())))


(define (get-x cuboid)
  (match cuboid
         ((state x1 x2 y1 y2 z1 z2)
          (list x1 (1+ x2)))))


(define (get-y cuboid)
  (match cuboid
         ((state x1 x2 y1 y2 z1 z2)
          (list y1 (1+ y2)))))


(define (get-z cuboid)
  (match cuboid
         ((state x1 x2 y1 y2 z1 z2)
          (list z1 (1+ z2)))))


(define (flatmap proc seq)
  (apply append (map proc seq)))


(define (unique-coords get-proc cuboids)
  (list-transduce
    (tdelete-neighbor-duplicates)
    rcons
    (sort
      (flatmap get-proc cuboids)
      <)))


(define (range-inside? inside outside)
  (match-let
    (((in1 in2) inside)
     ((out1 out2) outside))
    (<= out1 in1 in2 (1+ out2))))

(define (find-slots-rec ranges the-range cur-idx so-far)
  (if (nil? ranges)
    so-far
    (find-slots-rec (cdr ranges)
                    the-range
                    (1+ cur-idx)
                    (if (range-inside? (first ranges) the-range)
                      (cons cur-idx so-far)
                      so-far))))

(define (find-slots ranges the-range)
  (find-slots-rec ranges the-range 0 '()))


(define (array-set-multi! arr val indices)
  (let loop1 ((x1 (first indices)))
    (let loop2 ((x2 (second indices)))
      (let loop3 ((x3 (third indices)))
        (array-set! arr val
                    (car x1) (car x2) (car x3))
        (unless (nil? (cdr x3))
          (loop3 (cdr x3))))
      (unless (nil? (cdr x2))
        (loop2 (cdr x2))))
    (unless (nil? (cdr x1))
      (loop1 (cdr x1)))))

(define (compressed-array-set! arr val . ranges)
  (array-set-multi! (car arr)
                    val
                    (map find-slots (cdr arr) ranges)))


;; Converts a sorted list of coordinates to a
;; list of ranges [min,max)
(define (coords-to-ranges-rec coords so-far)
  (if (nil? (cdr coords))
    (reverse so-far)
    (coords-to-ranges-rec
      (cdr coords)
      (cons (list (first coords)
                  (second coords))
            so-far))))


(define (coords-to-ranges coords)
  (coords-to-ranges-rec coords '()))


(define (make-compressed-array . coords)
  (let ((ranges (map coords-to-ranges coords)))
    (let ((dims (map length ranges)))
      (cons
        (apply make-typed-array 'u8 0 (map (lambda (n)
                                             (list 0 (1- n)))
                                           dims))
        ranges))))

(define (num-cubes ranges indices)
  (apply *
         (map
           (lambda (range)
             (match range
                    ((x1 x2)
                     (- x2 x1))))
           (map vector-ref ranges indices))))

(define (compressed-array-fold arr proc init)
  (let ((fval init)
        (ranges (map list->vector (cdr arr))))
    (array-index-map!
      (first arr)
      (lambda (. indices)
        (let ((val (apply array-ref (first arr) indices)))
          (set! fval (proc val (num-cubes ranges indices) fval))
          val)))
    fval))


(define (run-test)
  (let ((input (call-with-input-file "part2-example-input" read-input)))
    (let ((cubes (make-compressed-array (unique-coords get-x input)
                                        (unique-coords get-y input)
                                        (unique-coords get-z input))))
      (format #t "Applying regions...\n")
      (for-each (lambda (cuboid)
                  (match-let
                    (((state x1 x2 y1 y2 z1 z2) cuboid))
                    (compressed-array-set! cubes state
                                           (list x1 x2)
                                           (list y1 y2)
                                           (list z1 z2))))
                input)
      (format #t "Summing cubes...\n")
      (compressed-array-fold cubes
                             (lambda (this-val num-cubes prev)
                               (+ prev (* this-val num-cubes)))
                             0))))


(format #t "Answer: ~a\n" (run-test))
