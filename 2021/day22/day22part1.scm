(use-modules
  (srfi srfi-1)
  (srfi srfi-26)
  (ice-9 match)
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


(define (make-first-index shape)
  (map car shape))


(define (inc-indices old-indices shape)
  (let ((new-indices
          (fold
            (lambda (n lim prev-c)
              (let ((carry (car prev-c))
                    (prev (cdr prev-c)))
                (let ((nval (+ carry n)))
                  (if (<= nval (second lim))
                    (cons 0 (cons nval prev))
                    (cons 1 (cons (first lim) prev))))))
            (cons 1 '())
            old-indices
            shape)))
    (if (> (car new-indices) 0)
      #f
      (reverse (cdr new-indices)))))


(define (multi-dim-iterate proc init shape)
  (let loop ((indices (make-first-index shape))
             (fold-val init))
    (let ((next-fold-val (proc indices fold-val)))
      (let ((next-indices (inc-indices indices shape)))
        (if next-indices
          (loop next-indices next-fold-val)
          next-fold-val)))))


(define (within-cuboid indices cuboid)
  (match indices
         ((x y z)
          (match cuboid
                 ((state x1 x2 y1 y2 z1 z2)
                  (and
                    (<= x1 x x2)
                    (<= y1 y y2)
                    (<= z1 z z2)))))))


(define (final-cube-state cuboids indices)
  (fold
    (lambda (cuboid prev-state)
      (if (within-cuboid indices cuboid)
        (first cuboid)
        prev-state))
    0
    cuboids))


(define (check-cube cuboids indices prev)
  (+ prev
     (final-cube-state cuboids indices)))


(define (run-test)
  (let ((input (call-with-input-file "input" read-input)))
    (multi-dim-iterate (cut check-cube input <> <>)
                       0
                       '((-50 50) (-50 50) (-50 50)))))
