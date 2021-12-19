(use-modules
  (srfi srfi-9)
  (srfi srfi-1)
  (ice-9 match))


(define-record-type <probe-state>
  (probe-state x-pos y-pos x-vel y-vel)
  probe-state?
  (x-pos   x-pos)
  (y-pos   y-pos)
  (x-vel   x-vel)
  (y-vel   y-vel))



(define target-x '(135 155))
(define target-y '(-102 -78))

;; Example input
;;(define target-x '(20 30))
;;(define target-y '(-10 -5))


(define (step probe)
  (match probe
         (($ <probe-state> x y xv yv)
          (probe-state
            (+ x xv)
            (+ y yv)
            (cond
              ((> xv 0) (- xv 1))
              ((< xv 0) (+ xv 1))
              ((= xv 0) xv))
            (- yv 1)))))


(define (hit-target? probe)
  (and
    (<= (first target-x) (x-pos probe) (second target-x))
    (<= (first target-y) (y-pos probe) (second target-y))))


(define (cannot-hit-target? probe)
  (and
    (< (y-pos probe) (first target-y))
    (< (y-vel probe) 0)))


(define (iterate-probe xv yv)
  (let loop ((probe (probe-state 0 0 xv yv))
             (max-y 0))
    (if (hit-target? probe)
      max-y
      (if (cannot-hit-target? probe)
        #f
        (loop (step probe)
              (if (> (y-pos probe) max-y)
                (y-pos probe)
                max-y))))))


(define (print-array-row arr y)
  (format #t "~3@a:: " y)
  (let loop-x ((x 0))
    (format #t "~2@a "
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


(define (array-max arr)
  (let ((maxv (- (inf))))
    (array-for-each
      (lambda (v)
        (when (and v (> v maxv))
          (set! maxv v)))
      arr)
    maxv))


(define (array-ntrue arr)
  (let ((n 0))
    (array-for-each
      (lambda (v)
        (when v
          (set! n (1+ n))))
      arr)
    n))


(define (run-test)
  (let ((heights (make-array #f 200 500)))
    (array-index-map! heights
                      (lambda (x y)
                        (iterate-probe x (- y 250))))
    (values
      (array-max heights)
      (array-ntrue heights))))

