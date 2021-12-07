(use-modules (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 textual-ports))


(define (read-input filename)
  (call-with-input-file
    filename
    (lambda (port)
      (map string->number
           (string-split (get-line port) #\,)))))


(define (list-sum c)
  (reduce + #f c))


(define (calculate-fuel inp target-pos)
  (list-sum (map (lambda (initial-pos)
                   (abs (- initial-pos target-pos)))
                 inp)))


(define (part1)
  (let ((inp (read-input "input")))
    (let ((extent (reduce max #f inp)))
      (reduce min #f
              (map (cut calculate-fuel inp <>)
                   (iota (+ 1 extent)))))))


(define (sum-to-1 n)
  (/ (* n (+ n 1)) 2))


(define (calculate-fuel-part2 inp target-pos)
  (list-sum (map (lambda (initial-pos)
                   (sum-to-1 (abs (- initial-pos target-pos))))
                 inp)))


(define (part2)
  (let ((inp (read-input "input")))
    (let ((extent (reduce max #f inp)))
      (reduce min #f
              (map (cut calculate-fuel-part2 inp <>)
                   (iota (+ 1 extent)))))))

