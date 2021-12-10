(use-modules
  (ice-9 textual-ports)
  (ice-9 receive)
  (srfi srfi-26)
  (srfi srfi-1))


(define (read-input-rec port signal-patterns outputs)
  (let ((l (get-line port)))
    (if (eof-object? l)
      (values signal-patterns outputs)
      (let ((new-signal-patterns
              (string-split (substring l 0 58) #\space))
            (new-outputs
              (string-split (substring l 61) #\space)))
        (read-input-rec port
                        (cons new-signal-patterns signal-patterns)
                        (cons new-outputs outputs))))))


(define (read-input port)
  (read-input-rec port '() '()))


(define (easy-pattern? p)
  (let ((l (string-length p)))
    (or
      (eq? l 2)
      (eq? l 3)
      (eq? l 4)
      (eq? l 7))))


(define (count-easy-patterns input)
  (length
    (filter easy-pattern? input)))


(define (part1)
  (receive
    (signal-patterns outputs)
    (call-with-input-file "input" read-input)
    (fold (lambda (outputs prev)
            (+ (count-easy-patterns outputs)
               prev))
          0
          outputs)))
