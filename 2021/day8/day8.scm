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


(define (make-segment-table signals)
  (let ((table (make-hash-table 7)))
    (for-each
      (lambda (c)
        (hashq-set! table c '(A B C D E F G)))
      (apply append (map string->list signals)))
    table))


(define (filter-segments table signal segments)
  (hashq-set! table
              signal
              (lset-intersection eq? segments
                                 (hashq-ref table signal))))


(define (filter-segments-by-len signal-patterns
                                segment-table
                                signal-length
                                segments-involved)
  (for-each
    (cut filter-segments segment-table <> segments-involved)
    (string->list
      (find
        (lambda (s)
          (eq? signal-length
               (string-length s)))
        signal-patterns))))


(define (print-hash-table ht)
  (hash-for-each (lambda (key value)
                   (display key)
                   (display " ---> ")
                   (display value)
                   (newline))
                 ht))


(define (decode-display signal-patterns digits)
  (let ((segment-table (make-segment-table signal-patterns)))
    (filter-segments-by-len signal-patterns segment-table 2 '(C F))
    (filter-segments-by-len signal-patterns segment-table 3 '(A C F))
    (filter-segments-by-len signal-patterns segment-table 4 '(B C D F))
    (filter-segments-by-len signal-patterns segment-table 7 '(A B C D F G))
    segment-table))


(define (part2)
  (receive
    (signal-patterns digits)
    (call-with-input-file "example-input" read-input)
    (map decode-display signal-patterns digits)))
