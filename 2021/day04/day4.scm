(use-modules (srfi srfi-1)
             (ice-9 textual-ports)
             (srfi srfi-9)
             (srfi srfi-26)
             (ice-9 receive))


(define (lines->board board)
  (map
    (lambda (line)
      (map
        (lambda (bits)
          (string->number
            (string-trim-both
              bits)))
        (list
          (substring line 0 2)
          (substring line 3 5)
          (substring line 6 8)
          (substring line 9 11)
          (substring line 12 14))))
    board))


(define (read-one-board port)
  (let ((blank-line (get-line port)))
    (if (eof-object? blank-line)
      #f
      (lines->board
        (reverse
          (let loop ((lines '())
                     (n 5))
            (let ((line (get-line port)))
              (if (or (eof-object? line)
                      (= n 1))
                (cons line lines)
                (loop (cons line lines) (- n 1))))))))))


(define (read-boards port so-far)
  (let ((board (read-one-board port)))
    (if board
      (read-boards port
                   (cons board so-far))
      so-far)))


(define (read-input filename)
  (call-with-input-file
    filename
    (lambda (port)
      (let ((called-numbers (map string->number
                                 (string-split (get-line port) #\,))))
        (let ((boards (read-boards port '())))
          (values called-numbers boards))))))


(define (mark-board called-number board)
  (for-each
    (lambda (line)
      (let ((n (memv called-number line)))
        (when n
          (set-car! n 'X))))
    board))


(define (line-wins l)
  (every
    (lambda (x)
      (eq? x 'X))
    l))


(define (transpose b)
  (apply map (lambda (. x) x) b))


(define (board-wins? b)
  (or (find line-wins b)
      (find line-wins (transpose b))))


(define (sum-unmarked-numbers board)
  (reduce + #f
    (filter number? (apply append board))))


(define (mark-number-on-all-boards number boards)
  (for-each (cut mark-board number <>) boards))


(define (part1)
  (receive
    (called-numbers boards)
    (read-input "input")
    (format #t "Numbers: ~a\n\nI have ~a boards\n"
            called-numbers
            (length boards))
    (let loop ((next-number called-numbers))
      (when (nil? next-number)
        (error "No more numbers!"))
      (let ((the-number (car next-number)))
        (mark-number-on-all-boards the-number boards)
        (let ((board-won (find board-wins? boards)))
          (if board-won
            (* the-number (sum-unmarked-numbers board-won))
            (loop (cdr next-number))))))))


(define (one-item? l)
  (and (pair? l)
       (nil? (cdr l))))


(define (board-in-list list-of-boards board)
  (member board list-of-boards))


(define (part2)
  (receive
    (called-numbers boards)
    (read-input "input")
    (format #t "Numbers: ~a\n\nI have ~a boards\n"
            called-numbers
            (length boards))
    (let loop ((next-number called-numbers)
               (remaining-boards boards))
      (when (nil? next-number)
        (error "No more numbers!"))
      (let ((the-number (car next-number)))
        (mark-number-on-all-boards the-number remaining-boards)
        (if (and (one-item? remaining-boards)
                 (board-wins? (car remaining-boards)))
          (* the-number (sum-unmarked-numbers (car remaining-boards)))
          (loop (cdr next-number)
                (remove board-wins? remaining-boards)))))))
